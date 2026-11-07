;;; orpheus.el --- A music player for Emacs with pluggable backends -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Version: 0.2.1
;;; Author: Josep Bigorra <jjbigorra@gmail.com>
;;; Maintainer: Josep Bigorra <jjbigorra@gmail.com>
;;; URL: https://github.com/jjba23/orpheus.el
;;; Keywords: multimedia music mpd mpv
;;; Package: orpheus
;;; Package-Requires: ((emacs "29.1"))

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; orpheus plays music without leaving Emacs.  It is, at heart, an MPD client
;;; in the shape of rmpc: the same queue, the same tabs for directories,
;;; artists, album artists, albums, playlists and search, the same transport
;;; keys, cover art and a spectrum visualizer.  Unlike rmpc it does not assume
;;; MPD is the player: a backend is a small object that implements a handful
;;; of generics, and two of them ship here.
;;;
;;; - `mpd' talks to a Music Player Daemon over TCP or a unix socket.  The
;;; daemon owns the audio output, so PipeWire, PulseAudio, ALSA and CoreAudio
;;; all work without this package knowing about them.
;;; - `local' plays files and URLs with mpv over its JSON IPC socket.  mpv uses
;;; PipeWire on Linux, CoreAudio on macOS and WASAPI on Windows, and hands
;;; YouTube links to yt-dlp.
;;;
;;; Everything is asynchronous: replies arrive in process filters, and the
;;; status is read on a timer, so a slow server or a starting player never
;;; blocks the editor.  The interface is text, so it works in a terminal and
;;; over SSH; in a graphical frame it adds cover art and an SVG visualizer and
;;; loses nothing when those are unavailable.
;;;
;;; Start with:
;;;
;;; (require 'orpheus)
;;; (setq orpheus-backend 'mpd)     ; or 'local
;;; (setq orpheus-mpd-host "localhost")
;;; (orpheus)
;;;
;;; Bind `orpheus' to a key, and `orpheus-mode-line-mode' if you want the
;;; current track in the mode line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)
(require 'orpheus-mpd)
(require 'orpheus-local)
(require 'orpheus-ui)
(require 'orpheus-vis)

(declare-function notifications-notify "notifications" (&rest args))
(declare-function dired-file-name-at-point "dired" ())

;;; Settings

(defconst orpheus-version "0.2.1"
  "Version of orpheus.")

(defcustom orpheus-backend 'mpd
  "Backend to play with: `mpd' or `local', or any registered one.
Register another with `orpheus-register-backend'."
  :type '(choice (const :tag "Music Player Daemon" mpd)
                 (const :tag "Local files and URLs through mpv" local)
                 (symbol :tag "Another registered backend"))
  :group 'orpheus)

(defcustom orpheus-refresh-interval 1.0
  "Seconds between two reads of the player's status."
  :type 'number :group 'orpheus)

(defcustom orpheus-volume-step 2
  "Volume change of `orpheus-volume-up' and `orpheus-volume-down'."
  :type 'integer :group 'orpheus)

(defcustom orpheus-seek-step 5
  "Seconds `orpheus-seek-forward' and `orpheus-seek-backward' move."
  :type 'integer :group 'orpheus)

(defcustom orpheus-notifications t
  "Whether to show a notification when the track changes."
  :type 'boolean :group 'orpheus)

(defvar orpheus--timer nil
  "Timer that reads the player's status.")

(defvar orpheus--songid nil
  "Identifier of the track the session has already reported.")

(defvar orpheus--failures 0
  "How many status reads in a row have failed.")

(defvar orpheus--retry-at 0
  "Time before which no new connection is attempted.")

;;; The session

(defun orpheus--ensure-backend ()
  "Make `orpheus-backend-current' the backend the user chose."
  (unless (and orpheus-backend-current
               (eq (orpheus-backend-name orpheus-backend-current)
                   orpheus-backend))
    (when orpheus-backend-current
      (ignore-errors (orpheus-backend-disconnect orpheus-backend-current)))
    (setq orpheus-backend-current (orpheus-make-backend orpheus-backend)
      orpheus--songid nil
      orpheus-status-current nil
      orpheus-track-current nil)))

(defun orpheus-start ()
  "Start reading the player's status on a timer."
  (interactive)
  (unless (timerp orpheus--timer)
    (setq orpheus--timer
      (run-with-timer 0 orpheus-refresh-interval #'orpheus--tick))))

(defun orpheus-quit ()
  "Stop reading the status and close the player."
  (interactive)
  (when (timerp orpheus--timer)
    (cancel-timer orpheus--timer))
  (setq orpheus--timer nil)
  (let ((backend orpheus-backend-current))
    (when backend
      (ignore-errors (orpheus-backend-disconnect backend)))))

(defun orpheus-connect (&optional callback)
  "Connect the chosen backend, then call CALLBACK."
  (interactive)
  (orpheus--ensure-backend)
  (orpheus-backend-connect
   orpheus-backend-current
   (lambda (backend)
     (if backend
       (progn (orpheus--tick)
         (when callback (funcall callback backend)))
       (message "orpheus: cannot connect to %s" orpheus-backend)))))

(defun orpheus--offline ()
  "Slow down while the backend is not answering."
  (setq orpheus--failures (1+ orpheus--failures))
  (setq orpheus--retry-at
    (+ (float-time) (min 30 (expt 2 (min orpheus--failures 5)))))
  (when (= orpheus--failures 1)
    ;; A backend that still holds a connection answered and refused, which
    ;; asks for a different fix than a daemon that is simply not there.
    (message "orpheus: %s %s" orpheus-backend
             (if (and orpheus-backend-current
                      (ignore-errors
                        (orpheus-backend-connected-p orpheus-backend-current)))
               "is there but refuses the request; check its password and permissions"
               "is not answering; retrying in the background")))
  (force-mode-line-update t))

(defun orpheus--tick ()
  "Read the backend's status and pass it on."
  (let ((backend orpheus-backend-current))
    (when (and backend (>= (float-time) orpheus--retry-at))
      (orpheus-backend-status
       backend
       (lambda (status)
         (if (null status)
           (orpheus--offline)
           (setq orpheus--failures 0
             orpheus--retry-at 0
             orpheus-status-current status)
           (orpheus--maybe-song-changed status)
           (force-mode-line-update t)
           (orpheus-ui-refresh)))))))

(defun orpheus--maybe-song-changed (status)
  "Ask for the track when STATUS reports a different song."
  (let ((songid (orpheus-status-songid status)))
    (when (or (null orpheus-track-current)
              (not (equal songid orpheus--songid)))
      (orpheus-backend-track
       orpheus-backend-current
       (lambda (track)
         (unless (equal songid orpheus--songid)
           (setq orpheus--songid songid
             orpheus-track-current track)
           (orpheus--song-changed track)))))))

(defun orpheus--song-changed (track)
  "Announce TRACK to whoever wants to know."
  (force-mode-line-update t)
  (when track
    (orpheus-ui--update-art track)
    (when orpheus-notifications
      (orpheus--notify track))))

(defun orpheus--notify (track)
  "Show a notification for TRACK, with its cover when there is one."
  (when (fboundp 'notifications-notify)
    (let ((art (plist-get orpheus-ui--art :file)))
      (ignore-errors
        (apply #'notifications-notify
               (append (list :title "orpheus"
                             :body (orpheus-track-label track)
                             :timeout 4000)
                       (when art (list :app-icon art))))))))

;;; Transport

(defun orpheus--command (action &optional value)
  "Send ACTION, with VALUE, to the backend in use."
  (let ((backend orpheus-backend-current))
    (cond
      ((null backend) (message "orpheus: no backend yet; run M-x orpheus"))
      ((not (orpheus-backend-connected-p backend))
       (message "orpheus: %s is not connected" orpheus-backend))
      (t (orpheus-backend-command backend action value)
        (orpheus--tick)))))

(defun orpheus--volume ()
  "Return the volume the backend last reported."
  (or (and orpheus-status-current
           (orpheus-status-volume orpheus-status-current))
      0))

(defun orpheus-play-pause ()
  "Play or pause, whichever comes next."
  (interactive)
  (orpheus--command 'toggle))

(defun orpheus-play ()
  "Start playing."
  (interactive)
  (orpheus--command 'play))

(defun orpheus-pause ()
  "Pause, keeping the queue."
  (interactive)
  (orpheus--command 'pause))

(defun orpheus-stop ()
  "Stop playing."
  (interactive)
  (orpheus--command 'stop))

(defun orpheus-next ()
  "Play the next track."
  (interactive)
  (orpheus--command 'next))

(defun orpheus-previous ()
  "Play the previous track."
  (interactive)
  (orpheus--command 'previous))

(defun orpheus-seek-forward ()
  "Move `orpheus-seek-step' seconds forward."
  (interactive)
  (orpheus--command 'seek orpheus-seek-step))

(defun orpheus-seek-backward ()
  "Move `orpheus-seek-step' seconds back."
  (interactive)
  (orpheus--command 'seek (- orpheus-seek-step)))

(defun orpheus-volume-up ()
  "Turn the volume up by `orpheus-volume-step'."
  (interactive)
  (orpheus--command 'volume (min 100 (+ (orpheus--volume)
                                        orpheus-volume-step))))

(defun orpheus-volume-down ()
  "Turn the volume down by `orpheus-volume-step'."
  (interactive)
  (orpheus--command 'volume (max 0 (- (orpheus--volume)
                                      orpheus-volume-step))))

(defun orpheus--toggle (field)
  "Flip the status FIELD on the backend."
  (orpheus--command
   (pcase field
          ('repeat 'repeat)
          ('random 'random)
          ('single 'single)
          ('consume 'consume))
   (not (and orpheus-status-current
             (funcall (intern (format "orpheus-status-%s" field))
                      orpheus-status-current)))))

(defun orpheus-toggle-repeat ()
  "Turn repeating on or off."
  (interactive)
  (orpheus--toggle 'repeat))

(defun orpheus-toggle-random ()
  "Turn shuffling on or off."
  (interactive)
  (orpheus--toggle 'random))

(defun orpheus-toggle-single ()
  "Turn single-track mode on or off."
  (interactive)
  (orpheus--toggle 'single))

(defun orpheus-toggle-consume ()
  "Turn consuming played tracks on or off."
  (interactive)
  (orpheus--toggle 'consume))

(defun orpheus-update-database ()
  "Ask the backend to look for new music."
  (interactive)
  (orpheus--command 'update))

(defun orpheus-rescan-database ()
  "Ask the backend to forget the library and read it again."
  (interactive)
  (orpheus--command 'rescan))

(defun orpheus-refresh ()
  "Read the status and the current tab again."
  (interactive)
  (orpheus--tick)
  (orpheus-ui-refresh))

;;; Playing files and URLs

(defun orpheus--playable-path ()
  "Return the file at point, or the buffer's file, or ask for one."
  (or (and (derived-mode-p 'dired-mode)
           (fboundp 'dired-file-name-at-point)
           (dired-file-name-at-point))
      (buffer-file-name)
      (expand-file-name (read-file-name "Play: "))))

(defun orpheus-play-file (&optional file)
  "Play FILE, the file at point, or the one asked for.
This needs a backend that plays paths, which is `local'."
  (interactive (list (orpheus--playable-path)))
  (orpheus--ensure-backend)
  (if (memq 'paths (orpheus-backend-capabilities orpheus-backend-current))
    (progn
      (orpheus-backend-play-uri orpheus-backend-current file)
      (setq orpheus--retry-at 0)
      (orpheus-start)
      (message "orpheus: %s" (file-name-nondirectory file)))
    (user-error "orpheus: %s plays its own library, not paths; play %s from the files tab or use the local backend"
                orpheus-backend (file-name-nondirectory file))))

(defun orpheus-add-file (&optional file)
  "Queue FILE, the file at point, or the one asked for."
  (interactive (list (orpheus--playable-path)))
  (orpheus--ensure-backend)
  (if (memq 'paths (orpheus-backend-capabilities orpheus-backend-current))
    (progn
      (orpheus-backend-add orpheus-backend-current file)
      (orpheus-start)
      (message "orpheus: queued %s" (file-name-nondirectory file)))
    (user-error "orpheus: %s plays its own library, not paths"
                orpheus-backend)))

(defun orpheus-play-url (url)
  "Play URL, which mpv hands to yt-dlp when it is a video page."
  (interactive
    (list (read-string "Play URL: "
                       (if (and (car kill-ring)
                                (string-match-p "\\`https?://" (car kill-ring)))
                         (car kill-ring)
                         ""))))
  (orpheus--ensure-backend)
  (if (memq 'paths (orpheus-backend-capabilities orpheus-backend-current))
    (progn
      (orpheus-backend-play-uri orpheus-backend-current url)
      (orpheus-start)
      (message "orpheus: %s" url))
    (user-error "orpheus: %s cannot play URLs" orpheus-backend)))

;;; The mode line

(defun orpheus-mode-line-string ()
  "Return the current track for the mode line, or nil."
  (when (and orpheus-track-current
             (not (string-empty-p (orpheus-track-label orpheus-track-current))))
    (concat
     " "
     (propertize (orpheus-status-glyph orpheus-status-current)
                 'face 'orpheus-status-face)
     " "
     (propertize (orpheus-ui--elide (orpheus-track-label orpheus-track-current)
                                    40)
                 'face 'orpheus-title-face))))

(define-minor-mode orpheus-mode-line-mode
  "Show what orpheus is playing in the mode line."
  :global t
  :group 'orpheus
  (if orpheus-mode-line-mode
    (add-to-list 'global-mode-string '(:eval (orpheus-mode-line-string)) t)
    (setq global-mode-string
      (delete '(:eval (orpheus-mode-line-string)) global-mode-string))))

;;; Entry point

;;; ###autoload
(defun orpheus ()
  "Open the orpheus player."
  (interactive)
  (orpheus--ensure-backend)
  (orpheus-start)
  (orpheus-connect)
  (orpheus-ui-open))

(provide 'orpheus)

;;; orpheus.el ends here
