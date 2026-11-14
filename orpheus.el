;;; orpheus.el --- A music player for Emacs with pluggable backends -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Version: 1.1.0
;;; Author: Josep Bigorra <jjbigorra@gmail.com>
;;; Maintainer: Josep Bigorra <jjbigorra@gmail.com>
;;; URL: https://github.com/jjba23/orpheus.el
;;; Keywords: multimedia music mpd mpv
;;; Package: orpheus
;;; Package-Requires: ((emacs "29.1"))

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; orpheus plays music without leaving Emacs.  It is, at heart, an MPD client:
;;; a queue, tabs for directories, artists, album artists, albums, playlists
;;; and search, transport keys, cover art and a spectrum visualizer.  But it
;;; does not assume MPD is the player: a backend is a small object that
;;; implements a handful of generics, and three of them ship here.
;;;
;;; - `mpd' talks to a Music Player Daemon over TCP or a unix socket.  The
;;; daemon owns the audio output, so PipeWire, PulseAudio, ALSA and CoreAudio
;;; all work without this package knowing about them.
;;; - `local' plays files and URLs with mpv over its JSON IPC socket.  mpv uses
;;;   PipeWire on Linux, CoreAudio on macOS and WASAPI on Windows, and hands
;;;   YouTube links to yt-dlp.
;;; - `ssh' lists music on another machine and copies a track into a temporary
;;;   directory before playing it here with mpv.  There is no daemon on the far
;;;   side and no tunnel; the queue and transport are the local backend's.
;;; - `spotify' controls a Spotify account through its Web API.  Spotify plays
;;;   the music on a device of yours, so this backend browses the playlists,
;;;   the saved albums, the followed artists and the saved songs, and moves
;;;   that device's queue; it needs Premium for playback control and it never
;;;   streams audio into Emacs.
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
;;; (setq orpheus-backend 'mpd)     ; or 'local, or 'ssh
;;; (setq orpheus-mpd-host "localhost")
;;; (orpheus)
;;;
;;; Bind `orpheus' to a key, and `orpheus-mode-line-mode' if you want the
;;; current track in the mode line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)
(require 'orpheus-playlist)
(require 'orpheus-mpd)
(require 'orpheus-local)
(require 'orpheus-ssh)
(require 'orpheus-spotify)
(require 'orpheus-ui)
(require 'orpheus-mpris)
(require 'orpheus-vis)

(declare-function notifications-notify "notifications" (&rest args))
(declare-function dired-file-name-at-point "dired" ())
(declare-function orpheus-equalizer-restore "orpheus-equalizer" ())

;;; Settings

(defconst orpheus-version "1.1.0"
  "Version of orpheus.")

(defcustom orpheus-backend 'mpd
  "Backend to play with: `mpd', `local', `ssh' or `spotify', or any registered one.
Register another with `orpheus-register-backend'."
  :type '(choice (const :tag "Music Player Daemon" mpd)
                 (const :tag "Local files and URLs through mpv" local)
                 (const :tag "Files on another machine, copied over ssh" ssh)
                 (const :tag "Spotify, controlled through its Web API" spotify)
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

(defvar orpheus--status-pending nil
  "Whether a status read is in flight, so ticks do not pile up behind it.")

(defvar orpheus--status-at 0
  "When the status read in flight was sent, as a float time.")

(defvar orpheus--track-pending nil
  "Whether a read of the current track is in flight.")

(defvar orpheus--track-at 0
  "When the track read in flight was sent, as a float time.")

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
  (orpheus-mpris-start)
  (unless (timerp orpheus--timer)
    (setq orpheus--timer
      (run-with-timer 0 orpheus-refresh-interval #'orpheus--tick))))

(defun orpheus-quit ()
  "Stop reading the status and close the player."
  (interactive)
  (when (timerp orpheus--timer)
    (cancel-timer orpheus--timer))
  (setq orpheus--timer nil
        orpheus--status-pending nil
        orpheus--track-pending nil)
  (orpheus-mpris-stop)
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
       (progn (orpheus-equalizer-restore)
         (orpheus--tick)
         (when callback (funcall callback backend)))
       (message "orpheus: cannot connect to %s" orpheus-backend)))))

(defun orpheus-switch-backend (backend)
  "Switch to the registered BACKEND and reconnect the player.
The current backend is disconnected first, so its player and transfers stop.
An open player buffer is rebuilt for the new backend's tabs.  Interactively,
BACKEND is chosen from the registered backends with completion."
  (interactive
   (list (intern
          (completing-read
           "Backend: "
           (mapcar (lambda (entry) (symbol-name (car entry)))
                   orpheus-backend-registry)
           nil t nil nil (symbol-name orpheus-backend)))))
  (unless (orpheus-backend-exists-p backend)
    (user-error "Orpheus: no backend called %s" backend))
  (if (eq backend orpheus-backend)
      (message "orpheus: already using the %s backend" backend)
    (orpheus-quit)
    (setq orpheus-backend backend
          orpheus--failures 0
          orpheus--retry-at 0)
    (orpheus--ensure-backend)
    (orpheus-start)
    (orpheus-connect
     (lambda (connected)
       (when (get-buffer orpheus-ui-buffer-name)
         (with-current-buffer orpheus-ui-buffer-name
           (when (derived-mode-p 'orpheus-ui-mode)
             (orpheus-ui--setup-tabs)
             (orpheus-ui--render)
             (when connected (orpheus-ui-refresh t)))))
       (if connected
           (message "orpheus: switched to the %s backend" backend)
         (message "orpheus: switched to %s, but it did not connect" backend)))))
  backend)

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
  "Read the backend's status and pass it on.
A read already in flight is not sent twice: a daemon slower than the timer
would otherwise be asked again every second, and the answers would queue up
until the editor felt them."
  (let ((backend orpheus-backend-current)
        (now (float-time)))
    (when (and backend
               (>= now orpheus--retry-at)
               (or (not orpheus--status-pending)
                   (> (- now orpheus--status-at) 5)))
      (setq orpheus--status-pending t
            orpheus--status-at now)
      (orpheus-backend-status
       backend
       (lambda (status)
         (setq orpheus--status-pending nil)
         (if (null status)
           (orpheus--offline)
           (setq orpheus--failures 0
             orpheus--retry-at 0
             orpheus-status-current status)
           (orpheus--maybe-song-changed status)
           (force-mode-line-update t)
           (orpheus-mpris--sync)
           (orpheus-ui-tick)))))))

(defun orpheus--maybe-song-changed (status)
  "Ask for the track when STATUS reports a different song."
  (let ((songid (orpheus-status-songid status))
        (now (float-time)))
    (when (and (or (null orpheus-track-current)
                   (not (equal songid orpheus--songid)))
               (or (not orpheus--track-pending)
                   (> (- now orpheus--track-at) 5)))
      (setq orpheus--track-pending t
            orpheus--track-at now)
      (orpheus-backend-track
       orpheus-backend-current
       (lambda (track)
         (setq orpheus--track-pending nil)
         (unless (equal songid orpheus--songid)
           (setq orpheus--songid songid
             orpheus-track-current track)
           (orpheus--song-changed track)))))))

(defun orpheus--song-changed (track)
  "Announce TRACK to whoever wants to know."
  (force-mode-line-update t)
  ;; What is playing decides the highlighted row, and with `consume' the
  ;; queue itself has moved on, so the tab is worth reading again.
  (orpheus-ui-invalidate)
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
        (orpheus-ui-invalidate)
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

(defun orpheus-cycle-repeat ()
  "Cycle repeating: off, then the queue, then the current track.
One key for the three states, with the mode flags in the status line
saying which one is in effect."
  (interactive)
  (let* ((status orpheus-status-current)
         (repeat (and status (orpheus-status-repeat status)))
         (single (and status (orpheus-status-single status)))
         (backend orpheus-backend-current))
    (cond
     ((null backend) (message "orpheus: no backend yet; run M-x orpheus"))
     ((not (orpheus-backend-connected-p backend))
      (message "orpheus: %s is not connected" orpheus-backend))
     ((and repeat single)
      (orpheus-backend-command backend 'single nil)
      (orpheus-backend-command backend 'repeat nil)
      (message "orpheus: repeat off"))
     (repeat
      (orpheus-backend-command backend 'single t)
      (message "orpheus: repeat this track"))
     (t
      (orpheus-backend-command backend 'repeat t)
      (orpheus-backend-command backend 'single nil)
      (message "orpheus: repeat the queue")))
    (orpheus-ui-invalidate)
    (orpheus--tick)))

(defun orpheus-show-info ()
  "Show the current track, its tags and the position in the echo area."
  (interactive)
  (let* ((track orpheus-track-current)
         (status orpheus-status-current)
         (separator (if orpheus-ui-plain-ascii " - " " · ")))
    (if (null track)
        (message "orpheus: nothing is playing")
      (message "%s%s%s%s%s%s"
               (orpheus-track-label track)
               (if (orpheus-track-album track)
                   (concat separator (orpheus-track-album track))
                 "")
               (if (orpheus-track-year track)
                   (format " (%s)" (orpheus-track-year track))
                 "")
               (format "%s%s / %s"
                       separator
                       (orpheus-format-time
                        (and status (orpheus-status-elapsed status)))
                       (orpheus-format-time
                        (and status (orpheus-status-total status))))
               (format "%svol %d" separator
                       (if status (orpheus-status-volume status) 0))
               (if (orpheus-track-uri track)
                   (concat separator (orpheus-track-uri track))
                 "")))))

(defun orpheus--open-program ()
  "Return the program that opens a file with its default application."
  (pcase system-type
         ('darwin "open")
         ('windows-nt "explorer.exe")
         (_ "xdg-open")))

(defun orpheus-open-externally ()
  "Open the current track in the system's default program.
A backend that keeps its files elsewhere, such as MPD, has no local file to
open, and says so rather than opening a copy that is not here."
  (interactive)
  (let* ((backend orpheus-backend-current)
         (file (and backend
                    (orpheus-backend-local-file backend
                                                orpheus-track-current)))
         (program (orpheus--open-program)))
    (cond
     ((null file)
      (message "orpheus: the current track is not a file here"))
     ((not (file-readable-p file))
      (message "orpheus: %s is no longer here" file))
     ((null (executable-find program))
      (message "orpheus: %s is not installed" program))
     (t
      (make-process :name "orpheus-open"
                    :command (list (executable-find program) file)
                    :buffer nil :noquery t)
      (message "orpheus: opened %s" (file-name-nondirectory file))))))

(defun orpheus-update-database ()
  "Ask the backend to look for new music, then drop cached rows."
  (interactive)
  (orpheus--command 'update)
  (orpheus-ui-invalidate-cache))

(defun orpheus-rescan-database ()
  "Ask the backend to forget the library and read it again."
  (interactive)
  (orpheus--command 'rescan)
  (orpheus-ui-invalidate-cache))

(defun orpheus-refresh ()
  "Read the status and the current tab again."
  (interactive)
  (orpheus-ui-invalidate)
  (orpheus--tick)
  (orpheus-ui-refresh t))

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
      (orpheus-ui-invalidate)
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
      (orpheus-ui-invalidate)
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
      (orpheus-ui-invalidate)
      (orpheus-start)
      (message "orpheus: %s" url))
    (user-error "orpheus: %s cannot play URLs" orpheus-backend)))

;;; The mode line

(defcustom orpheus-mode-line-width 40
  "Columns the track is given in the mode line."
  :type 'integer :group 'orpheus)

(defcustom orpheus-mode-line-controls t
  "Whether the mode line shows transport buttons beside the track.
The buttons are previous, play or pause, next and stop, and each runs its
command on a click.  Turn it off to show only the status glyph and the track."
  :type 'boolean :group 'orpheus)

(defvar orpheus-mode-line--control-maps nil
  "Keymaps for the mode-line transport buttons, one per command.")

(defun orpheus-mode-line--control-map (command)
  "Return the mouse map for a mode-line button that runs COMMAND."
  (or (cdr (assq command orpheus-mode-line--control-maps))
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] command)
        (push (cons command map) orpheus-mode-line--control-maps)
        map)))

(defun orpheus-mode-line--control (glyph command help)
  "Return GLYPH as a mode-line button that runs COMMAND, with HELP as tooltip."
  (propertize glyph
              'face 'orpheus-title-face
              'help-echo help
              'local-map (orpheus-mode-line--control-map command)
              'mouse-face 'mode-line-highlight))

(defun orpheus-mode-line--controls ()
  "Return the previous, play or pause, next and stop buttons."
  (let ((plain orpheus-ui-plain-ascii)
        (playing (eq (and orpheus-status-current
                          (orpheus-status-state orpheus-status-current))
                     'play)))
    (concat
     (orpheus-mode-line--control
      (if plain "<<" "⏮") #'orpheus-previous "mouse-1: previous track")
     " "
     (orpheus-mode-line--control
      (if plain (if playing "||" ">")
        (orpheus-status-glyph orpheus-status-current))
      #'orpheus-play-pause "mouse-1: play or pause")
     " "
     (orpheus-mode-line--control
      (if plain ">>" "⏭") #'orpheus-next "mouse-1: next track")
     " "
     (orpheus-mode-line--control
      (if plain "[]" "■") #'orpheus-stop "mouse-1: stop"))))

(defvar orpheus-mode-line-menu
  (let ((map (make-sparse-keymap "Orpheus")))
    (define-key-after map [play-pause]
      '(menu-item "Play or pause" orpheus-play-pause))
    (define-key-after map [next] '(menu-item "Next track" orpheus-next))
    (define-key-after map [previous]
      '(menu-item "Previous track" orpheus-previous))
    (define-key-after map [stop] '(menu-item "Stop" orpheus-stop))
    (define-key-after map [seek-backward]
      '(menu-item "Seek backward" orpheus-seek-backward))
    (define-key-after map [seek-forward]
      '(menu-item "Seek forward" orpheus-seek-forward))
    (define-key-after map [repeat]
      '(menu-item "Repeat mode" orpheus-cycle-repeat))
    (define-key-after map [random]
      '(menu-item "Shuffle" orpheus-toggle-random))
    (define-key-after map [single]
      '(menu-item "Single track" orpheus-toggle-single))
    (define-key-after map [consume]
      '(menu-item "Consume" orpheus-toggle-consume))
    (define-key-after map [volume-up]
      '(menu-item "Volume up" orpheus-volume-up))
    (define-key-after map [volume-down]
      '(menu-item "Volume down" orpheus-volume-down))
    (define-key-after map [open] '(menu-item "Open the player" orpheus))
    (define-key-after map [detach]
      '(menu-item "Detach to a new frame" orpheus-detach))
    (define-key-after map [equalizer]
      '(menu-item "Equalizer" orpheus-equalizer-menu))
    (define-key-after map [lyrics] '(menu-item "Lyrics" orpheus-lyrics-open))
    (define-key-after map [visualizer]
      '(menu-item "Visualizer" orpheus-visualizer))
    (define-key-after map [quit] '(menu-item "Quit orpheus" orpheus-quit))
    map)
  "Menu opened by a right click on the mode-line track.")

(defvar orpheus-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'orpheus)
    (define-key map [mode-line mouse-2] #'orpheus-play-pause)
    (define-key map [mode-line mouse-3] #'orpheus-mode-line-menu)
    map)
  "Keymap of the orpheus entry in the mode line.")

(defun orpheus-mode-line-menu (event)
  "Open the transport menu for EVENT, a click on the mode line."
  (interactive "e")
  (popup-menu orpheus-mode-line-menu (event-start event)))

(defun orpheus-mode-line-string ()
  "Return the current track and controls for the mode line, or nil.
The track answers the mouse, so it can be played, paused and opened without
leaving the buffer it is shown in; the transport buttons do the same, one
click each."
  (let* ((label (if (and orpheus-track-current
                         (not (string-empty-p
                               (orpheus-track-label orpheus-track-current))))
                    (orpheus-ui--elide
                     (orpheus-track-label orpheus-track-current)
                     orpheus-mode-line-width)
                  "orpheus"))
         (label (propertize
                 label
                 'face 'orpheus-title-face
                 'help-echo
                 "mouse-1: open orpheus, mouse-2: play or pause, mouse-3: more"
                 'local-map orpheus-mode-line-map
                 'mouse-face 'mode-line-highlight)))
    (concat " "
            (if orpheus-mode-line-controls
                (concat (orpheus-mode-line--controls) " ")
              (concat (orpheus-status-glyph orpheus-status-current) " "))
            label)))

(defconst orpheus-mode-line-entry '(:eval (orpheus-mode-line-string))
  "Entry `orpheus-mode-line-mode' keeps in `global-mode-string'.")

(define-minor-mode orpheus-mode-line-mode
  "Show what orpheus is playing in the mode line.
Clicking the track opens the player, plays or pauses, or opens a menu of the
transport commands, so the mode line is a control and not only a label.  The
buttons beside the track do the same on a click."
  :global t
  :group 'orpheus
  (if orpheus-mode-line-mode
      (add-to-list 'global-mode-string orpheus-mode-line-entry t)
    (setq global-mode-string
          (delete orpheus-mode-line-entry global-mode-string)))
  (force-mode-line-update t))

(defun orpheus-detach ()
  "Show the player in a new graphical frame and leave the old window.
The frame is an ordinary Emacs frame: the window manager moves and closes it
like any other.  A terminal has no new frame to give, so the command says so
rather than silently splitting the window."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Detaching needs a graphical frame"))
  (orpheus--ensure-backend)
  (orpheus-start)
  (orpheus-connect)
  (let* ((buffer (get-buffer-create orpheus-ui-buffer-name))
         (old (get-buffer-window buffer)))
    (orpheus-ui-open)
    (select-frame (make-frame '((name . "orpheus"))))
    (set-window-buffer (selected-window) buffer)
    (when (and old (window-live-p old) (not (eq old (selected-window))))
      (delete-window old))))

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
