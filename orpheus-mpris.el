;;; orpheus-mpris.el --- The desktop's media keys and now-playing widget -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; A media key does not reach a program: it reaches the desktop, which hands
;;; it to whichever MPRIS2 player is playing.  This file makes orpheus one of
;;; them while the player runs, so the pause key pauses orpheus, the next key
;;; skips a track, and the desktop's now-playing widget shows the track, its
;;; artist and its cover.
;;;
;;; The protocol is a name on the session bus, a handful of methods and the
;;; properties the desktop reads back.  Nothing here reaches the player
;;; directly: every method calls the same command the player's own keys call.
;;;
;;; The properties are registered read-only and published by re-registering
;;; them, which is how `dbus-register-property' updates a read-only value and
;;; announces it, so a client cannot change what the desktop believes orpheus
;;; is playing.  The position is the exception: it is stored without a
;;; PropertiesChanged signal, because MPRIS clients ask for it on a timer
;;; instead of listening to it.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'orpheus-backend)

(declare-function orpheus "orpheus" ())
(declare-function orpheus--command "orpheus" (action &optional value))
(declare-function orpheus-next "orpheus" ())
(declare-function orpheus-pause "orpheus" ())
(declare-function orpheus-play "orpheus" ())
(declare-function orpheus-play-pause "orpheus" ())
(declare-function orpheus-previous "orpheus" ())
(declare-function orpheus-stop "orpheus" ())
(defvar orpheus-ui--art)

;;; Settings

(defcustom orpheus-mpris-enabled t
  "Whether orpheus answers the desktop's media keys.
This is what MPRIS2 is for, and it needs a session bus, so it is on wherever
one exists and does nothing where there is none.  Turning it off leaves the
player exactly as it was."
  :type 'boolean
  :group 'orpheus)

;;; Names

(defconst orpheus-mpris--service "org.mpris.MediaPlayer2.orpheus"
  "The name orpheus owns on the session bus while it runs.")

(defconst orpheus-mpris--path "/org/mpris/MediaPlayer2"
  "The object every MPRIS player answers at.")

(defconst orpheus-mpris--root "org.mpris.MediaPlayer2"
  "The interface with the properties about the application itself.")

(defconst orpheus-mpris--player "org.mpris.MediaPlayer2.Player"
  "The interface with the transport and the current track.")

(defconst orpheus-mpris--no-track "/org/mpris/MediaPlayer2/TrackList/NoTrack"
  "The track id MPRIS uses when nothing is playing.")

(defvar orpheus-mpris--active nil
  "Whether orpheus currently owns its name on the session bus.")

(defvar orpheus-mpris--reported nil
  "The player properties the desktop was last told about.")

(defconst orpheus-mpris--fixed
  '(("org.mpris.MediaPlayer2" "Identity" . (:string "orpheus"))
    ("org.mpris.MediaPlayer2" "CanQuit" . (:boolean nil))
    ("org.mpris.MediaPlayer2" "CanRaise" . (:boolean t))
    ("org.mpris.MediaPlayer2" "HasTrackList" . (:boolean nil))
    ("org.mpris.MediaPlayer2.Player" "CanPlay" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "CanPause" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "CanGoNext" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "CanGoPrevious" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "CanSeek" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "CanControl" . (:boolean t))
    ("org.mpris.MediaPlayer2.Player" "MinimumRate" . (:double 1.0))
    ("org.mpris.MediaPlayer2.Player" "MaximumRate" . (:double 1.0))
    ("org.mpris.MediaPlayer2.Player" "Rate" . (:double 1.0)))
  "Properties that stay the same while orpheus runs.
Each entry is the interface, the property and its typed value.  A player that
cannot quit, or has no track list, says so rather than offering a method that
would fail.")

;;; What the desktop reads

(defun orpheus-mpris--playback-status (status)
  "Return STATUS as the string MPRIS reports for it."
  (pcase (and status (orpheus-status-state status))
    ('play "Playing")
    ('pause "Paused")
    (_ "Stopped")))

(defun orpheus-mpris--loop-status (status)
  "Return STATUS as the MPRIS loop status: None, Track or Playlist."
  (cond
   ((and status (orpheus-status-single status)) "Track")
   ((and status (orpheus-status-repeat status)) "Playlist")
   (t "None")))

(defun orpheus-mpris--trackid (status)
  "Return the object path naming the track STATUS plays.
An object path takes letters, digits and underscores only, so a track
identifier a backend chose is made to fit; with nothing playing, MPRIS has a
path that means exactly that."
  (let ((id (and status (orpheus-status-songid status))))
    (setq id (and id (replace-regexp-in-string
                      "[^A-Za-z0-9_]" "_" (format "%s" id))))
    (if (and id (not (string-empty-p id)))
        (concat "/org/mpris/MediaPlayer2/orpheus/track/" id)
      orpheus-mpris--no-track)))

(defun orpheus-mpris--text (property value)
  "Return PROPERTY as a typed string entry for VALUE, or nil when it is empty."
  (and value (not (string-empty-p value))
       (list (cons property (list :string value)))))

(defun orpheus-mpris--tags (track)
  "Return the tags of TRACK MPRIS names, each with its D-Bus type."
  (let ((trackno (orpheus-track-trackno track)))
    (append
     (orpheus-mpris--text "xesam:title" (orpheus-track-title track))
     (orpheus-mpris--text "xesam:album" (orpheus-track-album track))
     (orpheus-mpris--text "xesam:albumArtist" (orpheus-track-albumartist track))
     (orpheus-mpris--text "xesam:url" (orpheus-track-uri track))
     (let ((artist (orpheus-track-artist track)))
       (when (and artist (not (string-empty-p artist)))
         (list (cons "xesam:artist" (list :array :string artist)))))
     (when (and trackno (string-match-p "\\`[0-9]+\\'" trackno))
       (list (cons "xesam:trackNumber"
                   (list :int32 (string-to-number trackno))))))))

(defun orpheus-mpris--metadata (track status &optional art)
  "Return TRACK, as STATUS plays it, as the MPRIS metadata alist.
ART is the cover file a desktop may show.  A tag that is not known is left out
rather than sent empty, so a desktop shows nothing where the player would have
shown nothing either."
  (append
   (list (cons "mpris:trackid"
               (list :object-path (orpheus-mpris--trackid status))))
   (let ((total (and status (orpheus-status-total status))))
     (when (and total (> total 0))
       (list (cons "mpris:length"
                   (list :int64 (round (* total 1000000)))))))
   (when (and art (file-name-absolute-p art))
     (list (cons "mpris:artUrl"
                 (list :string (concat "file://" (expand-file-name art))))))
   (when track
     (orpheus-mpris--tags track))))

(defun orpheus-mpris--dict (alist)
  "Return ALIST as the dictionary of variants MPRIS carries metadata in."
  (append
   (list :array)
   (mapcar (lambda (entry)
             (list :dict-entry :string (car entry) :variant (cdr entry)))
           alist)))

(defun orpheus-mpris--values ()
  "Return the player properties that change while orpheus plays."
  (let* ((status orpheus-status-current)
         (track orpheus-track-current)
         (art (plist-get orpheus-ui--art :file))
         (volume (or (and status (orpheus-status-volume status)) 0)))
    (list (cons "PlaybackStatus"
                (list :string (orpheus-mpris--playback-status status)))
          (cons "Metadata"
                (orpheus-mpris--dict
                 (orpheus-mpris--metadata track status art)))
          (cons "LoopStatus"
                (list :string (orpheus-mpris--loop-status status)))
          (cons "Shuffle"
                (list :boolean (and status (orpheus-status-random status))))
          (cons "Volume" (list :double (/ volume 100.0))))))

(defun orpheus-mpris--position ()
  "Return the position as MPRIS counts it, in microseconds."
  (list :int64
        (round (* 1000000.0
                  (or (and orpheus-status-current
                           (orpheus-status-elapsed orpheus-status-current))
                      0.0)))))

;;; Publishing

(defun orpheus-mpris--publish (property value &optional signal)
  "Register PROPERTY with VALUE on the player.
The desktop is told about the change when SIGNAL is non-nil, which is what
makes its widget follow the player."
  (dbus-register-property :session orpheus-mpris--service orpheus-mpris--path
                          orpheus-mpris--player property :read value signal t))

(defun orpheus-mpris--sync ()
  "Tell the desktop whatever changed since the last call."
  (when orpheus-mpris--active
    (dolist (entry (orpheus-mpris--values))
      (unless (equal (cdr (assoc (car entry) orpheus-mpris--reported))
                     (cdr entry))
        (orpheus-mpris--publish (car entry) (cdr entry) t)
        (setf (alist-get (car entry) orpheus-mpris--reported nil nil #'equal)
              (cdr entry))))
    (orpheus-mpris--publish "Position" (orpheus-mpris--position))))

;;; The calls the desktop makes

(defun orpheus-mpris--register-command (method command)
  "Make the MPRIS METHOD run the player's COMMAND."
  (dbus-register-method
   :session orpheus-mpris--service orpheus-mpris--path orpheus-mpris--player
   method (lambda () (funcall command) :ignore) 'dont-register))

(defun orpheus-mpris--seek (seconds)
  "Move SECONDS from where the track is now."
  (orpheus--command 'seek seconds)
  ;; A method with no return value says so with `:ignore'; nil would be sent
  ;; as the boolean false.
  :ignore)

(defun orpheus-mpris--register-methods ()
  "Register the transport the desktop calls with a media key."
  (dolist (entry '(("Play" . orpheus-play)
                   ("Pause" . orpheus-pause)
                   ("PlayPause" . orpheus-play-pause)
                   ("Stop" . orpheus-stop)
                   ("Next" . orpheus-next)
                   ("Previous" . orpheus-previous)))
    (orpheus-mpris--register-command (car entry) (cdr entry)))
  (dbus-register-method
   :session orpheus-mpris--service orpheus-mpris--path orpheus-mpris--player
   "Seek" (lambda (offset) (orpheus-mpris--seek (/ offset 1000000.0)))
   'dont-register)
  (dbus-register-method
   :session orpheus-mpris--service orpheus-mpris--path orpheus-mpris--player
   "SetPosition"
   (lambda (_trackid position)
     ;; The player's backends seek by a distance, so an absolute position is
     ;; the distance from where the track is now.
     (let ((elapsed (or (and orpheus-status-current
                             (orpheus-status-elapsed orpheus-status-current))
                        0.0)))
       (orpheus-mpris--seek (- (/ position 1000000.0) elapsed))))
   'dont-register)
  (dbus-register-method
   :session orpheus-mpris--service orpheus-mpris--path orpheus-mpris--root
   "Raise" (lambda () (orpheus) :ignore) 'dont-register))

(defun orpheus-mpris--register-fixed ()
  "Publish the properties that do not change while orpheus runs."
  (dolist (entry orpheus-mpris--fixed)
    (dbus-register-property :session orpheus-mpris--service
                            orpheus-mpris--path (nth 0 entry) (nth 1 entry)
                            :read (nthcdr 2 entry) nil t)))

;;; Turning it on and off

(defun orpheus-mpris-start ()
  "Offer orpheus to the desktop as the MPRIS player it is playing.
Return non-nil when orpheus owns the name afterwards.  Another player already
holding the name is left alone: the desktop keeps sending the keys there."
  (when (and orpheus-mpris-enabled
             (featurep 'dbusbind)
             (not noninteractive)
             (not orpheus-mpris--active))
    (condition-case nil
        (unless (member orpheus-mpris--service (dbus-list-names :session))
          (dbus-register-service :session orpheus-mpris--service)
          (setq orpheus-mpris--active t
                orpheus-mpris--reported nil)
          (orpheus-mpris--register-methods)
          (orpheus-mpris--register-fixed)
          (orpheus-mpris--sync))
      ;; A session with no bus, or a bus that refuses the name, is a desktop
      ;; that will not send media keys; the player works without one.
      (error (setq orpheus-mpris--active nil)))
    orpheus-mpris--active))

(defun orpheus-mpris-stop ()
  "Stop answering the desktop's media keys."
  (interactive)
  (when orpheus-mpris--active
    (setq orpheus-mpris--active nil
          orpheus-mpris--reported nil)
    (ignore-errors
      (dbus-unregister-service :session orpheus-mpris--service))))

(provide 'orpheus-mpris)

;;; orpheus-mpris.el ends here
