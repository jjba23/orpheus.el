;;; orpheus-mpd.el --- Music Player Daemon backend -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; An MPD client written directly on `make-network-process'.  Everything MPD
;; says arrives through the process filter, so no command blocks the editor:
;; the reply to a command is handed to the callback that asked for it, in the
;; order MPD answered them.
;;
;; MPD's reply is a stream of `key: value' lines ending in `OK', or in a
;; one-line `ACK' error, and the `albumart' and `readpicture' commands add a
;; binary block announced by its byte count.  The parser below keeps the
;; connection byte-clean and decodes only the text lines, which is what lets
;; cover art survive the trip.
;;
;; The daemon owns the audio output, so PipeWire, PulseAudio, ALSA or CoreAudio
;; are MPD's business and this backend is platform-neutral: the same client
;; talks to a unix socket at home and to a TCP server across the world.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)

(declare-function auth-source-search "auth-source" (&rest args))


;;; Settings

(defcustom orpheus-mpd-host "localhost"
  "Host MPD listens on, when `orpheus-mpd-socket' is nil."
  :type 'string :group 'orpheus)

(defcustom orpheus-mpd-port 6600
  "Port MPD listens on, when `orpheus-mpd-socket' is nil."
  :type 'integer :group 'orpheus)

(defcustom orpheus-mpd-socket nil
  "Unix socket MPD listens on, or nil to use host and port.
Most desktop setups run MPD with a socket in the user's runtime directory;
a remote server is reached over TCP."
  :type '(choice (const :tag "Use host and port" nil) file)
  :group 'orpheus)

(defcustom orpheus-mpd-password nil
  "Password for MPD, or nil to take it from the environment.
When nil, `MPD_PASSWORD' is read, then `auth-source' is asked; a password
belongs nowhere in this file."
  :type '(choice (const :tag "Ask the environment" nil) string)
  :group 'orpheus)

(defcustom orpheus-mpd-search-tag "any"
  "Tag MPD searches in; `any' looks at every tag."
  :type 'string :group 'orpheus)


;;; The connection

(cl-defstruct (orpheus-mpd-connection (:constructor orpheus-mpd-connection-create))
  "One open link to MPD, with the state of the conversation on it."
  backend
  process
  (state 'connecting)
  (accum "")
  (queue nil)
  (pending nil)
  (lines nil)
  (binary nil)
  (binary-left 0)
  (on-ready nil)
  (password nil)
  (version nil))

(cl-defstruct (orpheus-mpd (:include orpheus-backend)
                           (:constructor make-orpheus-mpd)
                           (:conc-name orpheus-mpd-instance-))
  "The MPD backend, pointing at one daemon."
  host
  port
  socket
  password
  connection)

(defun orpheus-mpd--make ()
  "Return a new MPD backend built from the user options."
  (make-orpheus-mpd :name 'mpd
                    :host orpheus-mpd-host
                    :port orpheus-mpd-port
                    :socket orpheus-mpd-socket
                    :password orpheus-mpd-password))

(orpheus-register-backend 'mpd #'orpheus-mpd--make "Music Player Daemon")


;;; Talking MPD

(defun orpheus-mpd--quote (string)
  "Return STRING quoted the way MPD parses a single argument."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" string) "\""))

(defun orpheus-mpd--parse-line (line)
  "Return LINE as a cons of its key and its value.
The value is decoded as UTF-8; a line without a colon keeps its text as the
key, which is how MPD reports a few bare messages."
  (let ((decoded (decode-coding-string line 'utf-8)))
    (if (string-match "\\`\\([^:]*\\): ?\\(.*\\)\\'" decoded)
        (cons (match-string 1 decoded) (match-string 2 decoded))
      (cons decoded ""))))

(defun orpheus-mpd--parse-lines (string)
  "Return STRING, an MPD reply, as a list of key and value pairs."
  (let ((lines nil))
    (dolist (line (split-string string "\n"))
      (unless (string-empty-p line)
        (push (orpheus-mpd--parse-line line) lines)))
    (nreverse lines)))

(defun orpheus-mpd--line-value (lines key)
  "Return the first value of KEY in LINES."
  (cdr (assoc key lines)))

(defun orpheus-mpd--command-line (command args)
  "Return COMMAND and ARGS as one line, as MPD reads it."
  (mapconcat #'identity (cons command (delq nil args)) " "))

(defun orpheus-mpd--write (conn line)
  "Send LINE to CONN's process."
  (let ((process (orpheus-mpd-connection-process conn)))
    (if (process-live-p process)
        (process-send-string process (encode-coding-string
                                      (concat line "\n") 'binary))
      (orpheus-mpd--lost conn "the connection is closed"))))

(defun orpheus-mpd--request (conn line callback)
  "Send LINE on CONN and call CALLBACK with the reply when it arrives."
  (setf (orpheus-mpd-connection-queue conn)
        (append (orpheus-mpd-connection-queue conn) (list callback)))
  (if (eq (orpheus-mpd-connection-state conn) 'ready)
      (orpheus-mpd--write conn line)
    (setf (orpheus-mpd-connection-pending conn)
          (append (orpheus-mpd-connection-pending conn) (list line)))))

(defun orpheus-mpd--finish (conn error)
  "Close the reply in flight on CONN, with ERROR when MPD refused it."
  (let* ((callback (car (orpheus-mpd-connection-queue conn)))
         (lines (nreverse (orpheus-mpd-connection-lines conn)))
         (binary (orpheus-mpd-connection-binary conn)))
    (setf (orpheus-mpd-connection-queue conn)
          (cdr (orpheus-mpd-connection-queue conn))
          (orpheus-mpd-connection-lines conn) nil
          (orpheus-mpd-connection-binary conn) nil
          (orpheus-mpd-connection-binary-left conn) 0)
    (when callback
      (condition-case err
          (funcall callback (list :ok (null error) :error error
                                  :lines lines :binary binary))
        (error (message "orpheus: MPD callback failed: %S" err))))))

(defun orpheus-mpd--handle-line (conn line)
  "Interpret one complete LINE read on CONN."
  (cond
   ((string-empty-p line) nil)
   ((eq (orpheus-mpd-connection-state conn) 'connecting)
    (if (string-match "\\`OK MPD \\(.+\\)\\'" line)
        (progn
          (setf (orpheus-mpd-connection-version conn) (match-string 1 line))
          (setf (orpheus-mpd-connection-state conn) 'auth)
          (orpheus-mpd--authenticate conn))
      (orpheus-mpd--fail conn (format "unexpected greeting: %s" line))))
   ((equal line "OK") (orpheus-mpd--finish conn nil))
   ((string-prefix-p "ACK " line) (orpheus-mpd--finish conn (substring line 4)))
   ((string-match "\\`binary: \\([0-9]+\\)\\'" line)
    (setf (orpheus-mpd-connection-binary-left conn)
          (string-to-number (match-string 1 line))))
   (t (push (orpheus-mpd--parse-line line)
            (orpheus-mpd-connection-lines conn)))))

(defun orpheus-mpd--consume (conn)
  "Take every complete line and payload waiting in CONN's buffer."
  (let ((more t))
    (while more
      (setq more nil)
      (let* ((accum (orpheus-mpd-connection-accum conn))
             (left (orpheus-mpd-connection-binary-left conn)))
        (cond
         ;; A binary block is on its way: take what has arrived, and stop as
         ;; soon as the announced byte count is complete.
         ((> left 0)
          (let ((take (min left (length accum))))
            (when (> take 0)
              (setf (orpheus-mpd-connection-binary conn)
                    (concat (orpheus-mpd-connection-binary conn)
                            (substring accum 0 take)))
              (setf (orpheus-mpd-connection-accum conn) (substring accum take))
              (setf (orpheus-mpd-connection-binary-left conn) (- left take))
              (when (zerop (orpheus-mpd-connection-binary-left conn))
                ;; The newline after the payload is protocol, not data.
                (let ((rest (orpheus-mpd-connection-accum conn)))
                  (when (string-prefix-p "\n" rest)
                    (setf (orpheus-mpd-connection-accum conn)
                          (substring rest 1)))))
              (setq more t))))
         ((string-match "\n" accum)
          (let ((line (substring accum 0 (match-beginning 0))))
            (setf (orpheus-mpd-connection-accum conn)
                  (substring accum (match-end 0)))
            (orpheus-mpd--handle-line conn line)
            (setq more t))))))))

(defun orpheus-mpd--filter (conn chunk)
  "Feed CHUNK, as it arrives, to CONN's parser."
  (setf (orpheus-mpd-connection-accum conn)
        (concat (orpheus-mpd-connection-accum conn) chunk))
  (orpheus-mpd--consume conn))

(defun orpheus-mpd--authenticate (conn)
  "Log CONN in, then mark it ready."
  (let ((password (orpheus-mpd-connection-password conn)))
    (if (and password (not (string-empty-p password)))
        (progn
          ;; The password's own reply is queued like any other, so commands
          ;; issued meanwhile wait their turn instead of racing it.
          (setf (orpheus-mpd-connection-queue conn)
                (append (orpheus-mpd-connection-queue conn)
                        (list (lambda (reply)
                                (if (plist-get reply :ok)
                                    (orpheus-mpd--ready conn)
                                  (orpheus-mpd--fail
                                   conn (plist-get reply :error)))))))
          (orpheus-mpd--write conn
                              (format "password %s"
                                      (orpheus-mpd--quote password))))
      (orpheus-mpd--ready conn))))

(defun orpheus-mpd--ready (conn)
  "Send the commands CONN queued while it was connecting."
  (setf (orpheus-mpd-connection-state conn) 'ready)
  (let ((pending (orpheus-mpd-connection-pending conn))
        (callbacks (orpheus-mpd-connection-on-ready conn)))
    (setf (orpheus-mpd-connection-pending conn) nil
          (orpheus-mpd-connection-on-ready conn) nil)
    (dolist (line pending) (orpheus-mpd--write conn line))
    (dolist (callback callbacks)
      (condition-case err
          (funcall callback conn)
        (error (message "orpheus: MPD connect callback failed: %S" err))))))

(defun orpheus-mpd--credential (mpd)
  "Return MPD's password from the environment or auth-source, if any.
MPD is the backend the password is wanted for, and gives auth-source the
host and port to look up."
  (or (getenv "MPD_PASSWORD")
      (when (require 'auth-source nil t)
        (let ((entry (car (auth-source-search
                           :host (orpheus-mpd-instance-host mpd)
                           :port (number-to-string
                                  (or (orpheus-mpd-instance-port mpd) 6600))
                           :require '(:secret)))))
          (when entry
            (let ((secret (plist-get entry :secret)))
              (if (functionp secret) (funcall secret) secret)))))))

(defun orpheus-mpd--lost (conn event)
  "Forget CONN's process after EVENT and tell the session."
  (unless (eq (orpheus-mpd-connection-state conn) 'closed)
    (setf (orpheus-mpd-connection-state conn) 'closed)
    (let ((backend (orpheus-mpd-connection-backend conn)))
      (when (eq (orpheus-mpd-instance-connection backend) conn)
        (setf (orpheus-mpd-instance-connection backend) nil)))
    (let ((callbacks (orpheus-mpd-connection-on-ready conn)))
      (setf (orpheus-mpd-connection-on-ready conn) nil)
      (dolist (callback callbacks)
        (condition-case nil (funcall callback nil) (error nil))))
    (run-hook-with-args 'orpheus-backend-disconnected-hook
                        (orpheus-mpd-connection-backend conn) event)))

(defun orpheus-mpd--fail (conn message)
  "Give up on CONN because of MESSAGE."
  (let ((process (orpheus-mpd-connection-process conn)))
    (when (process-live-p process) (delete-process process)))
  (orpheus-mpd--lost conn message))

(defun orpheus-mpd--sentinel (conn event)
  "Notice CONN dying, as described by EVENT."
  (unless (process-live-p (orpheus-mpd-connection-process conn))
    (orpheus-mpd--lost conn event)))

(defun orpheus-mpd--open (mpd callback)
  "Open MPD's socket for MPD, then call CALLBACK with the connection."
  (let* ((conn (orpheus-mpd-connection-create
                :backend mpd
                :password (or (orpheus-mpd-instance-password mpd)
                              (orpheus-mpd--credential mpd))
                :on-ready (list callback)))
         (socket (orpheus-mpd-instance-socket mpd))
         (socket (when socket (expand-file-name socket)))
         (process (condition-case err
                      (apply #'make-network-process
                             (append
                              (list :name "orpheus-mpd"
                                    :buffer nil
                                    :coding 'binary
                                    :noquery t
                                    :filter (lambda (_process chunk)
                                              (orpheus-mpd--filter conn chunk))
                                    :sentinel (lambda (_process event)
                                                (orpheus-mpd--sentinel conn event)))
                              ;; A socket is addressed by file name; a daemon
                              ;; elsewhere by host and port.
                              (if socket
                                  (list :family 'local :service socket)
                                (list :host (or (orpheus-mpd-instance-host mpd)
                                                "localhost")
                                      :service (or (orpheus-mpd-instance-port mpd)
                                                   6600)))))
                    (error (message "orpheus: cannot reach MPD: %s"
                                    (error-message-string err))
                           nil))))
    (if process
        (progn
          (setf (orpheus-mpd-connection-process conn) process)
          (setf (orpheus-mpd-instance-connection mpd) conn)
          conn)
      (dolist (cb (orpheus-mpd-connection-on-ready conn))
        (ignore-errors (funcall cb nil)))
      nil)))

(defun orpheus-mpd--connect (mpd callback)
  "Ensure MPD is connected, then call CALLBACK with the connection or nil."
  (let ((conn (orpheus-mpd-instance-connection mpd)))
    (cond
     ((and conn (process-live-p (orpheus-mpd-connection-process conn)))
      (if (eq (orpheus-mpd-connection-state conn) 'ready)
          (funcall callback conn)
        (setf (orpheus-mpd-connection-on-ready conn)
              (append (orpheus-mpd-connection-on-ready conn)
                      (list callback)))))
     (t (orpheus-mpd--open mpd callback)))))

(defun orpheus-mpd--with-connection (mpd callback)
  "Call CALLBACK with a ready connection to MPD, opening one when needed."
  (orpheus-mpd--connect
   mpd
   (lambda (conn)
     (if conn
         (funcall callback conn)
       ;; A caller that expects a reply must not wait forever for a server
       ;; that never answered.
       (funcall callback nil)))))

(defun orpheus-mpd--send-command (mpd command &optional args callback)
  "Send COMMAND to MPD with ARGS, then call CALLBACK with the reply plist."
  (orpheus-mpd--with-connection
   mpd
   (lambda (conn)
     (if (null conn)
         (when callback
           (funcall callback (list :ok nil :error "MPD is not reachable"
                                   :lines nil :binary nil)))
       (orpheus-mpd--request conn
                             (orpheus-mpd--command-line command args)
                             (or callback #'ignore))))))


;;; Reading replies

(defun orpheus-mpd--status-from-lines (lines)
  "Return LINES, a status reply, as an `orpheus-status'."
  (let* ((value (lambda (key) (orpheus-mpd--line-value lines key)))
         (number (lambda (key)
                   (string-to-number (or (funcall value key) "0"))))
         (flag (lambda (key) (equal (funcall value key) "1"))))
    (let ((time (funcall value "time")))
      (orpheus-status-create
       :state (pcase (funcall value "state")
                ("play" 'play)
                ("pause" 'pause)
                (_ 'stop))
       :elapsed (if (and time (string-match "\\`\\([0-9.]+\\):" time))
                    (string-to-number (match-string 1 time))
                  (funcall number "elapsed"))
       :total (if (and time (string-match ":\\([0-9.]+\\)\\'" time))
                  (string-to-number (match-string 1 time))
                (funcall number "duration"))
       :volume (funcall number "volume")
       :bitrate (funcall number "bitrate")
       :songid (let ((id (funcall value "songid")))
                 (when id (string-to-number id)))
       :repeat (funcall flag "repeat")
       :random (funcall flag "random")
       :single (funcall flag "single")
       :consume (funcall flag "consume")))))

(defun orpheus-mpd--track-from-lines (lines)
  "Return LINES, a song reply, as an `orpheus-track', or nil for no song."
  (let ((uri (orpheus-mpd--line-value lines "file")))
    (when uri
      (let ((value (lambda (key) (orpheus-mpd--line-value lines key))))
        (orpheus-track-create
         :uri uri
         :title (funcall value "Title")
         :artist (or (funcall value "Artist") (funcall value "AlbumArtist"))
         :album (funcall value "Album")
         :albumartist (funcall value "AlbumArtist")
         :trackno (let ((track (funcall value "Track")))
                    (when track (string-to-number track)))
         :year (or (funcall value "Date") (funcall value "OriginalDate"))
         :genre (funcall value "Genre")
         :duration (string-to-number
                    (or (funcall value "duration")
                        (funcall value "Time") "0")))))))

(defun orpheus-mpd--songs (lines)
  "Split reply LINES into one alist of tags per song."
  (orpheus-mpd--blocks lines "file"))

(defun orpheus-mpd--blocks (lines key)
  "Split LINES into blocks, each starting at a KEY line."
  (let ((blocks nil)
        (current nil))
    (dolist (line lines)
      (when (equal (car line) key)
        (when current (push (nreverse current) blocks))
        (setq current nil))
      (when (or current (equal (car line) key))
        (push line current)))
    (when current (push (nreverse current) blocks))
    (nreverse blocks)))

(defun orpheus-mpd--strings (lines key)
  "Return every value of KEY in LINES, in order."
  (let ((values nil))
    (dolist (line lines)
      (when (equal (car line) key)
        (push (cdr line) values)))
    (nreverse values)))


;;; Backend methods

(defun orpheus-mpd--item-from-song (song &optional queue-p)
  "Return an `orpheus-item' for SONG, numbered when QUEUE-P."
  (let* ((track (orpheus-mpd--track-from-lines song))
         (pos (string-to-number
               (or (orpheus-mpd--line-value song "Pos") "0")))
         (id (string-to-number
              (or (orpheus-mpd--line-value song "Id") "0")))
         (label (orpheus-track-label track)))
    (orpheus-item-create
     :label (if queue-p (format "%2d  %s" (1+ pos) label) label)
     :detail (if queue-p
                 (orpheus-format-time (orpheus-track-duration track))
               (or (orpheus-track-album track)
                   (orpheus-format-time (orpheus-track-duration track))))
     :kind 'track
     :data (list (cons 'uri (orpheus-track-uri track))
                 (cons 'id id)
                 (cons 'pos pos)
                 (cons 'track track)))))

(defun orpheus-mpd--items-from-songs (lines &optional queue-p)
  "Return the songs in LINES as rows, numbered when QUEUE-P."
  (mapcar (lambda (song) (orpheus-mpd--item-from-song song queue-p))
          (orpheus-mpd--songs lines)))

(defun orpheus-mpd--status-request (mpd callback)
  "Ask MPD for a status, then call CALLBACK with an `orpheus-status'."
  (orpheus-mpd--send-command
   mpd "status" nil
   (lambda (reply)
     (funcall callback
              (when (plist-get reply :ok)
                (orpheus-mpd--status-from-lines (plist-get reply :lines)))))))

(defun orpheus-mpd--track-request (mpd callback)
  "Ask MPD for the current song, then call CALLBACK with a track or nil."
  (orpheus-mpd--send-command
   mpd "currentsong" nil
   (lambda (reply)
     (funcall callback
              (and (plist-get reply :ok)
                   (orpheus-mpd--track-from-lines
                    (plist-get reply :lines)))))))

(cl-defmethod orpheus-backend-connected-p ((mpd orpheus-mpd))
  "Return non-nil when MPD is logged in and listening."
  (let ((conn (orpheus-mpd-instance-connection mpd)))
    (and conn
         (eq (orpheus-mpd-connection-state conn) 'ready)
         (process-live-p (orpheus-mpd-connection-process conn)))))

(cl-defmethod orpheus-backend-connect ((mpd orpheus-mpd) callback)
  "Connect MPD, then call CALLBACK with it."
  (orpheus-mpd--connect mpd
                        (lambda (conn)
                          (funcall callback (when conn mpd)))))

(cl-defmethod orpheus-backend-disconnect ((mpd orpheus-mpd))
  "Close the connection to MPD."
  (let ((conn (orpheus-mpd-instance-connection mpd)))
    (when conn
      (setf (orpheus-mpd-instance-connection mpd) nil)
      (setf (orpheus-mpd-connection-state conn) 'closed)
      (let ((process (orpheus-mpd-connection-process conn)))
        (when (process-live-p process) (delete-process process))))))

(cl-defmethod orpheus-backend-status ((mpd orpheus-mpd) callback)
  "Ask MPD for its status, then call CALLBACK with it."
  (orpheus-mpd--status-request mpd callback))

(cl-defmethod orpheus-backend-track ((mpd orpheus-mpd) callback)
  "Ask MPD for the song it is playing, then call CALLBACK with it."
  (orpheus-mpd--track-request mpd callback))

(cl-defmethod orpheus-backend-queue ((mpd orpheus-mpd) callback)
  "Ask MPD for the queue, then call CALLBACK with the rows."
  (orpheus-mpd--send-command
   mpd "playlistinfo" nil
   (lambda (reply)
     (funcall callback
              (when (plist-get reply :ok)
                (orpheus-mpd--items-from-songs (plist-get reply :lines) t))))))

(cl-defmethod orpheus-backend-add ((mpd orpheus-mpd) uris)
  "Append URIS to MPD's queue."
  (dolist (uri (if (listp uris) uris (list uris)))
    (orpheus-mpd--send-command mpd "add" (list (orpheus-mpd--quote uri)))))

(cl-defmethod orpheus-backend-play-uri ((mpd orpheus-mpd) uri)
  "Append URI to MPD's queue and start it."
  (orpheus-mpd--send-command
   mpd "addid" (list (orpheus-mpd--quote uri))
   (lambda (reply)
     (let ((id (and (plist-get reply :ok)
                    (orpheus-mpd--line-value (plist-get reply :lines) "Id"))))
       (when id
         (orpheus-mpd--send-command mpd "playid" (list id)))))))

(cl-defmethod orpheus-backend-album-art ((mpd orpheus-mpd) track callback)
  "Fetch cover art for TRACK from MPD, then call CALLBACK with a file path."
  (let ((uri (orpheus-track-uri track)))
    (if (or (null uri) (string-empty-p uri))
        (funcall callback nil)
      (orpheus-mpd--album-art-try mpd "albumart" uri callback))))

(defun orpheus-mpd--album-art-try (mpd command uri callback)
  "Ask MPD for URI's art with COMMAND, falling back to `readpicture'."
  (orpheus-mpd--send-command
   mpd command (list (orpheus-mpd--quote uri) "0")
   (lambda (reply)
     (cond
      ((and (plist-get reply :ok) (plist-get reply :binary))
       (let* ((type (or (orpheus-mpd--line-value (plist-get reply :lines) "type")
                        "image/jpeg"))
              (name (pcase type
                      ((or "image/png" "PNG") "cover.png")
                      ((or "image/gif" "GIF") "cover.gif")
                      ((or "image/webp" "WEBP") "cover.webp")
                      (_ "cover.jpg")))
              (path (orpheus-cache-file uri name)))
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert (plist-get reply :binary))
           (write-region (point-min) (point-max) path nil 'silent))
         (funcall callback path)))
      ((equal command "albumart")
       (orpheus-mpd--album-art-try mpd "readpicture" uri callback))
      (t (funcall callback nil))))))

(cl-defmethod orpheus-backend-command ((mpd orpheus-mpd) action &optional value)
  "Perform ACTION on MPD, with VALUE when it takes one."
  (let ((send (lambda (command &optional args callback)
                (orpheus-mpd--send-command mpd command args callback))))
    (pcase action
      ('play (funcall send "play"))
      ('playpos (funcall send "play" (list (number-to-string value))))
      ('pause (funcall send "pause" '("1")))
      ('resume (funcall send "pause" '("0")))
      ('toggle (funcall send "pause"))
      ('stop (funcall send "stop"))
      ('next (funcall send "next"))
      ('previous (funcall send "previous"))
      ('seek (funcall send "seekcur" (list (format "%+g" (or value 0)))))
      ('volume (funcall send "setvol"
                        (list (number-to-string (max 0 (min 100 value))))))
      ('repeat (funcall send "repeat" (list (if value "1" "0"))))
      ('random (funcall send "random" (list (if value "1" "0"))))
      ('single (funcall send "single" (list (if value "1" "0"))))
      ('consume (funcall send "consume" (list (if value "1" "0"))))
      ('update (funcall send "update"))
      ('rescan (funcall send "rescan"))
      ('clear (funcall send "clear"))
      ('shuffle (funcall send "shuffle"))
      ('delete (funcall send "delete" (list (number-to-string value))))
      ('save (funcall send "save" (list (orpheus-mpd--quote value))))
      ('playid (funcall send "playid" (list (number-to-string value))))
      ('load (funcall send "load" (list (orpheus-mpd--quote value))))
      (_ (message "orpheus: MPD cannot %s" action)))))

(cl-defmethod orpheus-backend-browse ((mpd orpheus-mpd) kind arg callback)
  "Browse KIND in MPD, with ARG as the path down the hierarchy."
  (let ((send (lambda (command &optional args then)
                (orpheus-mpd--send-command mpd command args then)))
        (arg (if (listp arg) arg (when arg (list arg)))))
    (pcase kind
      ('directories
       (let ((dir (car arg)))
         (funcall send "lsinfo"
                  ;; The root is asked for with no argument at all.
                  (when (and dir (not (string-empty-p dir)))
                    (list (orpheus-mpd--quote dir)))
                  (lambda (reply)
                    (funcall callback
                             (when (plist-get reply :ok)
                               (orpheus-mpd--directory-items
                                (plist-get reply :lines))))))))
      ((or 'artists 'albumartists)
       (orpheus-mpd--browse-artist mpd kind arg callback))
      ('albums
       (if arg
           (orpheus-mpd--find-songs mpd (list "album" (car arg)) callback)
         (orpheus-mpd--list-values mpd "Album" 'albums callback)))
      ('playlists
       (if arg
           (funcall send "listplaylistinfo" (list (orpheus-mpd--quote (car arg)))
                    (lambda (reply)
                      (funcall callback
                               (when (plist-get reply :ok)
                                 (orpheus-mpd--items-from-songs
                                  (plist-get reply :lines))))))
         (funcall send "listplaylists" nil
                  (lambda (reply)
                    (funcall callback
                             (when (plist-get reply :ok)
                               (mapcar
                                (lambda (name)
                                  (orpheus-item-create
                                   :label name :kind 'playlist
                                   :data (list (cons 'browse
                                                     (cons 'playlists
                                                           (list name))))))
                                (orpheus-mpd--strings
                                 (plist-get reply :lines) "playlist"))))))))
      ('search
       (orpheus-mpd--find-songs mpd (list orpheus-mpd-search-tag (car arg))
                                callback "search"))
      ('queue (orpheus-backend-queue mpd callback))
      (_ (funcall callback nil)))))

(defun orpheus-mpd--browse-artist (mpd kind arg callback)
  "Browse KIND on MPD, an artist list or one artist, narrowed by ARG.
The rows are handed to CALLBACK."
  (let ((tag (if (eq kind 'albumartists) "AlbumArtist" "Artist")))
    (cond
     ((null arg)
      (orpheus-mpd--list-values mpd tag kind callback))
     ((= (length arg) 1)
      (orpheus-mpd--send-command
       mpd "list"
       (list "album" (if (eq kind 'albumartists) "albumartist" tag)
             (orpheus-mpd--quote (car arg)))
       (lambda (reply)
         (funcall callback
                  (when (plist-get reply :ok)
                    (mapcar
                     (lambda (album)
                       (orpheus-item-create
                        :label album :kind 'album
                        :data (list (cons 'browse
                                          (cons kind (list (car arg) album))))))
                     (orpheus-mpd--strings (plist-get reply :lines) "Album")))))))
     (t (orpheus-mpd--find-songs
         mpd (list (if (eq kind 'albumartists) "albumartist" "artist")
                   (car arg) "album" (car (last arg)))
         callback)))))

(defun orpheus-mpd--list-values (mpd tag kind callback)
  "List the distinct values of TAG on MPD, then call CALLBACK with KIND rows."
  (orpheus-mpd--send-command
   mpd "list" (list tag)
   (lambda (reply)
     (funcall callback
              (when (plist-get reply :ok)
                (mapcar
                 (lambda (value)
                   (orpheus-item-create
                    :label value :kind kind
                    :data (list (cons 'browse (cons kind (list value)))
                                (cons 'artist value))))
                 (orpheus-mpd--strings (plist-get reply :lines) tag)))))))

(defun orpheus-mpd--find-songs (mpd filters callback &optional command)
  "Find songs on MPD matching FILTERS, a list of tag and value.
CALLBACK receives the rows.  COMMAND is `find', where a tag must match
exactly, or `search', where a value may appear anywhere in the tag."
  (let ((args nil))
    (while filters
      (let ((tag (pop filters))
            (value (pop filters)))
        (setq args (append args (list tag (orpheus-mpd--quote (or value "")))))))
    (orpheus-mpd--send-command
     mpd (or command "find") args
     (lambda (reply)
       (funcall callback
                (when (plist-get reply :ok)
                  (orpheus-mpd--items-from-songs (plist-get reply :lines))))))))

(defun orpheus-mpd--directory-items (lines)
  "Turn the LINES of an `lsinfo' reply into directory and song rows."
  (let ((entries nil)
        (song nil))
    (dolist (line lines)
      (pcase (car line)
        ("directory"
         (when song (push (cons 'song (nreverse song)) entries) (setq song nil))
         (push (cons 'directory (cdr line)) entries))
        ("file"
         (when song (push (cons 'song (nreverse song)) entries))
         (setq song (list line)))
        (_ (when song (push line song)))))
    (when song (push (cons 'song (nreverse song)) entries))
    (mapcar
     (lambda (entry)
       (if (eq (car entry) 'directory)
           (let ((path (cdr entry)))
             (orpheus-item-create
              :label (concat (file-name-nondirectory
                              (directory-file-name path))
                             "/")
              :kind 'directory
              :data (list (cons 'browse (cons 'directories (list path)))
                          (cons 'uri path))))
         (orpheus-mpd--item-from-song (cdr entry))))
     (nreverse entries))))

(cl-defmethod orpheus-backend-outputs ((mpd orpheus-mpd) callback)
  "List MPD's audio outputs, then call CALLBACK with the rows."
  (orpheus-mpd--send-command
   mpd "outputs" nil
   (lambda (reply)
     (funcall callback
              (when (plist-get reply :ok)
                (mapcar
                 (lambda (output)
                   (orpheus-item-create
                    :label (or (alist-get "outputname" output) "output")
                    :detail (if (equal (alist-get "outputenabled" output) "1")
                                "enabled" "disabled")
                    :kind 'output
                    :data (list (cons 'id (alist-get "outputid" output)))))
                 (orpheus-mpd--blocks (plist-get reply :lines) "outputid")))))))

(cl-defmethod orpheus-backend-capabilities ((_mpd orpheus-mpd))
  "Return what MPD can browse."
  '(queue directories artists albumartists albums playlists search))

(provide 'orpheus-mpd)

;;; orpheus-mpd.el ends here
