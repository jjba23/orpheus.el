;;; orpheus-local.el --- Local files and URLs through mpv -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The backend that needs no daemon: it plays files and URLs with mpv, which
;; is one program on Linux, macOS and Windows and already knows how to hand a
;; YouTube link to yt-dlp.  Emacs talks to it over mpv's JSON IPC socket, so
;; every command is a small JSON line and no playback call blocks the editor.
;;
;; Emacs keeps the queue, mpv plays one item at a time, and the end of a file
;; is what moves the queue forward.  The audio output is mpv's business:
;; `orpheus-local--audio-args' asks for PipeWire on Linux (falling back to
;; PulseAudio and ALSA), CoreAudio on macOS and WASAPI on Windows.
;;
;; A shortcut worth naming: because Emacs drives the queue, the gap between
;; two tracks is the time it takes Emacs to load the next file.  Gapless
;; playback would mean handing the queue to mpv and reading it back.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'orpheus-backend)


;;; Settings

(defcustom orpheus-local-directory "~/Music"
  "Directory the files pane starts in."
  :type 'directory :group 'orpheus)

(defcustom orpheus-local-mpv-program "mpv"
  "Program name or path of mpv."
  :type 'string :group 'orpheus)

(defcustom orpheus-local-audio-driver nil
  "Audio output driver for mpv, or nil to pick one for the platform.
The value is what follows `--ao=', for example \"pipewire\" or
\"coreaudio/1\"."
  :type '(choice (const :tag "Choose for this system" nil) string)
  :group 'orpheus)

(defcustom orpheus-local-extra-args nil
  "Extra command line arguments passed to mpv."
  :type '(repeat string) :group 'orpheus)

(defcustom orpheus-local-socket nil
  "IPC socket for mpv, or nil to make a temporary one."
  :type '(choice (const :tag "Temporary socket" nil) file)
  :group 'orpheus)

(defcustom orpheus-local-cover-names
  '("cover.jpg" "cover.png" "front.jpg" "front.png" "folder.jpg"
    "folder.png" "album.jpg" "albumart.jpg" "artwork.jpg")
  "File names looked for next to a track before extracting art."
  :type '(repeat string) :group 'orpheus)

(defcustom orpheus-local-extensions
  '("mp3" "flac" "ogg" "oga" "opus" "m4a" "m4b" "aac" "wav" "aiff" "aif"
    "wma" "mp4" "m4v" "mkv" "webm" "avi" "mov" "mpg" "mpeg" "m3u" "m3u8")
  "File extensions the files pane shows."
  :type '(repeat string) :group 'orpheus)


;;; The backend

(cl-defstruct (orpheus-local (:include orpheus-backend)
                             (:constructor make-orpheus-local)
                             (:conc-name orpheus-local-instance-))
  "The mpv backend, with the queue Emacs keeps for it."
  process
  ipc
  socket
  (accum "")
  (queue nil)
  (index 0)
  (status (orpheus-status-create))
  track
  (repeat nil)
  (random nil)
  (volume 100)
  (request-id 0)
  (requests (make-hash-table :test #'eql))
  (connecting nil)
  (waiting nil)
  (attempts 0))

(defun orpheus-local--make ()
  "Return a new mpv backend."
  (make-orpheus-local :name 'local :volume 100))

(orpheus-register-backend 'local #'orpheus-local--make
                          "Local files and URLs played by mpv")


;;; Platform

(defun orpheus-local--audio-args ()
  "Return mpv's audio output arguments for this system.
PipeWire is asked for first on Linux and mpv falls back by itself, which is
what makes the same default work on a desktop, a laptop and a headless box."
  (list (concat "--ao="
                (or orpheus-local-audio-driver
                    (pcase system-type
                      ('gnu/linux "pipewire,pulse,alsa")
                      ('darwin "coreaudio")
                      ('windows-nt "wasapi")
                      (_ "pulse,alsa"))))))

(defun orpheus-local--socket-path ()
  "Return the path of the IPC socket to make mpv listen on."
  (or (when orpheus-local-socket
        (expand-file-name orpheus-local-socket))
      (if (eq system-type 'windows-nt)
          ;; Windows has no unix sockets; mpv takes a named pipe.
          "\\\\.\\pipe\\orpheus-emacs"
        (expand-file-name (format "orpheus-mpv-%d" (emacs-pid))
                          temporary-file-directory))))


;;; The queue

(defun orpheus-local--advance (count index repeat random)
  "Return the queue index after INDEX of COUNT, or nil at the end.
REPEAT wraps around; RANDOM picks another entry."
  (cond
   ((<= count 0) nil)
   ((>= index (1- count)) (when repeat 0))
   (random (let ((candidate (random count)))
             (if (= candidate index) (mod (1+ index) count) candidate)))
   (t (1+ index))))

(defun orpheus-local--queue-items (local)
  "Return LOCAL's queue as rows."
  (let ((index 0))
    (mapcar
     (lambda (uri)
       (prog1
           (orpheus-item-create
            :label (format "%2d  %s" (1+ index)
                           (if (string-match-p "\\`[a-z]+://" uri)
                               uri
                             (file-name-nondirectory uri)))
            :detail (if (string-match-p "\\`[a-z]+://" uri)
                        "stream"
                      (file-name-nondirectory
                       (directory-file-name (file-name-directory uri))))
            :kind 'track
            :data (list (cons 'uri uri) (cons 'pos index)
                        (cons 'id (1+ index))))
         (setq index (1+ index))))
     (orpheus-local-instance-queue local))))

(defun orpheus-local--play-index (local index)
  "Play the queue entry at INDEX on LOCAL."
  (let* ((queue (orpheus-local-instance-queue local))
         (uri (nth index queue)))
    (when uri
      (setf (orpheus-local-instance-index local) index)
      (setf (orpheus-local-instance-track local)
            (orpheus-track-create :uri uri
                                  :title (if (string-match-p "\\`[a-z]+://" uri)
                                             uri
                                           (file-name-nondirectory uri))))
      (let ((status (orpheus-local-instance-status local)))
        (setf (orpheus-status-songid status) (1+ index))
        (setf (orpheus-status-state status) 'play)
        (setf (orpheus-status-elapsed status) 0.0)
        (setf (orpheus-status-total status) 0.0))
      (orpheus-local--send local (list "loadfile" uri "replace"))
      (orpheus-local--send local (list "set_property" "pause" :false)))))


;;; Talking mpv

(defun orpheus-local--request-json (id command)
  "Return the JSON line for a request numbered ID carrying COMMAND."
  (concat (json-serialize (list :command (vconcat command) :request_id id))
          "\n"))

(defun orpheus-local--write (local line)
  "Send LINE to LOCAL's mpv."
  (let ((ipc (orpheus-local-instance-ipc local)))
    (when (process-live-p ipc)
      (process-send-string ipc line))))

(defun orpheus-local--send (local command &optional callback)
  "Send COMMAND to LOCAL's mpv, then call CALLBACK with what came back.
CALLBACK is called with the error string, or nil, and the data."
  (let ((id (1+ (orpheus-local-instance-request-id local))))
    (setf (orpheus-local-instance-request-id local) id)
    (when callback
      (puthash id callback (orpheus-local-instance-requests local)))
    (orpheus-local--write local (orpheus-local--request-json id command))))

(defun orpheus-local--dispatch (local message)
  "Handle one parsed MESSAGE from LOCAL's mpv."
  (let ((event (alist-get 'event message))
        (request (alist-get 'request_id message)))
    (cond
     (event
      (pcase event
        ("end-file"
         (when (equal (alist-get 'reason message) "eof")
           (let ((next (orpheus-local--advance
                        (length (orpheus-local-instance-queue local))
                        (orpheus-local-instance-index local)
                        (orpheus-local-instance-repeat local)
                        (orpheus-local-instance-random local))))
             (if next
                 (orpheus-local--play-index local next)
               (setf (orpheus-status-state
                      (orpheus-local-instance-status local))
                     'stop)))))
        ("file-loaded" (orpheus-local--poll-metadata local))))
     (request
      (let ((callback (gethash request
                               (orpheus-local-instance-requests local))))
        (when callback
          (remhash request (orpheus-local-instance-requests local))
          (let ((error (alist-get 'error message)))
            (funcall callback
                     (unless (equal error "success") error)
                     (alist-get 'data message)))))))))

(defun orpheus-local--filter (local chunk)
  "Feed CHUNK, as it arrives, to LOCAL's JSON parser."
  (setf (orpheus-local-instance-accum local)
        (concat (orpheus-local-instance-accum local) chunk))
  (while (string-match "\n" (orpheus-local-instance-accum local))
    (let* ((accum (orpheus-local-instance-accum local))
           (line (substring accum 0 (match-beginning 0))))
      (setf (orpheus-local-instance-accum local)
            (substring accum (match-end 0)))
      (unless (string-empty-p line)
        (condition-case err
            (orpheus-local--dispatch
             local (json-parse-string line :object-type 'alist
                                      :null-object nil
                                      :false-object nil))
          (error (message "orpheus: mpv sent something unreadable: %S" err)))))))


;;; Reading mpv's properties

(defun orpheus-local--set-field (local field error data)
  "Store DATA, one mpv property of FIELD, into LOCAL after ERROR."
  (unless error
    (let ((status (orpheus-local-instance-status local)))
      (pcase field
        ('elapsed (setf (orpheus-status-elapsed status) (or data 0.0)))
        ('total (setf (orpheus-status-total status) (or data 0.0)))
        ('pause (setf (orpheus-status-state status) (if data 'pause 'play)))
        ('volume (setf (orpheus-status-volume status)
                       (round (or data 0))))))))

(defun orpheus-local--poll-field (local property field)
  "Ask LOCAL's mpv for PROPERTY and store it in FIELD."
  (orpheus-local--send local (list "get_property" property)
                       (lambda (error data)
                         (orpheus-local--set-field local field error data))))

(defun orpheus-local--set-metadata (local error metadata)
  "Store METADATA, a tag alist, into LOCAL's track after ERROR."
  (unless error
    (let ((track (orpheus-local-instance-track local)))
      (when track
        (when-let* ((title (alist-get 'title metadata)))
          (setf (orpheus-track-title track) title))
        (when-let* ((artist (alist-get 'artist metadata)))
          (setf (orpheus-track-artist track) artist))
        (when-let* ((album (alist-get 'album metadata)))
          (setf (orpheus-track-album track) album))
        (when-let* ((genre (alist-get 'genre metadata)))
          (setf (orpheus-track-genre track) genre))
        (when-let* ((year (alist-get 'date metadata)))
          (setf (orpheus-track-year track) year))
        (when-let* ((trackno (alist-get 'track metadata)))
          (setf (orpheus-track-trackno track)
                (string-to-number (if (listp trackno) (car trackno) trackno))))))))

(defun orpheus-local--poll-metadata (local)
  "Refresh LOCAL's track tags and duration."
  (orpheus-local--send local (list "get_property" "metadata")
                       (lambda (error data)
                         (orpheus-local--set-metadata local error data)))
  (orpheus-local--poll-field local "duration" 'total))

(defun orpheus-local--poll (local)
  "Refresh LOCAL's status from mpv."
  (orpheus-local--poll-field local "time-pos" 'elapsed)
  (orpheus-local--poll-field local "duration" 'total)
  (orpheus-local--poll-field local "pause" 'pause)
  (orpheus-local--poll-field local "volume" 'volume)
  (orpheus-local--poll-metadata local)
  ;; Nothing loaded means nothing playing, however long mpv stays idle.
  (orpheus-local--send local (list "get_property" "idle-active")
                       (lambda (_error data)
                         (when data
                           (setf (orpheus-status-state
                                  (orpheus-local-instance-status local))
                                 'stop)))))


;;; Starting mpv

(defun orpheus-local--ready-p (local)
  "Return non-nil when LOCAL's mpv is listening."
  (process-live-p (orpheus-local-instance-ipc local)))

(defun orpheus-local--spawn (local)
  "Start mpv for LOCAL and connect to its IPC socket."
  (orpheus-local--kill local)
  (let* ((socket (orpheus-local--socket-path))
         (program (or (executable-find orpheus-local-mpv-program)
                      orpheus-local-mpv-program))
         (command (append (list program
                                "--idle=yes"
                                "--no-video"
                                "--force-window=no"
                                "--really-quiet"
                                (concat "--volume="
                                        (number-to-string
                                         (orpheus-local-instance-volume local)))
                                (concat "--input-ipc-server=" socket))
                          (orpheus-local--audio-args)
                          orpheus-local-extra-args)))
    (setf (orpheus-local-instance-socket local) socket)
    (when (eq system-type 'windows-nt) (ignore-errors (delete-file socket)))
    (setf (orpheus-local-instance-process local)
          (condition-case err
              (make-process :name "orpheus-mpv" :command command
                            :buffer nil :noquery t
                            ;; mpv's own complaints stay readable here.
                            :stderr (get-buffer-create "*orpheus mpv*")
                            :sentinel #'orpheus-local--process-sentinel)
            (error (message "orpheus: cannot start mpv: %s"
                            (error-message-string err))
                   nil)))
    (setf (orpheus-local-instance-attempts local) 0)
    (orpheus-local--try-connect local)))

(defun orpheus-local--process-sentinel (process event)
  "Notice mpv, PROCESS, going away because of EVENT."
  (unless (process-live-p process)
    (message "orpheus: mpv stopped (%s); see *orpheus mpv*"
             (string-trim event))))

(defun orpheus-local--try-connect (local)
  "Try to connect to LOCAL's mpv, retrying while it starts up."
  (let* ((socket (orpheus-local-instance-socket local))
         (ipc (when (and socket
                         (or (not (eq system-type 'windows-nt))
                             (file-exists-p socket)
                             (process-live-p
                              (orpheus-local-instance-process local))))
                (condition-case nil
                    (make-network-process
                     :name "orpheus-mpv-ipc"
                     :family 'local
                     :service socket
                     :coding 'utf-8-unix
                     :noquery t
                     :filter (lambda (_process chunk)
                               (orpheus-local--filter local chunk))
                     :sentinel (lambda (_process event)
                                 (orpheus-local--ipc-sentinel local event)))
                  ;; The socket is not there yet: mpv is still starting.
                  (error nil)))))
    (if ipc
        (progn
          (setf (orpheus-local-instance-ipc local) ipc
                (orpheus-local-instance-connecting local) nil
                (orpheus-local-instance-attempts local) 0)
          (let ((waiting (orpheus-local-instance-waiting local)))
            (setf (orpheus-local-instance-waiting local) nil)
            (dolist (callback waiting)
              (ignore-errors (funcall callback local))))
          (when (orpheus-local-instance-queue local)
            (orpheus-local--poll local)))
      (let ((attempts (1+ (orpheus-local-instance-attempts local))))
        (setf (orpheus-local-instance-attempts local) attempts)
        (if (> attempts 100)
            (progn
              (setf (orpheus-local-instance-connecting local) nil)
              (message "orpheus: mpv never opened its IPC socket")
              (let ((waiting (orpheus-local-instance-waiting local)))
                (setf (orpheus-local-instance-waiting local) nil)
                (dolist (callback waiting)
                  (ignore-errors (funcall callback nil)))))
          (run-with-timer 0.05 nil #'orpheus-local--try-connect local))))))

(defun orpheus-local--ipc-sentinel (local event)
  "Forget LOCAL's socket when the connection dies, as EVENT says."
  (unless (process-live-p (orpheus-local-instance-ipc local))
    (setf (orpheus-local-instance-ipc local) nil)
    (run-hook-with-args 'orpheus-backend-disconnected-hook local event)))

(defun orpheus-local--ensure (local callback)
  "Make sure LOCAL is running, then call CALLBACK with it, or nil."
  (cond
   ((orpheus-local--ready-p local) (funcall callback local))
   ((orpheus-local-instance-connecting local)
    (setf (orpheus-local-instance-waiting local)
          (append (orpheus-local-instance-waiting local) (list callback))))
   (t
    (setf (orpheus-local-instance-connecting local) t)
    (setf (orpheus-local-instance-waiting local) (list callback))
    (orpheus-local--spawn local))))

(defun orpheus-local--kill (local)
  "Stop LOCAL's mpv and forget the queue's playback."
  (let ((ipc (orpheus-local-instance-ipc local))
        (process (orpheus-local-instance-process local)))
    (setf (orpheus-local-instance-ipc local) nil)
    (setf (orpheus-local-instance-process local) nil)
    (setf (orpheus-local-instance-connecting local) nil)
    (when (process-live-p ipc) (ignore-errors (delete-process ipc)))
    (when (process-live-p process) (ignore-errors (delete-process process)))))


;;; Album art

(defun orpheus-local--extract-art (uri callback)
  "Extract URI's embedded cover with ffmpeg, then call CALLBACK with a path."
  (let ((ffmpeg (executable-find "ffmpeg"))
        (output (orpheus-cache-file uri "cover.jpg")))
    (cond
     ((file-readable-p output) (funcall callback output))
     ((null ffmpeg) (funcall callback nil))
     (t
      (make-process
       :name "orpheus-cover"
       :command (list ffmpeg "-v" "error" "-y" "-i" uri
                      "-map" "0:v" "-frames:v" "1" "-f" "image2" output)
       :buffer nil :noquery t
       :sentinel (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (funcall callback
                              (and (file-readable-p output) output)))))))))

(cl-defmethod orpheus-backend-album-art ((_local orpheus-local) track callback)
  "Find cover art for TRACK next to the file or inside it, then call CALLBACK."
  (let* ((uri (orpheus-track-uri track))
         (covers (when (and uri (file-regular-p uri))
                   (mapcar (lambda (name)
                             (expand-file-name name (file-name-directory uri)))
                           orpheus-local-cover-names)))
         (found (cl-find-if #'file-readable-p covers)))
    (cond
     (found (funcall callback found))
     ((and uri (file-regular-p uri))
      (orpheus-local--extract-art uri callback))
     (t (funcall callback nil)))))


;;; Browsing files

(defun orpheus-local--media-file-p (path)
  "Return non-nil when PATH looks like something mpv can play."
  (and (file-regular-p path)
       (member (downcase (or (file-name-extension path) ""))
               orpheus-local-extensions)))

(defun orpheus-local--directory-items (dir)
  "Return the rows of DIR, one per directory and playable file."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir "~"))))
         (parent (file-name-directory (directory-file-name dir)))
         (entries (condition-case nil
                      (directory-files dir t "\\`[^.]")
                    (error nil))))
    (append
     (when (and parent (not (equal parent dir)) (file-directory-p parent))
       (list (orpheus-item-create
              :label "../" :kind 'directory
              :data (list (cons 'browse (cons 'directories (list parent)))))))
     (delq nil
           (mapcar
            (lambda (path)
              (cond
               ((file-directory-p path)
                (orpheus-item-create
                 :label (concat (file-name-nondirectory
                                 (directory-file-name path))
                                "/")
                 :kind 'directory
                 :data (list (cons 'browse (cons 'directories (list path)))
                             (cons 'uri path))))
               ((orpheus-local--media-file-p path)
                (orpheus-item-create
                 :label (file-name-nondirectory path)
                 :detail (file-size-human-readable
                          (or (file-attribute-size (file-attributes path)) 0))
                 :kind 'track
                 :data (list (cons 'uri path))))))
            entries)))))


;;; Backend methods

(cl-defmethod orpheus-backend-connected-p ((local orpheus-local))
  "Return non-nil when LOCAL's mpv is listening."
  (orpheus-local--ready-p local))

(cl-defmethod orpheus-backend-connect ((local orpheus-local) callback)
  "Start mpv, then call CALLBACK with LOCAL."
  (orpheus-local--ensure local callback))

(cl-defmethod orpheus-backend-disconnect ((local orpheus-local))
  "Stop LOCAL's mpv."
  (orpheus-local--kill local))

(cl-defmethod orpheus-backend-status ((local orpheus-local) callback)
  "Ask LOCAL's mpv for the current status, then call CALLBACK with it."
  (orpheus-local--ensure
   local
   (lambda (running)
     (when running (orpheus-local--poll local))
     (funcall callback (and running
                            (orpheus-local-instance-status local))))))

(cl-defmethod orpheus-backend-track ((local orpheus-local) callback)
  "Return LOCAL's current track to CALLBACK."
  (funcall callback (orpheus-local-instance-track local)))

(cl-defmethod orpheus-backend-queue ((local orpheus-local) callback)
  "Return LOCAL's queue to CALLBACK."
  (funcall callback (orpheus-local--queue-items local)))

(cl-defmethod orpheus-backend-add ((local orpheus-local) uris)
  "Append URIS to LOCAL's queue and start playing when idle."
  (let ((uris (if (listp uris) uris (list uris))))
    (setf (orpheus-local-instance-queue local)
          (append (orpheus-local-instance-queue local) uris))
    (orpheus-local--ensure
     local
     (lambda (running)
       (when (and running
                  (memq (orpheus-status-state
                         (orpheus-local-instance-status local))
                        '(stop))
                  (null (orpheus-local-instance-track local)))
         (orpheus-local--play-index
          local (1- (length (orpheus-local-instance-queue local)))))))))

(cl-defmethod orpheus-backend-play-uri ((local orpheus-local) uri)
  "Queue URI on LOCAL and play it now."
  (let* ((queue (orpheus-local-instance-queue local))
         (index (or (cl-position uri queue :test #'equal)
                    (progn (setf (orpheus-local-instance-queue local)
                                 (append queue (list uri)))
                           (length queue)))))
    (orpheus-local--ensure
     local
     (lambda (running)
       (when running (orpheus-local--play-index local index))))))

(cl-defmethod orpheus-backend-command ((local orpheus-local) action &optional value)
  "Perform ACTION on LOCAL, with VALUE when it takes one."
  (let ((send (lambda (command &optional args)
                (orpheus-local--send local (append (list command) args)))))
    (pcase action
      ('play
       (if (orpheus-local-instance-track local)
           (funcall send "set_property" (list "pause" :false))
         (orpheus-local--play-index local (orpheus-local-instance-index local))))
      ('playpos (orpheus-local--play-index local (or value 0)))
      ('pause (funcall send "set_property" (list "pause" :true)))
      ('resume (funcall send "set_property" (list "pause" :false)))
      ('toggle
       (setf (orpheus-status-state (orpheus-local-instance-status local))
             (if (eq (orpheus-status-state
                      (orpheus-local-instance-status local))
                     'pause)
                 'play 'pause))
       (funcall send "cycle" '("pause")))
      ('stop
       (funcall send "stop")
       (setf (orpheus-status-state (orpheus-local-instance-status local))
             'stop))
      ('next
       (let ((next (orpheus-local--advance
                    (length (orpheus-local-instance-queue local))
                    (orpheus-local-instance-index local) t nil)))
         (if next (orpheus-local--play-index local next)
           (message "orpheus: no next track"))))
      ('previous
       (let ((index (1- (orpheus-local-instance-index local))))
         (orpheus-local--play-index local (if (< index 0) 0 index))))
      ('seek (funcall send "seek" (list (or value 0))))
      ('volume
       (setf (orpheus-local-instance-volume local)
             (max 0 (min 100 (round value))))
       (funcall send "set_property"
                (list "volume" (orpheus-local-instance-volume local))))
      ('repeat (setf (orpheus-local-instance-repeat local) (and value t)))
      ('random (setf (orpheus-local-instance-random local) (and value t)))
      ('delete
       (let* ((queue (orpheus-local-instance-queue local))
              (pos (or value 0))
              (rest (append (cl-subseq queue 0 (min pos (length queue)))
                            (nthcdr (1+ pos) queue))))
         (setf (orpheus-local-instance-queue local) rest)
         (when (and (>= (orpheus-local-instance-index local) (length rest))
                    rest)
           (setf (orpheus-local-instance-index local) 0))))
      ('clear
       (setf (orpheus-local-instance-queue local) nil
             (orpheus-local-instance-index local) 0
             (orpheus-local-instance-track local) nil)
       (funcall send "stop"))
      ('shuffle
       (setf (orpheus-local-instance-queue local)
             (orpheus-local--shuffle (orpheus-local-instance-queue local))))
      ((or 'single 'consume)
       (message "orpheus: mpv does not do %s; MPD does" action))
      ((or 'update 'rescan 'save 'playid 'load)
       (message "orpheus: mpv has no library to %s" action))
      (_ (message "orpheus: mpv cannot %s" action)))))

(defun orpheus-local--shuffle (list)
  "Return LIST in random order."
  (let ((copy (copy-sequence list)))
    (cl-loop for i from (1- (length copy)) downto 1
             do (cl-rotatef (nth i copy) (nth (random (1+ i)) copy)))
    copy))

(cl-defmethod orpheus-backend-browse ((local orpheus-local) kind arg callback)
  "Browse KIND on LOCAL, with ARG as the path down the hierarchy."
  (pcase kind
    ('queue (funcall callback (orpheus-local--queue-items local)))
    ('directories
     (funcall callback
              (orpheus-local--directory-items (or (car arg)
                                                 orpheus-local-directory))))
    (_ (funcall callback nil))))

(cl-defmethod orpheus-backend-capabilities ((_local orpheus-local))
  "Return what mpv can browse.
`paths' is not a tab: it says this backend takes file names and URLs, which
is what `orpheus-play-file' and `orpheus-play-url' look for."
  '(queue directories paths))

(provide 'orpheus-local)

;;; orpheus-local.el ends here
