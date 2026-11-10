;;; orpheus-local.el --- Local files and URLs through mpv -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; The backend that needs no daemon: it plays files and URLs with mpv, which
;;; is one program on Linux, macOS and Windows and already knows how to hand a
;;; YouTube link to yt-dlp.  Emacs talks to it over mpv's JSON IPC socket, so
;;; every command is a small JSON line and no playback call blocks the editor.
;;;
;;; Emacs keeps the queue, mpv plays one item at a time, and the end of a file
;;; is what moves the queue forward.  The audio output is mpv's business:
;;; `orpheus-local--audio-args' asks for PipeWire on Linux (falling back to
;;; PulseAudio and ALSA), CoreAudio on macOS and WASAPI on Windows.
;;;
;;; A shortcut worth naming: because Emacs drives the queue, the gap between
;;; two tracks is the time it takes Emacs to load the next file.  Gapless
;;; playback would mean handing the queue to mpv and reading it back.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'orpheus-backend)
(require 'orpheus-lyrics)

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
  '("cover.jpg" "cover.jpeg" "cover.png" "cover.webp" "cover.gif"
    "front.jpg" "front.jpeg" "front.png" "front.webp"
    "folder.jpg" "folder.jpeg" "folder.png" "folder.webp"
    "album.jpg" "album.jpeg" "album.png" "album.webp"
    "albumart.jpg" "albumart.jpeg" "albumart.png" "albumart.webp"
    "artwork.jpg" "artwork.jpeg" "artwork.png" "artwork.webp"
    "cover art.jpg" "cover art.png" "album art.jpg" "album art.png")
  "File names tried first next to a track or inside an album folder.
Matching is case-insensitive, so `Cover.JPG' matches `cover.jpg', and the
order decides between two names in one folder: `cover.jpg' wins over
`folder.jpg'.  Any other image is used when none of these is there.  The
local and SSH backends share this list."
  :type '(repeat string) :group 'orpheus)

(defcustom orpheus-local-art-extensions
  '("jpg" "jpeg" "png" "webp" "gif" "bmp" "tif" "tiff")
  "Image extensions accepted as cover art.
A file with one of these extensions is a cover even when its name is not in
`orpheus-local-cover-names'; the named files still win when both are in the
same folder.  Matching is case-insensitive, and a leading dot is optional.
The local and SSH backends share this list."
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
  (single nil)
  (volume 100)
  equalizer
  (request-id 0)
  (requests (make-hash-table :test #'eql))
  (load-token 0)
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

(defun orpheus-local--advance (count index repeat random &optional single)
  "Return the queue index after INDEX of COUNT, or nil at the end.
SINGLE plays INDEX again; REPEAT wraps around; RANDOM picks another entry."
  (cond
    ((<= count 0) nil)
    (single index)
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

(cl-defgeneric orpheus-local--resolve-uri (backend uri callback)
  "Call CALLBACK with a path BACKEND's mpv can open for URI.")

(cl-defmethod orpheus-local--resolve-uri ((_local orpheus-local) uri callback)
  "Play URI as it is and call CALLBACK with it."
  (funcall callback uri))

(cl-defgeneric orpheus-local--prefetch (backend)
  "Fetch what BACKEND expects to play next."
  (ignore backend))

(cl-defmethod orpheus-local--prefetch ((_local orpheus-local))
  "A local queue needs no prefetching."
  nil)

(defun orpheus-local--play-index (local index)
  "Play the queue entry at INDEX on LOCAL.
A backend that keeps its files elsewhere can make the entry playable first;
see `orpheus-local--resolve-uri'."
  (let* ((queue (orpheus-local-instance-queue local))
         (uri (nth index queue)))
    (when uri
      (setf (orpheus-local-instance-index local) index)
      (setf (orpheus-local-instance-track local)
            (orpheus-track-create :uri uri
                                  :title (if (string-match-p "\\`[a-z]+://" uri)
                                           uri
                                           (file-name-nondirectory uri))))
      (let ((status (orpheus-local-instance-status local))
            (token (1+ (orpheus-local-instance-load-token local))))
        (setf (orpheus-local-instance-load-token local) token)
        (setf (orpheus-status-songid status) (1+ index))
        (setf (orpheus-status-state status) 'stop)
        (setf (orpheus-status-elapsed status) 0.0)
        (setf (orpheus-status-total status) 0.0)
        (orpheus-local--resolve-uri
         local uri
         (lambda (path)
           (when (= token (orpheus-local-instance-load-token local))
             (if path
                 (let ((paused (eq (orpheus-status-state status) 'pause)))
                   (setf (orpheus-status-state status) (if paused 'pause 'play))
                   (orpheus-local--send local (list "loadfile" path "replace"))
                   (orpheus-local--send
                    local (list "set_property" "pause"
                                (if paused :true :false)))
                   (orpheus-local--prefetch local))
               (setf (orpheus-status-state status) 'stop)
               (message "orpheus: cannot play %s" uri)))))))))

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
                               (orpheus-local-instance-random local)
                               (orpheus-local-instance-single local))))
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
        ;; An equalizer chosen before mpv started is applied before the first
        ;; loadfile, so playback never begins flat and then jump.
        (when (orpheus-local-instance-equalizer local)
          (orpheus-local--send
           local (list "set_property" "af"
                       (orpheus-local-instance-equalizer local))))
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
    (setf (orpheus-local-instance-load-token local)
          (1+ (orpheus-local-instance-load-token local)))
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

(defun orpheus-local--art-extension-p (name)
  "Return non-nil when NAME ends in one of `orpheus-local-art-extensions'."
  (let ((extension (file-name-extension name)))
    (and extension
         (member (downcase extension)
                 (mapcar (lambda (candidate)
                           (downcase (string-remove-prefix "." candidate)))
                         orpheus-local-art-extensions)))))

(defun orpheus-local--art-name-index (path)
  "Return PATH's place in the artwork order, or nil when it is not an image.
Names in `orpheus-local-cover-names' come first, in that list's order, so
`cover.jpg' wins over an arbitrary image.  Any other image ranks after them."
  (let ((name (downcase (file-name-nondirectory path))))
    (cond
     ((cl-position name
                   (mapcar (lambda (candidate)
                             (downcase (file-name-nondirectory candidate)))
                           orpheus-local-cover-names)
                   :test #'equal))
     ((orpheus-local--art-extension-p name) 1000))))

(defun orpheus-local--art-rank (dir path)
  "Return PATH's place in DIR's artwork search.
0 is DIR itself, 1 its parent and 2 a child folder of DIR.  Nil means PATH is
elsewhere, such as the cover of a sibling album, and must not be shown."
  (let* ((dir (file-name-as-directory dir))
         (parent (or (file-name-directory (directory-file-name dir)) dir))
         (folder (file-name-directory path)))
    (cond
     ((equal folder dir) 0)
     ((equal folder parent) 1)
     ((and (not (equal parent dir)) (string-prefix-p dir path)) 2)
     (t nil))))

(defun orpheus-local--art-best (dir paths)
  "Return the best cover for DIR among PATHS, or nil.
DIR itself wins over its parent, which wins over a child folder.  Within a
folder, a name from `orpheus-local-cover-names' wins over any other image, and
that list's order decides among the names."
  (let ((candidates
         (delq nil
               (mapcar
                (lambda (path)
                  (when-let* ((index (orpheus-local--art-name-index path))
                              (rank (orpheus-local--art-rank dir path)))
                    (list rank index path)))
                paths))))
    (nth 2 (car (sort candidates
                      (lambda (a b)
                        (cond ((/= (nth 0 a) (nth 0 b))
                               (< (nth 0 a) (nth 0 b)))
                              ((/= (nth 1 a) (nth 1 b))
                               (< (nth 1 a) (nth 1 b)))
                              (t (string< (nth 2 a) (nth 2 b))))))))))

(defun orpheus-local--cover-file (dir)
  "Return DIR's best cover image, or nil.
A name from `orpheus-local-cover-names' wins over another image, and that
list's order decides among the names."
  (when (and dir (file-directory-p dir))
    (orpheus-local--art-best
     dir (cl-remove-if-not #'file-regular-p
                           (directory-files dir t "\\`[^.]")))))

(defun orpheus-local--cover-near (dir)
  "Return a cover for DIR from itself, its parent or a child folder.
This is the synchronous fallback for `orpheus-local--scan-art', used when
find is not installed."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir "~"))))
         (parent (file-name-directory (directory-file-name dir)))
         (children (condition-case nil
                       (directory-files dir t "\\`[^.]")
                     (error nil))))
    (or (orpheus-local--cover-file dir)
        (unless (or (null parent) (equal parent dir))
          (orpheus-local--cover-file parent))
        (cl-loop for child in children
                 when (file-directory-p child)
                 thereis (orpheus-local--cover-file
                          (file-name-as-directory child))))))

(defun orpheus-local--art-roots (dir)
  "Return the folders scanned for art around DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (parent (file-name-directory (directory-file-name dir))))
    (if (or (null parent) (equal parent dir))
        (list dir)
      (list dir parent))))

(defun orpheus-local--art-name-args ()
  "Return the find predicates that match an image file name."
  (let ((extensions
         (delete-dups
          (mapcar (lambda (extension)
                    (downcase (string-remove-prefix "." extension)))
                  orpheus-local-art-extensions))))
    (append
     '("(")
     (cl-loop for extension in extensions
              for first = t then nil
              append (if first
                         (list "-iname" (concat "*." extension))
                       (list "-o" "-iname" (concat "*." extension))))
     '(")"))))

(defun orpheus-local--scan-art (dir callback)
  "Find cover art for DIR without blocking, then call CALLBACK with a path.
DIR, its parent and one level of its child folders are searched by find, so an
album root or a Scans folder is found from a disc folder.  Nothing is read on
the main thread; when find is missing the fallback reads the same folders."
  (let ((program (executable-find "find"))
        (buffer (generate-new-buffer " *orpheus-art*")))
    (if (null program)
        (progn
          (kill-buffer buffer)
          (funcall callback (orpheus-local--cover-near dir)))
      (condition-case nil
          (make-process
           :name "orpheus-art"
           :command (append (list program)
                            (orpheus-local--art-roots dir)
                            (list "-maxdepth" "2" "-type" "f")
                            (orpheus-local--art-name-args)
                            (list "-print"))
           :buffer buffer
           :stderr (get-buffer-create "*orpheus find*")
           :noquery t
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((output
                      (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process))
                           (with-current-buffer buffer (buffer-string)))))
                 (when (buffer-live-p buffer) (kill-buffer buffer))
                 (funcall callback
                          (or (orpheus-local--art-best
                               dir (split-string (or output "") "\n" t))
                              (orpheus-local--cover-near dir)))))))
        (error
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (funcall callback (orpheus-local--cover-near dir)))))))

(defun orpheus-local--album-art-file (file callback)
  "Find cover art near FILE, then call CALLBACK with a path or nil.
Embedded art is extracted when no cover file is found anywhere near it."
  (let* ((dir (cond ((and file (file-directory-p file)) file)
                    ((and file (file-regular-p file))
                     (file-name-directory file)))))
    (if dir
        (orpheus-local--scan-art
         dir
         (lambda (found)
           (if (or found (null file) (not (file-regular-p file)))
               (funcall callback found)
             (orpheus-local--extract-art file callback))))
      (funcall callback nil))))

(cl-defmethod orpheus-backend-album-art ((_local orpheus-local) track callback)
  "Find cover art for TRACK next to the file or inside it, then call CALLBACK."
  (orpheus-local--album-art-file (orpheus-track-uri track) callback))

(cl-defmethod orpheus-backend-lyrics ((_local orpheus-local) track callback)
  "Read TRACK's sidecar or embedded lyrics, then call CALLBACK with them."
  (funcall callback (orpheus-lyrics--from-file (orpheus-track-uri track))))

(cl-defmethod orpheus-backend-folder-art ((_local orpheus-local) folder callback)
  "Find cover art for FOLDER, then call CALLBACK with a path or nil."
  (if (and folder (file-directory-p folder))
      (orpheus-local--scan-art folder callback)
    (funcall callback nil)))

(cl-defmethod orpheus-backend-local-file ((_local orpheus-local) track)
  "Return TRACK's own file, which mpv can play as it is."
  (let ((uri (orpheus-track-uri track)))
    (and uri (file-regular-p uri) uri)))

;;; Browsing files

(defun orpheus-local--media-file-p (path)
  "Return non-nil when PATH looks like something mpv can play."
  (and (file-regular-p path)
       (member (downcase (or (file-name-extension path) ""))
               orpheus-local-extensions)))

(defun orpheus-local--search-items (dir query output)
  "Return tracks below DIR from find OUTPUT that match QUERY."
  (let ((needle (downcase query))
        (root (file-name-as-directory (expand-file-name dir))))
    (delq nil
          (mapcar
           (lambda (path)
             (when (and (orpheus-local--media-file-p path)
                        (string-match-p (regexp-quote needle)
                                        (downcase path)))
               (orpheus-item-create
                :label (if (string-prefix-p root path)
                           (substring path (length root))
                         path)
                :kind 'track
                :detail (upcase (or (file-name-extension path) "file"))
                :data (list (cons 'uri path)))))
           (split-string output "\n" t)))))

(defun orpheus-local--search (query callback)
  "Find local files matching QUERY, then call CALLBACK with the rows."
  (let* ((dir (expand-file-name (or orpheus-local-directory "~")))
         (buffer (generate-new-buffer " *orpheus-find*"))
         (program (or (executable-find "find") "find")))
    (condition-case err
        (make-process
         :name "orpheus-find"
         :command (list program dir "-type" "f" "-print")
         :buffer buffer
         :stderr (get-buffer-create "*orpheus find*")
         :noquery t
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((output (and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process))
                                (with-current-buffer buffer (buffer-string)))))
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (funcall callback
                        (and output
                             (orpheus-local--search-items dir query output)))))))
      (error
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (message "orpheus: cannot search %s: %s" dir (error-message-string err))
       (funcall callback nil)))))

(defun orpheus-local--directory-items (dir)
  "Return the rows of DIR, one per directory and playable file.
The row that leaves DIR is added by the interface, as it is for every
backend, so a parent row here would show the way up twice."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir "~"))))
         (entries (condition-case nil
                    (directory-files dir t "\\`[^.]")
                    (error nil))))
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
           entries))))

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
  (let* ((uris (if (listp uris) uris (list uris))))
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
            (setf (orpheus-local-instance-load-token local)
                  (1+ (orpheus-local-instance-load-token local)))
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
           ('single
            (setf (orpheus-local-instance-single local) (and value t)
                  (orpheus-status-single
                   (orpheus-local-instance-status local))
                  (and value t)))
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
            (setf (orpheus-local-instance-load-token local)
                  (1+ (orpheus-local-instance-load-token local)))
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

(cl-defmethod orpheus-backend-equalizer ((local orpheus-local) filters)
  "Set LOCAL's mpv audio filter chain to FILTERS.
The filter is kept even while mpv is stopped, and applied when it starts."
  (setf (orpheus-local-instance-equalizer local) filters)
  (when (orpheus-local--ready-p local)
    (orpheus-local--send local (list "set_property" "af" filters)))
  t)

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
         ('search (orpheus-local--search (car arg) callback))
         (_ (funcall callback nil))))

(cl-defmethod orpheus-backend-portable-uri ((_local orpheus-local) uri)
  "Return URI relative to `orpheus-local-directory' when it is below it."
  (or (orpheus-backend--relative-uri uri orpheus-local-directory) uri))

(cl-defmethod orpheus-backend-playlist-uri ((_local orpheus-local) uri)
  "Return URI as a path below `orpheus-local-directory'."
  (cond ((null uri) nil)
        ((orpheus-backend--absolute-uri-p uri) uri)
        (t (expand-file-name uri orpheus-local-directory))))

(cl-defmethod orpheus-backend-capabilities ((_local orpheus-local))
  "Return what mpv can browse.
`paths' is not a tab: it says this backend takes file names and URLs, which
is what `orpheus-play-file' and `orpheus-play-url' look for."
  '(queue directories search paths))

(provide 'orpheus-local)

;;; orpheus-local.el ends here
