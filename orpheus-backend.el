;;; orpheus-backend.el --- Backend protocol for orpheus -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; This file holds what every backend shares: the values that travel through
;;; the player (a track, a status, a browsable item), the protocol a backend
;;; implements, and the registry that maps a symbol to a backend constructor.
;;;
;;; A backend is a `cl-defstruct' that includes `orpheus-backend' and
;;; implements the generics below.  Nothing here talks to the outside world, so
;;; the whole file is pure and can be tested without a server.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; The shared group

(defgroup orpheus nil
  "A music player for Emacs with pluggable backends."
  :group 'multimedia
  :prefix "orpheus-")

(defvar orpheus-backend-current nil
  "The backend in use, or nil before the first `orpheus-connect'.")

(defvar orpheus-status-current nil
  "The last status a backend reported, as an `orpheus-status'.")

(defvar orpheus-track-current nil
  "The last track a backend reported, as an `orpheus-track'.")

(defvar orpheus-backend-disconnected-hook nil
  "Hook run when a backend loses its connection.
Each function is called with the backend and a description of the event.")

;;; Values

(cl-defstruct (orpheus-track (:constructor orpheus-track-create))
  "One playable thing: a local file, a stream or a daemon-side song."
  uri title artist album albumartist duration trackno year genre)

(cl-defstruct (orpheus-status (:constructor orpheus-status-create))
  "What a backend says about playback right now."
  (state 'stop)
  (elapsed 0.0)
  (total 0.0)
  (volume 0)
  (repeat nil)
  (random nil)
  (single nil)
  (consume nil)
  (songid nil)
  (bitrate nil))

(cl-defstruct (orpheus-lyrics (:constructor orpheus-lyrics-create))
  "The lyrics of one track: where they came from and their lines.
Each line is (SECONDS . TEXT); SECONDS is nil for lyrics without timing."
  source synced lines)

(cl-defstruct (orpheus-item (:constructor orpheus-item-create))
  "One row of a browsable list.
LABEL and DETAIL are the two columns; KIND says what the row is, and DATA is
a backend-specific alist.  A row that descends carries `browse' in DATA, a
cons of the browse kind and its argument; a row that can be queued carries
`uri'."
  label detail kind data)

(defun orpheus-item-search-match-p (item query)
  "Return non-nil when ITEM's label, URI or track tags match QUERY."
  (let* ((needle (downcase (or query "")))
         (data (orpheus-item-data item))
         (track (alist-get 'track data)))
    (and (not (string-empty-p needle))
         (cl-some
          (lambda (text)
            (and (stringp text)
                 (not (string-empty-p text))
                 (string-match-p (regexp-quote needle) (downcase text))))
          (list (orpheus-item-label item)
                (alist-get 'uri data)
                (and track (orpheus-track-title track))
                (and track (orpheus-track-artist track))
                (and track (orpheus-track-album track)))))))

(defun orpheus-search-filter (items query path)
  "Return the rows in ITEMS that QUERY matches.
ITEMS are all rows one file contributes; a cue sheet gives several tracks.
Matching a track keeps only those tracks.  When no track matches but PATH
does, the whole file, and so every track of a disc, is kept."
  (let ((matches (cl-remove-if-not
                  (lambda (item) (orpheus-item-search-match-p item query))
                  items)))
    (cond (matches matches)
          ((and path (not (string-empty-p (or query "")))
                (string-match-p (regexp-quote (downcase query))
                                (downcase path)))
           items)
          (t nil))))

;;; Formatting

(defcustom orpheus-progress-filled-char ?━
  "Character used for the played part of a progress bar."
  :type 'character :group 'orpheus)

(defcustom orpheus-progress-empty-char ?─
  "Character used for the remaining part of a progress bar."
  :type 'character :group 'orpheus)

(defcustom orpheus-cache-directory
  (expand-file-name "orpheus/"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory where album art and generated files are kept."
  :type 'directory :group 'orpheus)

(defun orpheus-cache-file (key name)
  "Return a path in `orpheus-cache-directory' for KEY and NAME.
KEY is hashed, so a name coming from a tag or a URL cannot climb out of the
cache directory; NAME only contributes its base name."
  (let ((safe (replace-regexp-in-string
               "[^A-Za-z0-9._-]" "_" (file-name-nondirectory name)))
        (dir (expand-file-name (secure-hash 'sha1 (or key ""))
                               orpheus-cache-directory)))
    (make-directory dir t)
    (expand-file-name (if (string-empty-p safe) "file" safe) dir)))

(defcustom orpheus-cache-max-bytes (* 10 1024 1024 1024)
  "Largest total size of `orpheus-cache-directory', in bytes.
Downloaded tracks and covers are deleted, least recently used first, until the
directory fits.  The default leaves room for a few thousand tracks and keeps
the cache across a reboot; nil keeps everything."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'orpheus)

(defconst orpheus-cache-prune-interval 30
  "Seconds between two automatic prunes of the cache.")

(defvar orpheus-cache--pruned-at 0
  "When the cache was last pruned, as a float time.")

(defun orpheus-cache-files (&optional directory)
  "Return the cached files as a list of (PATH SIZE MTIME).
DIRECTORY is `orpheus-cache-directory' unless given.  A partial download is
left out: its bytes are still being written."
  (let ((root (expand-file-name (or directory orpheus-cache-directory))))
    (when (file-directory-p root)
      (delq nil
            (mapcar
             (lambda (path)
               (let ((attributes (and (not (string-suffix-p ".part" path))
                                      (file-attributes path))))
                 (when (and attributes
                            (null (file-attribute-type attributes)))
                   (list path
                         (or (file-attribute-size attributes) 0)
                         (float-time
                          (file-attribute-modification-time attributes))))))
             (directory-files-recursively root "" t))))))

(defun orpheus-cache-size (&optional directory)
  "Return the total size of DIRECTORY, `orpheus-cache-directory' by default.
The answer is in bytes."
  (cl-loop for entry in (orpheus-cache-files directory) sum (nth 1 entry)))

(defun orpheus-cache-prune (&optional keep directory limit)
  "Delete the least recently used cached files until the cache fits.
LIMIT is `orpheus-cache-max-bytes' and DIRECTORY `orpheus-cache-directory'
unless given.  KEEP is a list of paths never deleted, such as the track being
played.  Return the bytes freed, or nil when nothing had to go."
  (let ((limit (or limit orpheus-cache-max-bytes)))
    (when limit
      (let* ((files (orpheus-cache-files directory))
             (total (cl-loop for entry in files sum (nth 1 entry)))
             (keep (mapcar #'file-truename (delq nil keep)))
             (freed 0))
        (dolist (entry (sort files (lambda (a b) (< (nth 2 a) (nth 2 b)))))
          (when (> total limit)
            (let ((path (car entry)))
              (unless (member (file-truename path) keep)
                (when (ignore-errors (delete-file path) t)
                  (cl-incf freed (nth 1 entry))
                  (cl-decf total (nth 1 entry)))))))
        (when (> freed 0) freed)))))

(defun orpheus-cache-prune-later (&optional keep)
  "Prune the cache when Emacs is idle, at most every 30 seconds.
KEEP names the files that must survive, such as the one being played.  The
directory and the limit are read now, so a caller that binds them for a test
or a session cannot prune another cache later."
  (let ((now (float-time))
        (directory (expand-file-name orpheus-cache-directory))
        (limit orpheus-cache-max-bytes))
    (when (and limit
               (>= (- now orpheus-cache--pruned-at)
                   orpheus-cache-prune-interval))
      (setq orpheus-cache--pruned-at now)
      (run-with-idle-timer
       1 nil
       (lambda ()
         (ignore-errors (orpheus-cache-prune keep directory limit)))))))

(defun orpheus-format-time (seconds)
  "Return SECONDS as a clock string, or dashes when there is no duration."
  (if (or (null seconds) (not (numberp seconds)) (< seconds 0)
          (> seconds 359999))
    "-:--"
    (let ((total (round seconds)))
      (if (>= total 3600)
        (format "%d:%02d:%02d" (/ total 3600) (/ (% total 3600) 60)
                (% total 60))
        (format "%d:%02d" (/ total 60) (% total 60))))))

(defun orpheus-progress-percent (elapsed total)
  "Return how far ELAPSED is into TOTAL, as a clamped percentage."
  (if (or (null elapsed) (null total) (<= total 0))
    0
    (max 0 (min 100 (round (* 100 (/ (float elapsed) total)))))))

(defun orpheus-progress-bar (elapsed total width)
  "Draw a bar WIDTH columns wide, filled in proportion to ELAPSED of TOTAL."
  (let* ((width (max 1 width))
         (percent (orpheus-progress-percent elapsed total))
         (filled (min width (round (* width (/ percent 100.0))))))
    (concat (make-string filled orpheus-progress-filled-char)
            (make-string (- width filled) orpheus-progress-empty-char))))

;;; Tracks that play part of a file

;; A CD ripped as one file has no track boundary inside it: the boundaries
;; live in a cue sheet beside it, or in the CUESHEET block of a FLAC file.
;; Such a track is the part of the file it occupies, written as
;; `disc.flac#start=12.533&end=214.316'.  The backend that plays it seeks; the
;; backend that copies it first sends only those minutes.

(defconst orpheus-track-range-regexp
  "\\`\\(.+\\)#start=\\([0-9.]+\\)\\(?:&end=\\([0-9.]+\\)\\)?\\'"
  "Matches a URI that plays only part of the file it names.")

(defun orpheus-track-range (uri)
  "Return the seconds URI plays as a cons of START and END, or nil.
END is nil when URI plays to the end of the file, as the last track of a disc
is cut; a URI that names a whole file has no range at all."
  (when (and (stringp uri)
             (string-match orpheus-track-range-regexp uri))
    (cons (string-to-number (match-string 2 uri))
          (when (match-string 3 uri)
            (string-to-number (match-string 3 uri))))))

(defun orpheus-track-path (uri)
  "Return URI without the range that limits it to one track."
  (if (and (stringp uri) (string-match orpheus-track-range-regexp uri))
      (match-string 1 uri)
    uri))

(defun orpheus-track-range-uri (path start &optional end)
  "Return PATH limited to the seconds between START and END."
  (format "%s#start=%.3f%s" path start
          (if end (format "&end=%.3f" end) "")))

(defun orpheus-track-label (track)
  "Return TRACK as \"artist – title\", falling back to whatever exists."
  (cond ((null track) "")
        ((and (orpheus-track-artist track) (orpheus-track-title track))
         (format "%s – %s" (orpheus-track-artist track)
                 (orpheus-track-title track)))
        ((orpheus-track-title track) (orpheus-track-title track))
        ((orpheus-track-uri track)
         (file-name-nondirectory (orpheus-track-path
                                  (orpheus-track-uri track))))
        (t "")))

(defun orpheus-item-playable (item)
  "Return non-nil when ITEM can be queued or played."
  (and item
       (not (eq (orpheus-item-kind item) 'directory))
       (alist-get 'uri (orpheus-item-data item))
       t))

(defun orpheus-item-browse (item)
  "Return the (KIND . ARG) ITEM descends into, or nil."
  (alist-get 'browse (orpheus-item-data item)))

;;; The registry

(defvar orpheus-backend-registry nil
  "Alist of backend name, constructor and description.
Add one with `orpheus-register-backend' rather than by hand.")

(defconst orpheus-backend-browse-kinds
  '(queue downloads directories artists albumartists albums playlists search)
  "Browse kinds a backend may implement, in tab order.")

(defun orpheus-register-backend (name constructor &optional doc)
  "Register CONSTRUCTOR as the backend called NAME, described by DOC."
  (setf (alist-get name orpheus-backend-registry)
        (cons constructor doc))
  name)

(defun orpheus-backend-exists-p (name)
  "Return non-nil when NAME names a registered backend."
  (and (assq name orpheus-backend-registry) t))

(defun orpheus-make-backend (name)
  "Build a fresh instance of the backend NAME."
  (let ((entry (assq name orpheus-backend-registry)))
    (unless entry
      (error "Orpheus: no backend called %s" name))
    (funcall (car (cdr entry)))))

;;; The protocol

(cl-defstruct (orpheus-backend (:constructor orpheus-backend-create))
  "Base of every backend; it only names itself."
  name)

(cl-defgeneric orpheus-backend-connected-p (backend)
  "Return non-nil when BACKEND is ready to take commands.")

(cl-defgeneric orpheus-backend-connect (backend callback)
  "Connect BACKEND, calling CALLBACK with it when ready, or with nil.")

(cl-defgeneric orpheus-backend-disconnect (backend)
  "Close BACKEND and forget its connection.")

(cl-defgeneric orpheus-backend-status (backend callback)
  "Ask BACKEND for its status and call CALLBACK with an `orpheus-status'.")

(cl-defgeneric orpheus-backend-track (backend callback)
  "Ask BACKEND for the current track and call CALLBACK with it or nil.")

(cl-defgeneric orpheus-backend-queue (backend callback)
  "Ask BACKEND for its queue and call CALLBACK with `orpheus-item' rows.")

(cl-defgeneric orpheus-backend-browse (backend kind arg callback)
  "Ask BACKEND for the KIND list narrowed by ARG, then call CALLBACK.
KIND is one of `orpheus-backend-browse-kinds'; ARG is backend-specific and
usually a list of strings, the path down the hierarchy.")

(cl-defgeneric orpheus-backend-search (backend query callback)
  "Search BACKEND for QUERY, then call CALLBACK with the matches."
  (orpheus-backend-browse backend 'search (list query) callback))

(cl-defgeneric orpheus-backend-downloads (backend callback)
  "Ask BACKEND for its transfers and call CALLBACK with rows.
A backend with a `downloads' capability implements this; the rows have a
`download' kind and carry `uri', `state', `bytes' and `path' in their data."
  (ignore backend)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-cancel-download (backend uri)
  "Cancel BACKEND's transfer for URI and forget its cached copy."
  (ignore backend uri))

(cl-defgeneric orpheus-backend-add (backend uris)
  "Append URIS to BACKEND's queue.")

(cl-defgeneric orpheus-backend-play-uri (backend uri)
  "Play URI on BACKEND now, without discarding what is queued.")

(cl-defgeneric orpheus-backend-portable-uri (backend uri)
  "Return URI as a library-relative path for BACKEND's playlists.
The default keeps URI as written; a backend with a library root overrides it."
  (ignore backend)
  uri)

(cl-defgeneric orpheus-backend-playlist-uri (backend uri)
  "Return the URI BACKEND plays for portable playlist URI.
The default keeps URI as written; a backend with a library root overrides it."
  (ignore backend)
  uri)

(defun orpheus-backend--url-p (uri)
  "Return non-nil when URI names a URL rather than a file."
  (and (stringp uri)
       (string-match-p "\\`[a-z][a-z0-9+.-]*://" uri)))

(defun orpheus-backend--absolute-uri-p (uri)
  "Return non-nil when URI is an absolute path or a URL."
  (and (stringp uri)
       (or (orpheus-backend--url-p uri)
           (file-name-absolute-p uri))))

(defun orpheus-backend--relative-uri (uri root)
  "Return URI below ROOT as a relative path, or nil.
ROOT may be an absolute local directory or a relative remote one.  URLs are
never rewritten."
  (when (and (stringp uri) (stringp root)
             (not (string-empty-p uri))
             (not (string-empty-p root))
             (not (orpheus-backend--url-p uri)))
    (let ((directory (file-name-as-directory root)))
      (if (file-name-absolute-p directory)
          (let ((path (expand-file-name uri directory)))
            (when (and (not (equal directory path))
                       (string-prefix-p directory path))
              (substring path (length directory))))
        (when (string-prefix-p directory uri)
          (substring uri (length directory)))))))

(cl-defgeneric orpheus-backend-command (backend action &optional value)
  "Perform ACTION on BACKEND, with VALUE when the action takes one.
The actions are `play', `pause', `resume', `toggle', `stop', `next',
`previous', `seek', `volume', `repeat', `random', `single', `consume',
`update', `clear', `shuffle', `delete', `save' and `playid'.")

(cl-defgeneric orpheus-backend-equalizer (backend filters)
  "Set BACKEND's audio filter chain to FILTERS.
FILTERS is an mpv-style filter string, or an empty string for no filter.
Return non-nil when the backend accepted it.  A backend whose audio leaves
through another process, such as MPD, returns nil."
  (ignore backend filters)
  nil)

(cl-defgeneric orpheus-backend-lyrics (backend track callback)
  "Ask BACKEND for the lyrics of TRACK, then call CALLBACK with them.
CALLBACK receives an `orpheus-lyrics', or nil when the track has none.  The
default answers nil, for a backend that cannot read them."
  (ignore backend track)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-album-art (backend track callback)
  "Fetch cover art for TRACK from BACKEND, then call CALLBACK with a path or nil."
  (ignore backend track)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-folder-art (backend folder callback)
  "Fetch cover art for FOLDER from BACKEND, then call CALLBACK with a path.
FOLDER is the backend's own path to a directory or album."
  (ignore backend folder)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-local-file (backend track)
  "Return a local file BACKEND holds for TRACK, or nil.
A backend that copies its tracks here, such as SSH, answers with the copy; one
whose files live on another machine, such as MPD, answers nil."
  (ignore backend track)
  nil)

(cl-defgeneric orpheus-backend-outputs (backend callback)
  "Ask BACKEND for its audio outputs, then call CALLBACK with items."
  (ignore backend)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-capabilities (backend)
  "Return the browse kinds BACKEND implements.")

(provide 'orpheus-backend)

;;; orpheus-backend.el ends here
