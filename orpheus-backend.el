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

(cl-defstruct (orpheus-item (:constructor orpheus-item-create))
  "One row of a browsable list.
LABEL and DETAIL are the two columns; KIND says what the row is, and DATA is
a backend-specific alist.  A row that descends carries `browse' in DATA, a
cons of the browse kind and its argument; a row that can be queued carries
`uri'."
  label detail kind data)

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

(defun orpheus-track-label (track)
  "Return TRACK as \"artist – title\", falling back to whatever exists."
  (cond ((null track) "")
        ((and (orpheus-track-artist track) (orpheus-track-title track))
         (format "%s – %s" (orpheus-track-artist track)
                 (orpheus-track-title track)))
        ((orpheus-track-title track) (orpheus-track-title track))
        ((orpheus-track-uri track)
         (file-name-nondirectory (orpheus-track-uri track)))
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
  "Browse kinds a backend may implement, in rmpc's tab order.")

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

(cl-defgeneric orpheus-backend-album-art (backend track callback)
  "Fetch cover art for TRACK from BACKEND, then call CALLBACK with a path or nil."
  (ignore backend track)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-folder-art (backend folder callback)
  "Fetch cover art for FOLDER from BACKEND, then call CALLBACK with a path.
FOLDER is the backend's own path to a directory or album."
  (ignore backend folder)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-outputs (backend callback)
  "Ask BACKEND for its audio outputs, then call CALLBACK with items."
  (ignore backend)
  (funcall callback nil))

(cl-defgeneric orpheus-backend-capabilities (backend)
  "Return the browse kinds BACKEND implements.")

(provide 'orpheus-backend)

;;; orpheus-backend.el ends here
