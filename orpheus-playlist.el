;;; orpheus-playlist.el --- Portable playlists for orpheus -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; A playlist is a list of URIs, not a backend object.  The core writes the
;;; queue as an M3U8 file, reads M3U and PLS files written elsewhere, and asks
;;; the active backend to queue the URIs.  The active backend decides what a
;;; URI means: a local path, an MPD library path or a path on an SSH host.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)

(declare-function orpheus-ui-selected-tracks "orpheus-ui" ())

(defcustom orpheus-playlist-directory
  (expand-file-name "orpheus/playlists"
                    (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory where portable playlists are stored.
`orpheus-ui-save-playlist' writes here unless the playlist is new, in which
case it asks where to keep it and starts in `orpheus-playlist-new-directory'."
  :type 'directory :group 'orpheus)

(defcustom orpheus-playlist-new-directory "~/Music"
  "Directory offered when a new playlist is created.
The player still asks for the directory every time, so this is a starting
point rather than a rule; it defaults to the music directory.  Set it to
`orpheus-playlist-directory' to keep every playlist in one place."
  :type 'directory :group 'orpheus)

(defconst orpheus-playlist--extensions '("m3u" "m3u8" "pls")
  "Extensions this package reads as playlists.")

(defun orpheus-playlist--extension-p (file)
  "Return non-nil when FILE has a playlist extension."
  (member (downcase (or (file-name-extension file) ""))
          orpheus-playlist--extensions))

(defun orpheus-playlist-directories ()
  "Return the directories portable playlists are read from, without duplicates."
  (delete-dups
   (mapcar (lambda (directory)
             (file-name-as-directory (expand-file-name directory)))
           (list orpheus-playlist-directory orpheus-playlist-new-directory))))

(defun orpheus-playlist--inside-p (file &optional directory)
  "Return non-nil when FILE is inside a playlist directory.
With DIRECTORY, only that directory counts."
  (let ((file (expand-file-name file))
        (directories (if directory
                         (list (file-name-as-directory
                                (expand-file-name directory)))
                       (orpheus-playlist-directories))))
    (cl-some (lambda (directory)
               (and (not (equal directory file))
                    (orpheus-backend--path-prefix-p directory file)))
             directories)))

(defun orpheus-playlist--base-name (name)
  "Return NAME as a safe playlist file name.
Only the base name survives, so NAME cannot choose a path, and a name without
an M3U or M3U8 extension gets one."
  (let* ((base (file-name-nondirectory (or name "")))
         (base (cond ((string-empty-p base) "")
                     ((member (downcase (or (file-name-extension base) ""))
                              '("m3u" "m3u8"))
                      base)
                     (t (concat base ".m3u8"))))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._ -]" "_" base)))
    (when (or (string-empty-p safe)
              (member safe '("." ".." ".m3u" ".m3u8")))
      (user-error "Playlist name is empty"))
    safe))

(defun orpheus-playlist-file (name &optional directory)
  "Return the path of the portable playlist called NAME.
DIRECTORY defaults to `orpheus-playlist-directory'.  The name is reduced to
its base name, so it cannot choose a path outside DIRECTORY."
  (expand-file-name (orpheus-playlist--base-name name)
                    (or directory orpheus-playlist-directory)))

(defun orpheus-playlist-find (name)
  "Return the existing portable playlist NAME, or nil.
Both `orpheus-playlist-directory' and `orpheus-playlist-new-directory' are
searched, so a playlist saved next to the music is found like any other."
  (let ((base (orpheus-playlist--base-name name)))
    (cl-find-if (lambda (file) (equal (file-name-nondirectory file) base))
                (orpheus-playlist-files))))

(defun orpheus-playlist--playlist-file-p (file)
  "Return non-nil when FILE is a regular file with a playlist extension."
  (and (file-regular-p file) (orpheus-playlist--extension-p file)))

(defun orpheus-playlist-files ()
  "Return the portable playlist files in the configured directories.
`orpheus-playlist-directory' and `orpheus-playlist-new-directory' are both
read, so a playlist saved next to the music appears in the Playlists tab."
  (let ((files nil))
    (dolist (directory (orpheus-playlist-directories))
      (when (file-directory-p directory)
        (setq files
              (append files
                      (cl-remove-if-not
                       #'orpheus-playlist--playlist-file-p
                       (directory-files directory t "\\`[^.]"))))))
    (sort (delete-dups files) #'string<)))

(defun orpheus-playlist--lines ()
  "Return the lines of the current buffer without line endings."
  (let ((lines nil))
    (while (not (eobp))
      (push (string-trim-right
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position))
             "[\r\n]+")
            lines)
      (forward-line 1))
    (nreverse lines)))

(defun orpheus-playlist--parse-extinf (line)
  "Return the title, artist and duration in an EXTINF LINE."
  (let* ((body (substring line (length "#EXTINF:")))
         (comma (string-search "," body))
         (duration-text (and comma (substring body 0 comma)))
         (label (and comma (string-trim (substring body (1+ comma)))))
         (split (and label (string-match "\\`\\(.+\\) - \\(.+\\)\\'" label))))
    (list :duration (and duration-text
                         (string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'"
                                         duration-text)
                         (string-to-number duration-text))
          :title (cond ((null label) nil)
                       (split (string-trim (match-string 2 label)))
                       (t label))
          :artist (and split (string-trim (match-string 1 label))))))

(defun orpheus-playlist--read-m3u ()
  "Read an M3U playlist from the current buffer."
  (let ((tracks nil)
        (metadata nil))
    (dolist (line (orpheus-playlist--lines))
      (cond
       ((string-empty-p line))
       ((string-prefix-p "#EXTINF:" line)
        (setq metadata (orpheus-playlist--parse-extinf line)))
       ((string-prefix-p "#" line))
       (t
        (push (orpheus-track-create
               :uri line
               :title (plist-get metadata :title)
               :artist (plist-get metadata :artist)
               :duration (plist-get metadata :duration))
              tracks)
        (setq metadata nil))))
    (nreverse tracks)))

(defun orpheus-playlist--read-pls ()
  "Read a PLS playlist from the current buffer."
  (let ((files nil)
        (titles nil))
    (dolist (line (orpheus-playlist--lines))
      (cond
       ((string-match "\\`File\\([0-9]+\\)=\\(.*\\)\\'" line)
        (push (cons (string-to-number (match-string 1 line))
                    (match-string 2 line))
              files))
       ((string-match "\\`Title\\([0-9]+\\)=\\(.*\\)\\'" line)
        (push (cons (string-to-number (match-string 1 line))
                    (match-string 2 line))
              titles))))
    (mapcar
     (lambda (entry)
       (orpheus-track-create
        :uri (cdr entry)
        :title (cdr (assq (car entry) titles))))
     (sort files (lambda (a b) (< (car a) (car b)))))))

(defun orpheus-playlist-read (file)
  "Return the tracks in portable playlist FILE.
URIs are kept as written: a relative path is passed to the backend unchanged."
  (unless (file-readable-p file)
    (user-error "Cannot read playlist %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (eq (char-after) ?\ufeff)
      (forward-char 1))
    (if (equal (downcase (or (file-name-extension file) "")) "pls")
        (orpheus-playlist--read-pls)
      (orpheus-playlist--read-m3u))))

(defun orpheus-playlist--label (track)
  "Return TRACK's label for an EXTINF line."
  (let ((artist (orpheus-track-artist track))
        (title (orpheus-track-title track))
        (uri (orpheus-track-uri track)))
    (replace-regexp-in-string
     "[\r\n]" " "
     (cond ((and artist title) (format "%s - %s" artist title))
           (title title)
           (uri (or (file-name-nondirectory uri) uri))
           (t "Unknown")))))

(defun orpheus-playlist-write (file tracks &optional directory)
  "Write TRACKS as an M3U8 playlist to FILE, and return FILE.
FILE must stay inside one of the configured playlist directories, or inside
DIRECTORY when the user chose it for a new playlist."
  (let ((file (expand-file-name file)))
    (unless (orpheus-playlist--inside-p file directory)
      (user-error "Playlist must stay inside %s"
                  (or directory orpheus-playlist-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert "#EXTM3U\n")
      (dolist (track tracks)
        (let ((uri (orpheus-track-uri track)))
          (when (and uri (not (string-empty-p uri)))
            (let ((duration (orpheus-track-duration track)))
              (insert (format "#EXTINF:%d,%s\n"
                              (if (and (numberp duration) (>= duration 0))
                                  (round duration)
                                -1)
                              (orpheus-playlist--label track))))
            (insert (replace-regexp-in-string "[\r\n]" " " uri) "\n"))))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file nil 'silent)))
    file))

(defun orpheus-playlist-save (name tracks &optional directory)
  "Write TRACKS as portable playlist NAME and return its path.
A playlist that already exists is overwritten where it is; a new one goes to
DIRECTORY, or `orpheus-playlist-directory' when DIRECTORY is nil."
  (let ((file (or (and (null directory) (orpheus-playlist-find name))
                  (orpheus-playlist-file name directory))))
    (when (and (file-exists-p file)
               (not (y-or-n-p (format "Overwrite %s? "
                                      (file-name-nondirectory file)))))
      (user-error "Playlist not saved"))
    (orpheus-playlist-write file tracks directory)))

(defun orpheus-playlist--track-from-item (item)
  "Return a track for queue ITEM, or nil when it has no URI."
  (let* ((data (orpheus-item-data item))
         (track (alist-get 'track data))
         (uri (alist-get 'uri data)))
    (cond
     ((and track (orpheus-track-uri track)) track)
     (uri (orpheus-track-create
           :uri uri
           :title (or (file-name-nondirectory uri) uri))))))

(defun orpheus-playlist--track-with-uri (track uri)
  "Return a copy of TRACK carrying URI."
  (let ((copy (copy-orpheus-track track)))
    (setf (orpheus-track-uri copy) uri)
    copy))

(defun orpheus-playlist-save-queue (name callback &optional directory)
  "Save the current queue as portable playlist NAME.
Call CALLBACK with the file path, or nil when the queue is empty.  A new
playlist is written to DIRECTORY, `orpheus-playlist-directory' by default."
  (let ((backend orpheus-backend-current))
    (unless backend
      (user-error "No backend is connected"))
    (orpheus-backend-queue
     backend
     (lambda (items)
       (let* ((tracks (delq nil (mapcar #'orpheus-playlist--track-from-item
                                       items)))
              (tracks (mapcar
                       (lambda (track)
                         (let ((uri (orpheus-track-uri track)))
                           (if uri
                               (orpheus-playlist--track-with-uri
                                track
                                (orpheus-backend-portable-uri backend uri))
                             track)))
                       tracks))
              (file (and tracks
                         (orpheus-playlist-save name tracks directory))))
         (when callback (funcall callback file)))))))

(defun orpheus-playlist-create (name directory)
  "Create an empty portable playlist NAME in DIRECTORY and return its path.
DIRECTORY is the directory the player asked for; it is made when missing, and
NAME is reduced to a safe base name so it cannot climb out of it."
  (interactive
   (list (read-string "New playlist name: ")
         (read-directory-name
          "Store playlist in: "
          (file-name-as-directory
           (expand-file-name orpheus-playlist-new-directory))
          nil nil)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (file (orpheus-playlist-file name directory)))
    (when (file-exists-p file)
      (user-error "Playlist %s already exists" file))
    (make-directory directory t)
    (orpheus-playlist-write file nil directory)
    file))

(defun orpheus-playlist-uris (file &optional backend)
  "Return the URIs in portable playlist FILE.
With BACKEND, resolve each URI for that backend's library root."
  (let ((uris (delq nil (mapcar #'orpheus-track-uri
                               (orpheus-playlist-read file)))))
    (if backend
        (mapcar (lambda (uri) (orpheus-backend-playlist-uri backend uri))
                uris)
      uris)))

(defun orpheus-playlist-play (file)
  "Replace the queue with portable playlist FILE and play its first track.
Return the number of tracks loaded."
  (let* ((backend orpheus-backend-current)
         (uris (orpheus-playlist-uris file backend)))
    (unless backend
      (user-error "No backend is connected"))
    (when uris
      (orpheus-backend-command backend 'clear)
      (orpheus-backend-play-uri backend (car uris))
      (when (cdr uris)
        (orpheus-backend-add backend (cdr uris))))
    (length uris)))

(defun orpheus-playlist-add (file)
  "Append the tracks in portable playlist FILE to the current queue.
Return the number of tracks added."
  (let* ((backend orpheus-backend-current)
         (uris (orpheus-playlist-uris file backend)))
    (unless backend
      (user-error "No backend is connected"))
    (when uris
      (orpheus-backend-add backend uris))
    (length uris)))

(defun orpheus-playlist-delete (file)
  "Delete portable playlist FILE when it belongs to the playlist directory."
  (when (member (expand-file-name file) (orpheus-playlist-files))
    (delete-file file)
    t))

(defvar-local orpheus-playlist-editor--file nil
  "Portable playlist file the editor is changing.")

(defvar-local orpheus-playlist-editor--tracks nil
  "Tracks in the playlist editor, in the order they will be saved.")

(defvar-local orpheus-playlist-editor--index 0
  "Row the playlist editor keeps point on.")

(defun orpheus-playlist-editor--index ()
  "Return the playlist row index at point, or nil."
  (get-text-property (line-beginning-position) 'orpheus-playlist-index))

(defvar orpheus-playlist-editor-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'orpheus-playlist-editor--select)
    (define-key map [mouse-3] #'orpheus-playlist-editor-mouse-menu)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each playlist row, so the mouse can select and act on it.")

(defvar orpheus-playlist-editor-menu
  (let ((map (make-sparse-keymap "Playlist")))
    (define-key-after map [insert]
      '(menu-item "Insert from player" orpheus-playlist-editor-insert))
    (define-key-after map [move-up]
      '(menu-item "Move up" orpheus-playlist-editor-move-up))
    (define-key-after map [move-down]
      '(menu-item "Move down" orpheus-playlist-editor-move-down))
    (define-key-after map [delete]
      '(menu-item "Remove" orpheus-playlist-editor-delete))
    (define-key-after map [save]
      '(menu-item "Save" orpheus-playlist-editor-save))
    map)
  "Menu opened by a right click on a playlist editor row.")

(defun orpheus-playlist-editor--select (event)
  "Move point to the playlist row clicked in EVENT."
  (interactive "e")
  (let ((position (posn-point (event-start event))))
    (when (integer-or-marker-p position)
      (goto-char position)
      (goto-char (line-beginning-position))
      (let ((index (orpheus-playlist-editor--index)))
        (when index
          (setq orpheus-playlist-editor--index index))))))

(defun orpheus-playlist-editor-mouse-menu (event)
  "Open the row menu for the playlist row clicked in EVENT."
  (interactive "e")
  (orpheus-playlist-editor--select event)
  (popup-menu orpheus-playlist-editor-menu (event-start event)))

(defun orpheus-playlist-editor--render ()
  "Draw the tracks of the playlist editor."
  (let ((inhibit-read-only t)
        (index 0))
    (erase-buffer)
    (if (null orpheus-playlist-editor--tracks)
        (insert (propertize "  empty playlist\n" 'face 'shadow))
      (dolist (track orpheus-playlist-editor--tracks)
        (insert (propertize
                 (format "%3d  %-40s %s\n"
                         (1+ index)
                         (or (orpheus-track-label track) "")
                         (or (orpheus-track-uri track) ""))
                 'orpheus-playlist-index index
                 'mouse-face 'highlight
                 'keymap orpheus-playlist-editor-item-map
                 'help-echo "mouse-1: select, mouse-3: menu"))
        (setq index (1+ index))))
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                        'orpheus-playlist-index
                        (or orpheus-playlist-editor--index 0) #'eq)))
      (goto-char (prop-match-beginning match)))))

(defun orpheus-playlist-editor--move (step)
  "Move the track at point STEP rows and redraw."
  (let* ((index (orpheus-playlist-editor--index))
         (tracks orpheus-playlist-editor--tracks)
         (target (and index (+ index step))))
    (if (or (null index) (< target 0) (>= target (length tracks)))
        (message "orpheus: cannot move this track further")
      (let* ((track (nth index tracks))
             (remaining (delq track tracks))
             (reordered (append (cl-subseq remaining 0 target)
                                (list track)
                                (cl-subseq remaining target))))
        (setq orpheus-playlist-editor--tracks reordered
              orpheus-playlist-editor--index target)
        (set-buffer-modified-p t)
        (orpheus-playlist-editor--render)))))

(defun orpheus-playlist-editor-move-up ()
  "Move the track at point up one row."
  (interactive)
  (orpheus-playlist-editor--move -1))

(defun orpheus-playlist-editor-move-down ()
  "Move the track at point down one row."
  (interactive)
  (orpheus-playlist-editor--move 1))

(defun orpheus-playlist-editor-delete ()
  "Remove the track at point from the playlist."
  (interactive)
  (let ((index (orpheus-playlist-editor--index))
        (tracks orpheus-playlist-editor--tracks))
    (if (or (null index) (>= index (length tracks)))
        (message "orpheus: no track here")
      (setq tracks (delq (nth index tracks) tracks))
      (setq orpheus-playlist-editor--tracks tracks
            orpheus-playlist-editor--index
            (if (>= index (length tracks))
                (max 0 (1- (length tracks)))
              index))
      (set-buffer-modified-p t)
      (orpheus-playlist-editor--render))))

(defun orpheus-playlist-editor-insert ()
  "Insert the player's marked tracks, or its row at point, above this row."
  (interactive)
  (let ((new-tracks (orpheus-ui-selected-tracks)))
    (if (null new-tracks)
        (message "orpheus: no player tracks to insert")
      (let* ((index (or (orpheus-playlist-editor--index)
                        (length orpheus-playlist-editor--tracks)))
             (tracks orpheus-playlist-editor--tracks)
             (before (cl-subseq tracks 0 index))
             (after (nthcdr index tracks)))
        (setq orpheus-playlist-editor--tracks
              (append before new-tracks after)
              orpheus-playlist-editor--index index)
        (set-buffer-modified-p t)
        (orpheus-playlist-editor--render)
        (message "orpheus: inserted %d track(s)" (length new-tracks))))))

(defun orpheus-playlist-editor-save ()
  "Write the edited playlist back to its file."
  (interactive)
  (orpheus-playlist-write orpheus-playlist-editor--file
                           orpheus-playlist-editor--tracks)
  (set-buffer-modified-p nil)
  (when (fboundp 'orpheus-ui-invalidate)
    (orpheus-ui-invalidate))
  (message "orpheus: saved %s"
           (file-name-nondirectory orpheus-playlist-editor--file)))

(defvar orpheus-playlist-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "M-<up>") #'orpheus-playlist-editor-move-up)
    (define-key map (kbd "M-<down>") #'orpheus-playlist-editor-move-down)
    (define-key map (kbd "i") #'orpheus-playlist-editor-insert)
    (define-key map (kbd "d") #'orpheus-playlist-editor-delete)
    (define-key map (kbd "C-k") #'orpheus-playlist-editor-delete)
    (define-key map (kbd "C-x C-s") #'orpheus-playlist-editor-save)
    map)
  "Keymap of `orpheus-playlist-editor-mode'.")

(define-derived-mode orpheus-playlist-editor-mode special-mode "Orpheus-Playlist"
  "Major mode for reordering a portable playlist.
M-<up> and M-<down> move the track at point, `i' inserts the player's current
track or marked tracks, `d' removes it, and `C-x C-s' writes the file.  Mouse-1
selects a row and mouse-3 opens the row menu."
  (setq-local truncate-lines t)
  (setq-local header-line-format
              " M-<up>/M-<down> reorder  i insert  d remove  C-x C-s save  q bury"))

(defun orpheus-playlist-edit (file)
  "Open portable playlist FILE in an editor buffer."
  (interactive
   (list (read-file-name "Playlist: "
                         (file-name-as-directory orpheus-playlist-directory)
                         nil t)))
  (when (equal (downcase (or (file-name-extension file) "")) "pls")
    (user-error "PLS playlists are read-only; export as M3U8 first"))
  (let ((tracks (orpheus-playlist-read file))
        (buffer (generate-new-buffer
                 (format "*orpheus playlist: %s*"
                         (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (orpheus-playlist-editor-mode)
      (setq-local orpheus-playlist-editor--file (expand-file-name file)
                  orpheus-playlist-editor--tracks tracks
                  orpheus-playlist-editor--index 0)
      (setq-local header-line-format
                  (format " %s  M-<up>/M-<down> reorder  i insert  d remove  C-x C-s save  q bury"
                          (file-name-nondirectory file)))
      (orpheus-playlist-editor--render))
    (pop-to-buffer buffer '(display-buffer-same-window))
    buffer))

(defun orpheus-playlist-items ()
  "Return portable playlist files as `orpheus-item' rows."
  (mapcar
   (lambda (file)
     (let ((tracks (condition-case nil
                       (orpheus-playlist-read file)
                     (error 'error))))
       (orpheus-item-create
        :label (file-name-base file)
        :detail (if (eq tracks 'error)
                    "unreadable"
                  (format "%d track%s" (length tracks)
                          (if (= (length tracks) 1) "" "s")))
        :kind 'playlist
        :data (list (cons 'file file)
                    (cons 'count (if (eq tracks 'error) 0 (length tracks)))))))
   (orpheus-playlist-files)))

(provide 'orpheus-playlist)

;;; orpheus-playlist.el ends here
