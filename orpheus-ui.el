;;; orpheus-ui.el --- The orpheus player interface -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; One buffer holds the player.  A header line shows what is playing, the
;;; first line of the buffer holds the tabs, and the rest is the list the
;;; current tab shows: the queue, the filesystem, artists, albums, playlists or
;;; search results.
;;;
;;; The interface is text first, so it works in a terminal as well as in a
;;; graphical frame.  Two things are added when the frame can draw them: cover
;;; art above the list, and the progress bar's Unicode fill, which falls back to
;;; ASCII when `orpheus-ui-plain-ascii' says so.  Everything else is ordinary
;;; Emacs text, faces and a keymap, so a terminal user loses nothing but
;;; pictures.
;;;
;;; The status lives in `header-line-format' on purpose: it changes every
;;; second, and redrawing only the header line leaves the list, point and the
;;; scroll position untouched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)

;;; Look

(defface orpheus-status-face
  '((t :inherit bold))
  "Face of the status line."
  :group 'orpheus)

(defface orpheus-title-face
  '((t :inherit default))
  "Face of a track's title."
  :group 'orpheus)

(defface orpheus-artist-face
  '((t :inherit font-lock-variable-name-face))
  "Face of artists, playlists and other named things."
  :group 'orpheus)

(defface orpheus-album-face
  '((t :inherit font-lock-string-face))
  "Face of album names."
  :group 'orpheus)

(defface orpheus-current-face
  '((t :inherit highlight :extend t))
  "Face of the row that is playing."
  :group 'orpheus)

(defface orpheus-tab-face
  '((t :inherit shadow))
  "Face of a tab that is not selected."
  :group 'orpheus)

(defface orpheus-tab-active-face
  '((t :inherit bold :underline t))
  "Face of the selected tab."
  :group 'orpheus)

(defface orpheus-muted-face
  '((t :inherit shadow))
  "Face of separators, hints and other quiet text."
  :group 'orpheus)

(defface orpheus-directory-face
  '((t :inherit dired-directory))
  "Face of a directory row."
  :group 'orpheus)

(defface orpheus-error-face
  '((t :inherit error))
  "Face of a failure or a disconnection."
  :group 'orpheus)

(defcustom orpheus-ui-buffer-name "*orpheus*"
  "Name of the player's buffer."
  :type 'string :group 'orpheus)

(defcustom orpheus-ui-plain-ascii nil
  "Whether to draw glyphs with ASCII instead of Unicode.
Set it in a terminal whose font has no play, pause or block characters."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ui-show-art t
  "Whether to draw cover art in a graphical frame."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ui-art-size 220
  "Longest side of the cover art, in pixels."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ui-progress-width 16
  "Width of the progress bar in the status line, in columns."
  :type 'integer :group 'orpheus)

;;; Glyphs and strings

(defun orpheus-status-glyph (status)
  "Return the glyph that stands for STATUS's state."
  (let ((plain orpheus-ui-plain-ascii))
    (pcase (and status (orpheus-status-state status))
           ('play (if plain ">" "▶"))
           ('pause (if plain "||" "⏸"))
           (_ (if plain "." "⏹")))))

(defun orpheus-ui--flag (on letter)
  "Return LETTER when ON, and a blank column otherwise."
  (if on letter " "))

(defun orpheus-ui--status-string (status track)
  "Return the one-line status of STATUS and TRACK."
  (let ((elapsed (and status (orpheus-status-elapsed status)))
        (total (and status (orpheus-status-total status)))
        (flags (concat (orpheus-ui--flag (and status (orpheus-status-repeat status)) "R")
                       (orpheus-ui--flag (and status (orpheus-status-random status)) "Z")
                       (orpheus-ui--flag (and status (orpheus-status-single status)) "S")
                       (orpheus-ui--flag (and status (orpheus-status-consume status)) "C"))))
    (format " %s  %s  %s / %s  [%s] %3d%%  vol %d  %s"
            (orpheus-status-glyph status)
            (if (and track (not (string-empty-p (orpheus-track-label track))))
              (orpheus-track-label track)
              (if status "nothing playing" "connecting…"))
            (orpheus-format-time elapsed)
            (orpheus-format-time total)
            (orpheus-progress-bar elapsed total orpheus-ui-progress-width)
            (orpheus-progress-percent elapsed total)
            (if status (orpheus-status-volume status) 0)
            flags)))

(defun orpheus-ui--elide (string width)
  "Return STRING fitted into WIDTH columns, cut with an ellipsis."
  (let* ((width (max 1 width)))
    (if (<= (string-width string) width)
      string
      (truncate-string-to-width string width nil nil "…"))))

(defun orpheus-ui--columns (left right width)
  "Return LEFT and RIGHT as one row WIDTH columns wide, RIGHT last."
  (let* ((right (orpheus-ui--elide right width))
         (right-width (string-width right))
         (left (orpheus-ui--elide left (max 0 (- width right-width 1))))
         (pad (max 1 (- width right-width (string-width left)))))
    (concat left (make-string pad ?\s) right)))

(defun orpheus-ui-kind-title (kind)
  "Return the tab title for the browse KIND."
  (pcase kind
         ('queue "Queue")
         ('directories "Directories")
         ('artists "Artists")
         ('albumartists "Album Artists")
         ('albums "Albums")
         ('playlists "Playlists")
         ('search "Search")
         (_ (capitalize (symbol-name kind)))))

(defun orpheus-ui--tabs-for-capabilities (capabilities)
  "Return the tabs a backend with CAPABILITIES gets, as (KIND . TITLE).
The queue is always there, because every backend can queue."
  (delq nil
        (mapcar (lambda (kind)
                  (when (or (eq kind 'queue) (memq kind capabilities))
                    (cons kind (orpheus-ui-kind-title kind))))
                orpheus-backend-browse-kinds)))

;;; State

(cl-defstruct (orpheus-ui-tab (:constructor orpheus-ui-tab-create))
  "One tab: what it is, what it shows and where the point is."
  kind
  title
  view
  arg
  (stack nil)
  (items nil)
  (index 0)
  (loading nil))

(defvar-local orpheus-ui--tabs nil
              "Vector of `orpheus-ui-tab' in this buffer.")

(defvar-local orpheus-ui--tab 0
              "Index of the tab this buffer shows.")

(defvar-local orpheus-ui--backend nil
              "The backend the tabs were built for.")

(defvar-local orpheus-ui--render-key nil
              "What the buffer was last drawn from, so an unchanged view is left alone.")

(defvar orpheus-ui--art nil
  "Plist of the cover art in use: :uri and, once fetched, :file.")

(defun orpheus-ui--current-tab ()
  "Return the tab this buffer shows."
  (and (vectorp orpheus-ui--tabs)
       (> (length orpheus-ui--tabs) orpheus-ui--tab)
       (aref orpheus-ui--tabs orpheus-ui--tab)))

(defun orpheus-ui--current-items ()
  "Return the rows of the current tab."
  (let ((tab (orpheus-ui--current-tab)))
    (and tab (orpheus-ui-tab-items tab))))

(defun orpheus-ui--item-at-point ()
  "Return the row point is on."
  (let ((index (get-text-property (point) 'orpheus-item)))
    (when (integerp index)
      (nth index (orpheus-ui--current-items)))))

(defun orpheus-ui--sync-index ()
  "Record the row point is on as the tab's position."
  (let ((tab (orpheus-ui--current-tab))
        (index (get-text-property (point) 'orpheus-item)))
    (when (and tab (integerp index))
      (setf (orpheus-ui-tab-index tab) index))))

;;; Drawing

(defvar orpheus-ui-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-ui-mouse-select)
    (define-key map [mouse-2] 'orpheus-ui-mouse-play)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each row, so the mouse works on it.")

(defun orpheus-ui--header-line ()
  "Return the header line of the player."
  (if (and orpheus-backend-current
           (orpheus-backend-connected-p orpheus-backend-current))
    (orpheus-ui--status-string orpheus-status-current orpheus-track-current)
    (propertize " orpheus  not connected" 'face 'orpheus-error-face)))

(defun orpheus-ui--tab-string ()
  "Return the tab bar."
  (let ((index 0)
        (parts nil))
    (dolist (tab (append orpheus-ui--tabs nil))
      (push (propertize (format "%d %s" (1+ index) (orpheus-ui-tab-title tab))
                        'face (if (= index orpheus-ui--tab)
                                'orpheus-tab-active-face
                                'orpheus-tab-face))
            parts)
      (setq index (1+ index)))
    (concat " " (string-join (nreverse parts) "    "))))

(defun orpheus-ui--path-string ()
  "Return the path inside the current tab, for the header."
  (let ((tab (orpheus-ui--current-tab)))
    (if (and tab (orpheus-ui-tab-arg tab))
      (concat "  " (string-join
                    (delq nil (mapcar (lambda (part)
                                        (when (stringp part) part))
                                      (orpheus-ui-tab-arg tab)))
                    " / "))
      "")))

(defun orpheus-ui--art-image ()
  "Return the cover art as an image, or nil."
  (let ((file (plist-get orpheus-ui--art :file)))
    (when (and file (file-readable-p file) (display-graphic-p))
      (create-image file nil nil :max-width orpheus-ui-art-size
                    :max-height orpheus-ui-art-size))))

(defun orpheus-ui--insert-art (width)
  "Insert the cover art centred in WIDTH, when there is one."
  (when-let* ((image (orpheus-ui--art-image)))
             (let* ((size (image-size image t))
                    (indent (max 0 (/ (- width (car size)) 2))))
               (insert (make-string indent ?\s))
               (insert-image image (make-string 1 ?\s))
               (insert "\n"))))

(defun orpheus-ui--current-p (item)
  "Return non-nil when ITEM is the row playing now."
  (let* ((data (orpheus-item-data item))
         (id (alist-get 'id data))
         (playing (and orpheus-status-current
                       (orpheus-status-songid orpheus-status-current))))
    (and id playing (equal id playing))))

(defun orpheus-ui--insert-item (item index width)
  "Insert ITEM, the INDEXth row, fitted into WIDTH."
  (let* ((kind (orpheus-item-kind item))
         (current (orpheus-ui--current-p item))
         (face (pcase kind
                      ('directory 'orpheus-directory-face)
                      ('album 'orpheus-album-face)
                      ('playlist 'orpheus-artist-face)
                      ('artist 'orpheus-artist-face)
                      ('output 'orpheus-muted-face)
                      (_ 'orpheus-title-face)))
         (row (orpheus-ui--columns (or (orpheus-item-label item) "")
                                   (or (orpheus-item-detail item) "")
                                   width))
         (start (point)))
    (insert (propertize row
                        'face (if current 'orpheus-current-face face)
                        'orpheus-item index
                        'orpheus-item-kind kind
                        'mouse-face 'highlight
                        'help-echo (orpheus-item-label item)))
    (insert "\n")
    (add-text-properties start (1- (point))
                         (list 'keymap orpheus-ui-item-map))))

(defun orpheus-ui--render ()
  "Draw the buffer from the tabs and rows it holds."
  (let ((inhibit-read-only t)
        (tab (orpheus-ui--current-tab))
        (width (max 24 (if (get-buffer-window) (window-body-width) 80))))
    (erase-buffer)
    (insert (orpheus-ui--tab-string) "\n")
    (when (orpheus-ui-tab-arg tab)
      (insert (propertize (orpheus-ui--path-string) 'face 'orpheus-muted-face)
              "\n"))
    (when orpheus-ui-show-art
      (orpheus-ui--insert-art width))
    (insert (propertize (make-string width ?─) 'face 'orpheus-muted-face) "\n")
    (let ((items (orpheus-ui-tab-items tab)))
      (if (null items)
        (insert (propertize
                 (if (orpheus-ui-tab-loading tab)
                   "  loading…"
                   "  nothing here yet")
                 'face 'orpheus-muted-face)
                "\n")
        (let ((index 0))
          (dolist (item items)
            (orpheus-ui--insert-item item index width)
            (setq index (1+ index))))))
    (insert (propertize
             " RET play  a add  d delete  / search  g refresh  ? help  q bury"
             'face 'orpheus-muted-face))
    (orpheus-ui--place-point)
    (setq orpheus-ui--render-key (orpheus-ui--key))))

(defun orpheus-ui--key ()
  "Return what the buffer was drawn from.
The tab record itself is left out: it is mutated in place, so comparing it
would always say nothing changed.  The playing song is in, because it decides
which row is highlighted."
  (let ((tab (orpheus-ui--current-tab)))
    (list orpheus-ui--tab
          (and tab (orpheus-ui-tab-view tab))
          (and tab (orpheus-ui-tab-arg tab))
          (and tab (orpheus-ui-tab-items tab))
          (and tab (orpheus-ui-tab-loading tab))
          (and orpheus-status-current
               (orpheus-status-songid orpheus-status-current))
          (plist-get orpheus-ui--art :file))))

(defun orpheus-ui--redraw ()
  "Draw the buffer again, but only when what it shows has changed."
  (when (buffer-live-p (get-buffer orpheus-ui-buffer-name))
    (with-current-buffer orpheus-ui-buffer-name
      (unless (equal (orpheus-ui--key) orpheus-ui--render-key)
        (orpheus-ui--render)))))

(defun orpheus-ui--place-point ()
  "Put point on the row the tab remembers."
  (let* ((tab (orpheus-ui--current-tab))
         (index (if tab (orpheus-ui-tab-index tab) 0))
         (match (progn (goto-char (point-min))
                  (text-property-search-forward 'orpheus-item index #'eq))))
    (when match
      (goto-char (prop-match-beginning match)))))

;;; Reading data

(defun orpheus-ui--set-items (tab items)
  "Remember ITEMS as TAB's rows."
  (setf (orpheus-ui-tab-items tab) items
        (orpheus-ui-tab-loading tab) nil)
  (when (>= (orpheus-ui-tab-index tab) (max 1 (length items)))
    (setf (orpheus-ui-tab-index tab) (max 0 (1- (length items))))))

(defun orpheus-ui--load-tab (tab callback)
  "Read TAB's rows and then call CALLBACK, in this buffer."
  (let ((backend orpheus-backend-current)
        (buffer (current-buffer)))
    (when (and backend (orpheus-backend-connected-p backend))
      (setf (orpheus-ui-tab-loading tab) t)
      (let ((done (lambda (items)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (orpheus-ui--set-items tab items)
                        (funcall callback))))))
        (if (eq (orpheus-ui-tab-view tab) 'queue)
          (orpheus-backend-queue backend done)
          (orpheus-backend-browse backend (orpheus-ui-tab-view tab)
                                  (orpheus-ui-tab-arg tab) done))))))

(defun orpheus-ui-refresh ()
  "Read the current tab again and redraw when it changed."
  (interactive)
  (let ((buffer (get-buffer orpheus-ui-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((tab (orpheus-ui--current-tab)))
          (when tab
            (orpheus-ui--load-tab tab #'orpheus-ui--redraw)))))))

(defun orpheus-ui--setup-tabs ()
  "Build the tab bar for the backend in use."
  (let* ((backend orpheus-backend-current)
         (capabilities (if backend
                         (orpheus-backend-capabilities backend)
                         '(queue directories))))
    (setq orpheus-ui--tabs
      (vconcat
       (mapcar (lambda (pair)
                 (orpheus-ui-tab-create :kind (car pair) :title (cdr pair)
                                        :view (car pair)))
               (orpheus-ui--tabs-for-capabilities capabilities)))
      orpheus-ui--backend backend
      orpheus-ui--tab 0)))

;;; Moving

(defun orpheus-ui--move (n)
  "Move the remembered row by N."
  (let* ((tab (orpheus-ui--current-tab))
         (count (length (orpheus-ui-tab-items tab))))
    (when (and tab (> count 0))
      (setf (orpheus-ui-tab-index tab)
            (max 0 (min (1- count) (+ (orpheus-ui-tab-index tab) n))))
      (orpheus-ui--place-point))))

(defun orpheus-ui-next-line (&optional n)
  "Move down N rows."
  (interactive "p")
  (orpheus-ui--move (or n 1)))

(defun orpheus-ui-previous-line (&optional n)
  "Move up N rows."
  (interactive "p")
  (orpheus-ui--move (- (or n 1))))

(defun orpheus-ui-first-line ()
  "Move to the first row."
  (interactive)
  (orpheus-ui--move (- (length (orpheus-ui--current-items)))))

(defun orpheus-ui-last-line ()
  "Move to the last row."
  (interactive)
  (orpheus-ui--move (length (orpheus-ui--current-items))))

(defun orpheus-ui-scroll-down ()
  "Move down one screenful."
  (interactive)
  (orpheus-ui--move (max 1 (- (window-body-height) 4))))

(defun orpheus-ui-scroll-up ()
  "Move up one screenful."
  (interactive)
  (orpheus-ui--move (- (max 1 (- (window-body-height) 4)))))

;;; Acting

(defun orpheus-ui--switch-to (kind)
  "Show the tab of KIND."
  (let ((index 0)
        (found nil))
    (dotimes (i (length (or orpheus-ui--tabs [])))
      (when (eq (orpheus-ui-tab-kind (aref orpheus-ui--tabs i)) kind)
        (setq index i found t)))
    (when found
      (setq orpheus-ui--tab index)
      (orpheus-ui--render)
      (orpheus-ui-refresh))))

(defun orpheus-ui-next-tab ()
  "Show the next tab."
  (interactive)
  (when (vectorp orpheus-ui--tabs)
    (setq orpheus-ui--tab (% (1+ orpheus-ui--tab)
                             (length orpheus-ui--tabs)))
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-previous-tab ()
  "Show the previous tab."
  (interactive)
  (when (vectorp orpheus-ui--tabs)
    (setq orpheus-ui--tab (% (+ (1- orpheus-ui--tab)
                                (length orpheus-ui--tabs))
                             (length orpheus-ui--tabs)))
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-select-tab (n)
  "Show the Nth tab."
  (interactive "nTab: ")
  (when (and (vectorp orpheus-ui--tabs)
             (> n 0) (<= n (length orpheus-ui--tabs)))
    (setq orpheus-ui--tab (1- n))
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-confirm ()
  "Play the row at point, or descend into it."
  (interactive)
  (let ((item (orpheus-ui--item-at-point))
        (tab (orpheus-ui--current-tab)))
    (cond
      ((null item) (message "orpheus: no row here"))
      ((orpheus-item-browse item)
       (pcase-let ((`(,kind . ,arg) (orpheus-item-browse item)))
         (setf (orpheus-ui-tab-stack tab)
               (cons (list (orpheus-ui-tab-view tab) (orpheus-ui-tab-arg tab))
                     (orpheus-ui-tab-stack tab)))
         (setf (orpheus-ui-tab-view tab) kind
               (orpheus-ui-tab-arg tab) (if (listp arg) arg (list arg))
               (orpheus-ui-tab-index tab) 0)
         (orpheus-ui--render)
         (orpheus-ui-refresh)))
      ((eq (orpheus-ui-tab-view tab) 'queue)
       (orpheus-backend-command orpheus-backend-current 'playpos
                                (alist-get 'pos (orpheus-item-data item))))
      ((orpheus-item-playable item)
       (orpheus-backend-play-uri orpheus-backend-current
                                 (alist-get 'uri (orpheus-item-data item)))
       (orpheus-ui-refresh))
      (t (message "orpheus: nothing to play here")))))

(defun orpheus-ui-up ()
  "Go back up the current tab's hierarchy."
  (interactive)
  (let ((tab (orpheus-ui--current-tab)))
    (if-let* ((parent (car (orpheus-ui-tab-stack tab))))
             (progn
               (setf (orpheus-ui-tab-stack tab) (cdr (orpheus-ui-tab-stack tab)))
               (setf (orpheus-ui-tab-view tab) (car parent)
                     (orpheus-ui-tab-arg tab) (car (cdr parent))
                     (orpheus-ui-tab-index tab) 0)
               (orpheus-ui--render)
               (orpheus-ui-refresh))
             (message "orpheus: already at the top"))))

(defun orpheus-ui-add ()
  "Add the row at point to the queue.
The queue tab is not a source: a queue row added to the queue appends the
whole queue to itself."
  (interactive)
  (let ((item (orpheus-ui--item-at-point))
        (tab (orpheus-ui--current-tab)))
    (cond
      ((eq (orpheus-ui-tab-kind tab) 'queue)
       (message "orpheus: this is the queue already; nothing to add"))
      ((orpheus-item-playable item)
       (progn
         (orpheus-backend-add orpheus-backend-current
                              (alist-get 'uri (orpheus-item-data item)))
         (message "orpheus: queued %s" (orpheus-item-label item))
         (orpheus-ui-refresh)))
      (t (message "orpheus: nothing to queue here")))))

(defun orpheus-ui-add-all ()
  "Add every playable row of this tab to the queue.
Like `orpheus-ui-add', this does nothing on the queue tab, where every row
is a copy of a row that is there already."
  (interactive)
  (let ((uris (delq nil (mapcar (lambda (item)
                                  (and (orpheus-item-playable item)
                                       (alist-get 'uri (orpheus-item-data item))))
                                (orpheus-ui--current-items))))
        (tab (orpheus-ui--current-tab)))
    (cond
      ((eq (orpheus-ui-tab-kind tab) 'queue)
       (message "orpheus: this is the queue already; nothing to add"))
      (uris
        (orpheus-backend-add orpheus-backend-current uris)
        (message "orpheus: queued %d track(s)" (length uris))
        (orpheus-ui-refresh))
      (t (message "orpheus: nothing to queue here")))))

(defun orpheus-ui-delete ()
  "Remove the row at point from the queue."
  (interactive)
  (let ((tab (orpheus-ui--current-tab))
        (item (orpheus-ui--item-at-point)))
    (cond
      ((null item) (message "orpheus: no row here"))
      ((not (eq (orpheus-ui-tab-view tab) 'queue))
       (message "orpheus: only the queue can be edited here"))
      (t
        (let ((pos (or (alist-get 'pos (orpheus-item-data item))
                       (orpheus-ui-tab-index tab))))
          (orpheus-backend-command orpheus-backend-current 'delete pos)
          (orpheus-ui-refresh))))))

(defun orpheus-ui-clear ()
  "Empty the queue, after asking."
  (interactive)
  (when (y-or-n-p "Clear the queue? ")
    (orpheus-backend-command orpheus-backend-current 'clear)
    (orpheus-ui-refresh)))

(defun orpheus-ui-jump-to-current ()
  "Show the queue and put point on the playing row."
  (interactive)
  (orpheus-ui--switch-to 'queue)
  (let ((songid (and orpheus-status-current
                     (orpheus-status-songid orpheus-status-current)))
        (index 0))
    (dolist (item (orpheus-ui--current-items))
      (when (and songid (equal (alist-get 'id (orpheus-item-data item)) songid))
        (setf (orpheus-ui-tab-index (orpheus-ui--current-tab)) index))
      (setq index (1+ index))))
  (orpheus-ui--place-point))

(defun orpheus-ui-search (query)
  "Search the backend for QUERY."
  (interactive (list (read-string "Search: ")))
  (when (string-empty-p query)
    (user-error "No search string"))
  (let ((index 0)
        (found nil))
    (dotimes (i (length (or orpheus-ui--tabs [])))
      (when (eq 'search (orpheus-ui-tab-kind (aref orpheus-ui--tabs i)))
        (setq index i found t)))
    (when found
      (setq orpheus-ui--tab index)
      (let ((tab (aref orpheus-ui--tabs index)))
        (setf (orpheus-ui-tab-view tab) 'search
              (orpheus-ui-tab-arg tab) (list query)
              (orpheus-ui-tab-stack tab) nil
              (orpheus-ui-tab-index tab) 0))
      (orpheus-ui--render)
      (orpheus-ui-refresh))))

(defun orpheus-ui-save-playlist (name)
  "Save the queue as the playlist NAME."
  (interactive (list (read-string "Save queue as playlist: ")))
  (orpheus-backend-command orpheus-backend-current 'save name)
  (message "orpheus: saved %s" name))

(defun orpheus-ui-mouse-select (event)
  "Move point to the row clicked on in EVENT."
  (interactive "e")
  (let ((position (posn-point (event-start event))))
    (when (integer-or-marker-p position)
      (goto-char position)
      (orpheus-ui--sync-index)
      (orpheus-ui--place-point))))

(defun orpheus-ui-mouse-play (event)
  "Play the row clicked on in EVENT."
  (interactive "e")
  (orpheus-ui-mouse-select event)
  (orpheus-ui-confirm))

;;; Help

(defconst orpheus-ui--help-lines
  '(("RET" "play the track, or open the album, artist or directory")
    ("n / C-n / j" "next row")
    ("C-p / k" "previous row")
    ("C-v / M-v" "one screen down and up")
    ("M-< / M->" "first and last row")
    ("TAB / S-TAB" "next and previous tab")
    ("1 … 7" "queue, directories, artists, album artists, albums, playlists, search")
    ("C-b" "back up one level")
    ("a / A" "queue this row, queue everything here")
    ("d / C-k" "delete the row (from the queue, on the queue tab)")
    ("D" "clear the queue")
    ("/ / C-s" "search")
    ("C" "jump to the playing row")
    ("?" "this help")
    ("SPC / p" "play or pause")
    ("s" "stop")
    ("> / <" "next and previous track")
    ("f / b" "seek forward and back")
    ("M-<up> / M-<down>, + / -" "volume up and down")
    ("z / x / c / v" "repeat, random, consume, single")
    ("u / U" "update the library, rescan it")
    ("C-x C-s" "save the queue as a playlist")
    ("g" "refresh the status and this tab")
    ("V" "spectrum visualizer")
    ("q" "bury the player"))
  "The keys `orpheus-ui-mode' binds, as rows of key and meaning.")

(defun orpheus-ui-help ()
  "Show the keys of the player."
  (interactive)
  (let ((buffer (get-buffer-create "*orpheus help*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "orpheus keys\n\n" 'face 'orpheus-status-face))
        (dolist (row orpheus-ui--help-lines)
          (insert (propertize (format "  %-20s %s\n" (car row) (cadr row))
                              'face 'orpheus-title-face)))
        (insert "\n")
        (insert (propertize " q buries the player; g reads everything again\n"
                            'face 'orpheus-muted-face))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)))

;;; The mode

(defvar orpheus-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'orpheus-ui-confirm)
    (define-key map (kbd "n") 'orpheus-ui-next-line)
    (define-key map (kbd "C-n") 'orpheus-ui-next-line)
    (define-key map (kbd "j") 'orpheus-ui-next-line)
    (define-key map (kbd "C-p") 'orpheus-ui-previous-line)
    (define-key map (kbd "k") 'orpheus-ui-previous-line)
    (define-key map (kbd "C-v") 'orpheus-ui-scroll-down)
    (define-key map (kbd "M-v") 'orpheus-ui-scroll-up)
    (define-key map (kbd "M-<") 'orpheus-ui-first-line)
    (define-key map (kbd "M->") 'orpheus-ui-last-line)
    (define-key map (kbd "TAB") 'orpheus-ui-next-tab)
    (define-key map (kbd "<backtab>") 'orpheus-ui-previous-tab)
    ;; The digits pick a tab; each gets a closure so no prefix is needed.
    (dotimes (i 7)
      (let ((number (1+ i)))
        (define-key map (vector (+ ?1 i))
                    (lambda () (interactive) (orpheus-ui-select-tab number)))))
    (define-key map (kbd "C-b") 'orpheus-ui-up)
    (define-key map (kbd "a") 'orpheus-ui-add)
    (define-key map (kbd "A") 'orpheus-ui-add-all)
    (define-key map (kbd "d") 'orpheus-ui-delete)
    (define-key map (kbd "C-k") 'orpheus-ui-delete)
    (define-key map (kbd "D") 'orpheus-ui-clear)
    (define-key map (kbd "/") 'orpheus-ui-search)
    (define-key map (kbd "C-s") 'orpheus-ui-search)
    (define-key map (kbd "C") 'orpheus-ui-jump-to-current)
    (define-key map (kbd "?") 'orpheus-ui-help)
    (define-key map (kbd "SPC") 'orpheus-play-pause)
    (define-key map (kbd "p") 'orpheus-play-pause)
    (define-key map (kbd "s") 'orpheus-stop)
    (define-key map (kbd ">") 'orpheus-next)
    (define-key map (kbd "<") 'orpheus-previous)
    (define-key map (kbd "f") 'orpheus-seek-forward)
    (define-key map (kbd "b") 'orpheus-seek-backward)
    (define-key map (kbd "M-<up>") 'orpheus-volume-up)
    (define-key map (kbd "M-<down>") 'orpheus-volume-down)
    (define-key map (kbd "+") 'orpheus-volume-up)
    (define-key map (kbd "-") 'orpheus-volume-down)
    (define-key map (kbd "z") 'orpheus-toggle-repeat)
    (define-key map (kbd "x") 'orpheus-toggle-random)
    (define-key map (kbd "c") 'orpheus-toggle-consume)
    (define-key map (kbd "v") 'orpheus-toggle-single)
    (define-key map (kbd "u") 'orpheus-update-database)
    (define-key map (kbd "U") 'orpheus-rescan-database)
    (define-key map (kbd "g") 'orpheus-refresh)
    (define-key map (kbd "C-x C-s") 'orpheus-ui-save-playlist)
    (define-key map (kbd "V") 'orpheus-visualizer)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap of `orpheus-ui-mode'.")

(define-derived-mode orpheus-ui-mode special-mode "Orpheus"
  "Major mode for the orpheus music player."
  (setq-local header-line-format '(:eval (orpheus-ui--header-line)))
  (setq-local truncate-lines t)
  (setq-local line-spacing nil)
  (buffer-disable-undo)
  (setq-local orpheus-ui--tabs nil)
  (setq-local orpheus-ui--tab 0)
  (setq-local orpheus-ui--render-key nil))

;;; Opening

(defun orpheus-ui-open ()
  "Show the player, building it the first time."
  (interactive)
  (let ((buffer (get-buffer-create orpheus-ui-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'orpheus-ui-mode)
        (orpheus-ui-mode))
      (unless (and (vectorp orpheus-ui--tabs)
                   (eq orpheus-ui--backend orpheus-backend-current))
        (orpheus-ui--setup-tabs))
      (orpheus-ui--render))
    (pop-to-buffer buffer)
    (orpheus-ui-refresh)))

(defun orpheus-ui--update-art (track)
  "Fetch cover art for TRACK, when it is not the one already shown."
  (let ((uri (and track (orpheus-track-uri track)))
        (backend orpheus-backend-current))
    (when (and uri backend orpheus-ui-show-art
               (not (equal uri (plist-get orpheus-ui--art :uri))))
      (setq orpheus-ui--art (list :uri uri))
      (orpheus-backend-album-art
       backend track
       (lambda (file)
         (when (and file (equal uri (plist-get orpheus-ui--art :uri)))
           (setq orpheus-ui--art (list :uri uri :file file))
           (orpheus-ui--redraw)))))))

(provide 'orpheus-ui)

;;; orpheus-ui.el ends here
