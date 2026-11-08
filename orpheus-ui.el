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
(require 'orpheus-playlist)

;;; Look

(defface orpheus-status-face
  '((((background dark)) :inherit bold :foreground "#e6c78a")
    (((background light)) :inherit bold :foreground "#6b4f1f")
    (t :inherit bold))
  "Face of the status line: gold on a dark frame, bronze on a light one."
  :group 'orpheus)

(defface orpheus-title-face
  '((((background dark)) :inherit default :foreground "#ece5d8")
    (((background light)) :inherit default :foreground "#2b2622")
    (t :inherit default))
  "Face of a track's title: marble in either light."
  :group 'orpheus)

(defface orpheus-artist-face
  '((((background dark)) :inherit font-lock-variable-name-face
     :foreground "#93b8e6")
    (((background light)) :inherit font-lock-variable-name-face
     :foreground "#2f5d8a")
    (t :inherit font-lock-variable-name-face))
  "Face of artists, playlists and other named things: Aegean blue."
  :group 'orpheus)

(defface orpheus-album-face
  '((((background dark)) :inherit font-lock-string-face
     :foreground "#c3aede")
    (((background light)) :inherit font-lock-string-face
     :foreground "#5b4a7a")
    (t :inherit font-lock-string-face))
  "Face of album names: iris."
  :group 'orpheus)

(defface orpheus-current-face
  '((t :inherit highlight :weight bold :extend t))
  "Face of the row that is playing."
  :group 'orpheus)

(defface orpheus-tab-face
  '((t :inherit shadow))
  "Face of a tab that is not selected."
  :group 'orpheus)

(defface orpheus-tab-active-face
  '((((background dark)) :inherit bold :underline t :foreground "#e6c78a")
    (((background light)) :inherit bold :underline t :foreground "#6b4f1f")
    (t :inherit bold :underline t))
  "Face of the selected tab: gold, underlined, bold."
  :group 'orpheus)

(defface orpheus-muted-face
  '((t :inherit shadow))
  "Face of separators, hints and other quiet text."
  :group 'orpheus)

(defface orpheus-directory-face
  '((((background dark)) :foreground "#b7c98a")
    (((background light)) :foreground "#4f5d2a")
    (t :inherit dired-directory))
  "Face of a directory row: olive, the colour of a grove."
  :group 'orpheus)

(defface orpheus-error-face
  '((t :inherit error))
  "Face of a failure or a disconnection."
  :group 'orpheus)

(defface orpheus-marked-face
  '((t :inherit warning :weight bold))
  "Face of the mark column of a marked row."
  :group 'orpheus)

(defface orpheus-progress-filled-face
  '((((background dark)) :foreground "#e6c78a" :weight bold)
    (((background light)) :foreground "#8a6420" :weight bold)
    (t :inherit bold))
  "Face of the played part of the progress bar."
  :group 'orpheus)

(defface orpheus-progress-empty-face
  '((t :inherit shadow))
  "Face of the remaining part of the progress bar."
  :group 'orpheus)

(defface orpheus-download-face
  '((((background dark)) :foreground "#7fd1c8")
    (((background light)) :foreground "#236e67")
    (t :inherit default))
  "Face of a completed download."
  :group 'orpheus)

(defface orpheus-download-active-face
  '((((background dark)) :inherit bold :foreground "#a8e6df")
    (((background light)) :inherit bold :foreground "#155b55")
    (t :inherit bold))
  "Face of a download that is still running."
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

(defcustom orpheus-ui-show-now-playing t
  "Whether to draw the now-playing card above the list."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ui-art-size 220
  "Longest side of the cover art, in pixels."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ui-progress-width 16
  "Width of the progress bar in the status line, in columns."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ui-icons t
  "Whether each row starts with a glyph for what it is.
`orpheus-ui-plain-ascii' turns the glyphs into letters, and a font without
symbols is the reason to turn them off."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ui-add-confirm-threshold 250
  "Number of rows `orpheus-ui-add' and `orpheus-ui-add-all' queue before asking.
The files tab can hold a whole flat music library, and queueing all of it with
one keystroke is what turns a queue into thousands of rows."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ui-queue-warning-size 1000
  "Track count at which loading the queue says how to clear it."
  :type 'integer :group 'orpheus)

(defvar-local orpheus-ui--downloads nil
  "Rows in the downloads tab, kept for the header summary.")

;;; Glyphs and strings

(defun orpheus-status-glyph (status)
  "Return the glyph that stands for STATUS's state."
  (let ((plain orpheus-ui-plain-ascii))
    (pcase (and status (orpheus-status-state status))
           ('play (if plain ">" "▶"))
           ('pause (if plain "||" "⏸"))
           (_ (if plain "." "⏹")))))

(defconst orpheus-ui--icon-alist
  '((directory . ("📁" . "d"))
    (track . ("🎵" . "m"))
    (album . ("💿" . "a"))
    (artist . ("🎤" . "@"))
    (albumartist . ("🎤" . "@"))
    (playlist . ("🎶" . "p"))
    (output . ("🔈" . "o"))
    (download . ("⬇" . "v"))
    (search . ("🔎" . "?")))
  "Glyph of each row kind, as (UNICODE . ASCII).")

(defun orpheus-ui--icon (kind)
  "Return the glyph that marks KIND, or nil when icons are off."
  (when orpheus-ui-icons
    (let ((pair (alist-get kind orpheus-ui--icon-alist)))
      (if orpheus-ui-plain-ascii (cdr pair) (car pair)))))

(defun orpheus-ui--flag (on letter)
  "Return LETTER when ON, and a blank column otherwise."
  (if on letter " "))

(defun orpheus-ui--download-summary-string ()
  "Return a short live summary of the rows in `orpheus-ui--downloads'."
  (let* ((items orpheus-ui--downloads)
         (active (cl-count-if
                  (lambda (item)
                    (eq (alist-get 'state (orpheus-item-data item))
                        'downloading))
                  items))
         (queued (cl-count-if
                  (lambda (item)
                    (eq (alist-get 'state (orpheus-item-data item)) 'queued))
                  items))
         (speed (cl-loop for item in items
                         when (eq (alist-get 'state (orpheus-item-data item))
                                  'downloading)
                         sum (or (alist-get 'speed (orpheus-item-data item)) 0))))
    (when (> (+ active queued) 0)
      (concat "  " (if orpheus-ui-plain-ascii "v" "↓") " "
              (number-to-string (+ active queued))
              (when (> active 0)
                (format " %s/s"
                        (file-size-human-readable (round speed) 'iec)))))))

(defun orpheus-ui--progress-string (elapsed total width)
  "Return a two-face progress bar WIDTH columns wide, from ELAPSED and TOTAL."
  (let* ((width (max 1 width))
         (bar (orpheus-progress-bar elapsed total width))
         (filled (min width
                      (round (* width
                                (/ (orpheus-progress-percent elapsed total)
                                   100.0))))))
    (concat (propertize (substring bar 0 filled)
                        'face 'orpheus-progress-filled-face)
            (propertize (substring bar filled)
                        'face 'orpheus-progress-empty-face))))

(defun orpheus-ui--status-string (status track)
  "Return the one-line status of STATUS and TRACK."
  (let ((elapsed (and status (orpheus-status-elapsed status)))
        (total (and status (orpheus-status-total status)))
        (flags (concat (orpheus-ui--flag (and status (orpheus-status-repeat status)) "R")
                       (orpheus-ui--flag (and status (orpheus-status-random status)) "Z")
                       (orpheus-ui--flag (and status (orpheus-status-single status)) "S")
                       (orpheus-ui--flag (and status (orpheus-status-consume status)) "C"))))
    (format " %s  %s  %s / %s  [%s] %3d%%  vol %d  %s%s"
            (orpheus-status-glyph status)
            (if (and track (not (string-empty-p (orpheus-track-label track))))
              (orpheus-track-label track)
              (if status "nothing playing" "connecting…"))
            (orpheus-format-time elapsed)
            (orpheus-format-time total)
            (orpheus-ui--progress-string elapsed total orpheus-ui-progress-width)
            (orpheus-progress-percent elapsed total)
            (if status (orpheus-status-volume status) 0)
            flags
            (or (orpheus-ui--download-summary-string) ""))))

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
         ('downloads "Downloads")
         ('directories "Directories")
         ('artists "Artists")
         ('albumartists "Album Artists")
         ('albums "Albums")
         ('playlists "Playlists")
         ('search "Search")
         (_ (capitalize (symbol-name kind)))))

(defconst orpheus-ui--tab-icons
  '((queue . ("♫" . "Q"))
    (downloads . ("⬇" . "D"))
    (directories . ("📁" . "d"))
    (artists . ("🎤" . "@"))
    (albumartists . ("🎤" . "@"))
    (albums . ("💿" . "a"))
    (playlists . ("🎶" . "p"))
    (search . ("🔎" . "s")))
  "Glyph of each tab kind, as (UNICODE . ASCII).")

(defun orpheus-ui--tab-label (kind title)
  "Return TITLE for KIND with its tab glyph when icons are on."
  (let ((pair (and orpheus-ui-icons (alist-get kind orpheus-ui--tab-icons))))
    (if pair
        (format "%s %s" (if orpheus-ui-plain-ascii (cdr pair) (car pair)) title)
      title)))

(defun orpheus-ui--tabs-for-capabilities (capabilities)
  "Return the tabs a backend with CAPABILITIES gets, as (KIND . TITLE).
The queue is always there, and so are portable playlists, which do not depend
on what CAPABILITIES offers."
  (let ((capabilities (if (and capabilities
                              (not (memq 'playlists capabilities)))
                          (cons 'playlists capabilities)
                        capabilities)))
    (delq nil
          (mapcar (lambda (kind)
                    (when (or (eq kind 'queue) (memq kind capabilities))
                      (cons kind (orpheus-ui-kind-title kind))))
                  orpheus-backend-browse-kinds))))

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
  (loading nil)
  (loaded nil)
  (seq 0)
  (marks nil))

(defvar-local orpheus-ui--tabs nil
              "Vector of `orpheus-ui-tab' in this buffer.")

(defvar-local orpheus-ui--tab 0
              "Index of the tab this buffer shows.")

(defvar-local orpheus-ui--backend nil
              "The backend the tabs were built for.")

(defvar-local orpheus-ui--render-key nil
              "What the buffer was last drawn from, so an unchanged view is left alone.")

(defvar orpheus-ui--art nil
  "Plist of the cover art in use: :uri, :loading and, once fetched, :file.")

(defvar orpheus-ui--art-cache (make-hash-table :test #'equal)
  "Cover file of each URI already fetched, so a cover is fetched once.")

(defvar orpheus-ui--queue-warned nil
  "Whether a queue over `orpheus-ui-queue-warning-size' was reported.")

(defvar orpheus-ui--needs-refresh t
  "Whether the player buffer must ask the backend for its rows again.")

(defun orpheus-ui-invalidate ()
  "Ask the player to read its current tab again on the next tick."
  (setq orpheus-ui--needs-refresh t))

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

(defun orpheus-ui--item-key (item)
  "Return the key ITEM is marked under, or nil for a row with no action."
  (and item
       (or (alist-get 'uri (orpheus-item-data item))
           (alist-get 'id (orpheus-item-data item))
           (orpheus-item-label item))))

(defun orpheus-ui--item-uris (item)
  "Return the queue URIs ITEM contributes, or nil.
A portable playlist row contributes every URI in its file; a track contributes
its own URI.  A server playlist has no URI list until it is opened."
  (let ((file (alist-get 'file (orpheus-item-data item))))
    (cond
     (file
      (condition-case err
          (orpheus-playlist-uris file orpheus-backend-current)
        (error
         (message "orpheus: cannot read %s: %s" file
                  (error-message-string err))
         nil)))
     ((orpheus-item-playable item)
      (list (alist-get 'uri (orpheus-item-data item)))))))

(defun orpheus-ui--marked-p (item)
  "Return non-nil when ITEM is marked in the current tab."
  (let* ((tab (orpheus-ui--current-tab))
         (key (orpheus-ui--item-key item)))
    (and tab key (member key (orpheus-ui-tab-marks tab)) t)))

(defun orpheus-ui--marked-items ()
  "Return the marked rows of the current tab, in the order they appear."
  (let ((tab (orpheus-ui--current-tab)))
    (when tab
      (let ((marks (orpheus-ui-tab-marks tab)))
        (delq nil
              (mapcar (lambda (item)
                        (let ((key (orpheus-ui--item-key item)))
                          (when (and key (member key marks)) item)))
                      (orpheus-ui-tab-items tab)))))))

(defun orpheus-ui--parent-item (tab)
  "Return the row that leaves TAB's directory, or nil at the top.
A top-level directory's parent is the root, which is the empty path on every
backend."
  (let ((dir (and (eq (orpheus-ui-tab-view tab) 'directories)
                  (car (orpheus-ui-tab-arg tab)))))
    (when (and (stringp dir) (not (string-empty-p dir)))
      (let ((parent (or (file-name-directory (directory-file-name dir)) "")))
        (unless (equal (directory-file-name parent)
                       (directory-file-name dir))
          (orpheus-item-create
           :label "../" :kind 'directory
           :data (list (cons 'browse (cons 'directories (list parent))))))))))

;;; Drawing

(defvar orpheus-ui-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-ui-mouse-select)
    (define-key map [mouse-2] 'orpheus-ui-mouse-play)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each row, so the mouse works on it.")

(defvar orpheus-ui-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-ui-mouse-tab)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each tab, so the mouse can select it.")

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
      (push (propertize (format "%d %s" (1+ index)
                                (orpheus-ui--tab-label
                                 (orpheus-ui-tab-kind tab)
                                 (orpheus-ui-tab-title tab)))
                        'face (if (= index orpheus-ui--tab)
                                'orpheus-tab-active-face
                                'orpheus-tab-face)
                        'orpheus-tab index
                        'local-map orpheus-ui-tab-map
                        'mouse-face 'highlight)
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
      (let* ((window (get-buffer-window))
             (max (if window
                      (max 1 (min orpheus-ui-art-size
                                  (window-body-width window t)))
                    orpheus-ui-art-size)))
        (ignore-errors
          (create-image file nil nil :max-width max :max-height max
                        :ascent 'center))))))

(defun orpheus-ui--insert-art (width)
  "Insert the cover art centred in WIDTH, or keep its place while it loads."
  (let ((image (orpheus-ui--art-image)))
    (cond
      (image
       (let* ((size (ignore-errors (image-size image t)))
              (columns (if size
                           (ceiling (/ (float (car size))
                                       (max 1 (frame-char-width))))
                         1))
              (indent (max 0 (/ (- width columns) 2))))
         (insert (make-string indent ?\s))
         (insert-image image " ")
         (insert "\n")))
      ((and (plist-get orpheus-ui--art :loading) (display-graphic-p))
       ;; The list must not jump up and back down when the cover arrives.
       (insert (propertize " " 'display
                           (list 'space :width 1 :height orpheus-ui-art-size))
               "\n")))))

(defun orpheus-ui--center-line (text width face)
  "Return TEXT centred in WIDTH, with FACE."
  (let* ((text (orpheus-ui--elide text width))
         (indent (max 0 (/ (- width (string-width text)) 2))))
    (concat (make-string indent ?\s) (propertize text 'face face))))

(defun orpheus-ui--insert-now-playing (width)
  "Insert a centered now-playing card in WIDTH."
  (let* ((track orpheus-track-current)
         (status orpheus-status-current)
         (title (or (and track (orpheus-track-title track))
                    (and track (orpheus-track-label track))
                    "unknown track"))
         (artist (and track (orpheus-track-artist track)))
         (album (and track (orpheus-track-album track)))
         (sub (string-join (delq nil (list artist album))
                           (if orpheus-ui-plain-ascii " - " " · ")))
         (elapsed (and status (orpheus-status-elapsed status)))
         (total (and status (orpheus-status-total status))))
    (insert (orpheus-ui--center-line
             (format "%s  %s" (orpheus-status-glyph status) title)
             width 'orpheus-title-face)
            "\n")
    (unless (string-empty-p sub)
      (insert (orpheus-ui--center-line sub width 'orpheus-artist-face) "\n"))
    (insert (orpheus-ui--center-line
             (format "%s  %s / %s"
                     (orpheus-ui--progress-string
                      elapsed total orpheus-ui-progress-width)
                     (orpheus-format-time elapsed)
                     (orpheus-format-time total))
             width 'orpheus-muted-face)
            "\n\n")))

(defun orpheus-ui--current-p (item)
  "Return non-nil when ITEM is the row playing now."
  (let* ((data (orpheus-item-data item))
         (id (alist-get 'id data))
         (playing (and orpheus-status-current
                       (orpheus-status-songid orpheus-status-current))))
    (and id playing (equal id playing))))

(defun orpheus-ui--insert-item (item index width)
  "Insert ITEM, the INDEXth row, fitted into WIDTH.
The row starts with a mark column and the glyph of its kind, so a mark and a
folder are read at a glance the way Dired shows them."
  (let* ((kind (orpheus-item-kind item))
         (current (orpheus-ui--current-p item))
         (marked (orpheus-ui--marked-p item))
         (icon (orpheus-ui--icon kind))
         (prefix (concat (if marked ">" " ")
                         (when icon icon)
                         (when icon " ")))
         (face (pcase kind
                      ('directory 'orpheus-directory-face)
                      ('album 'orpheus-album-face)
                      ('playlist 'orpheus-artist-face)
                      ('artist 'orpheus-artist-face)
                      ('albumartist 'orpheus-artist-face)
                      ('output 'orpheus-muted-face)
                      ('download
                       (pcase (alist-get 'state (orpheus-item-data item))
                         ('downloading 'orpheus-download-active-face)
                         ('failed 'orpheus-error-face)
                         (_ 'orpheus-download-face)))
                      (_ 'orpheus-title-face)))
         (row (orpheus-ui--columns (concat prefix
                                           (or (orpheus-item-label item) ""))
                                   (or (orpheus-item-detail item) "")
                                   width))
         (start (point)))
    (insert (propertize row
                        'face (if current 'orpheus-current-face face)
                        'orpheus-item index
                        'orpheus-item-kind kind
                        'orpheus-marked marked
                        'mouse-face 'highlight
                        'help-echo (orpheus-item-label item)))
    (when marked
      (add-text-properties start (1+ start) '(face orpheus-marked-face)))
    (insert "\n")
    (add-text-properties start (1- (point))
                         (list 'keymap orpheus-ui-item-map))))

(defun orpheus-ui--footer (kind)
  "Return the hint line for the current tab KIND."
  (pcase kind
         ('downloads
          " RET play or retry  d cancel  g refresh  ? help  q bury")
         ('playlists
          " RET play  e edit  a add  d delete  C-x C-s save queue  g refresh  ? help  q bury")
         ('search
          " RET play  a add  m mark  / search again  g refresh  ? help  q bury")
         (_
          " RET play  a add  m mark  d delete  / search  g refresh  ? help  q bury")))

(defun orpheus-ui--render ()
  "Draw the buffer from the tabs and rows it holds.
The window start is put back afterwards, so a redraw while a cover arrives
does not scroll the list under the reader."
  (let* ((inhibit-read-only t)
         (window (get-buffer-window))
         (start (and window (window-start window)))
         (tab (orpheus-ui--current-tab))
         (width (max 24 (if window (window-body-width window) 80))))
    (erase-buffer)
    (insert (orpheus-ui--tab-string) "\n")
    (when (orpheus-ui-tab-arg tab)
      (insert (propertize (orpheus-ui--path-string) 'face 'orpheus-muted-face)
              "\n"))
    (when orpheus-ui-show-art
      (orpheus-ui--insert-art width))
    (when (and orpheus-ui-show-now-playing orpheus-track-current)
      (orpheus-ui--insert-now-playing width))
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
    (insert (propertize (orpheus-ui--footer (and tab (orpheus-ui-tab-kind tab)))
                        'face 'orpheus-muted-face))
    (orpheus-ui--place-point)
    (when (and window start (<= start (point-max)))
      (set-window-start window start))
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
          (plist-get orpheus-ui--art :key)
          (plist-get orpheus-ui--art :file)
          (orpheus-ui--download-summary-string)
          orpheus-ui--downloads)))

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
  "Remember ITEMS as TAB's rows, with the parent row in front of a directory."
  (let ((parent (orpheus-ui--parent-item tab)))
    (setf (orpheus-ui-tab-items tab) (if parent (cons parent items) items)
          (orpheus-ui-tab-loading tab) nil
          (orpheus-ui-tab-loaded tab) t)
    (when (>= (orpheus-ui-tab-index tab)
              (max 1 (length (orpheus-ui-tab-items tab))))
      (setf (orpheus-ui-tab-index tab)
            (max 0 (1- (length (orpheus-ui-tab-items tab))))))
    (when (and (eq (orpheus-ui-tab-view tab) 'queue)
               (> (length items) orpheus-ui-queue-warning-size)
               (not orpheus-ui--queue-warned))
      (setq orpheus-ui--queue-warned t)
      (message "orpheus: the queue has %d tracks; D clears it"
               (length items)))))

(defun orpheus-ui--set-downloads (items)
  "Remember ITEMS as the current transfer rows and redraw."
  (setq orpheus-ui--downloads items)
  (let ((tab (orpheus-ui--current-tab)))
    (when (and tab (eq (orpheus-ui-tab-view tab) 'downloads))
      (setf (orpheus-ui-tab-items tab) items
            (orpheus-ui-tab-loading tab) nil
            (orpheus-ui-tab-loaded tab) t)))
  (orpheus-ui--redraw))

(defun orpheus-ui--refresh-downloads ()
  "Refresh the transfer rows and the header summary."
  (let ((backend orpheus-backend-current)
        (buffer (current-buffer)))
    (when (and backend
               (memq 'downloads (orpheus-backend-capabilities backend))
               (orpheus-backend-connected-p backend))
      (orpheus-backend-downloads
       backend
       (lambda (items)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (orpheus-ui--set-downloads items))))))))

(defun orpheus-ui--playlist-items (callback)
  "Read portable playlists, then any server playlists, and call CALLBACK.
Portable rows come first, so a backend that also keeps playlists on its server
adds to the same tab instead of replacing it."
  (let ((portable (orpheus-playlist-items))
        (backend orpheus-backend-current))
    (if (and backend
             (memq 'playlists (orpheus-backend-capabilities backend)))
        (orpheus-backend-browse
         backend 'playlists nil
         (lambda (server)
           (funcall callback (append portable server))))
      (funcall callback portable))))

(defun orpheus-ui--load-tab (tab callback)
  "Read TAB's rows and then call CALLBACK, in this buffer.
Return non-nil when a read was asked for.  A reply that arrives after a newer
read started is dropped, so a slow daemon cannot overwrite a fresh list with
an older one."
  (let ((backend orpheus-backend-current)
        (buffer (current-buffer)))
    (when (and backend (orpheus-backend-connected-p backend))
      (let* ((seq (1+ (orpheus-ui-tab-seq tab)))
             (done (lambda (items)
                    (when (and (buffer-live-p buffer)
                               (= seq (orpheus-ui-tab-seq tab)))
                      (with-current-buffer buffer
                        (orpheus-ui--set-items tab items)
                        (when (eq (orpheus-ui-tab-view tab) 'downloads)
                          (setq orpheus-ui--downloads items))
                        (funcall callback))))))
        (setf (orpheus-ui-tab-seq tab) seq
              (orpheus-ui-tab-loading tab) t)
        (cond
         ((eq (orpheus-ui-tab-view tab) 'queue)
          (orpheus-backend-queue backend done))
         ((eq (orpheus-ui-tab-view tab) 'downloads)
          (orpheus-backend-downloads backend done))
         ((eq (orpheus-ui-tab-view tab) 'playlists)
          (orpheus-ui--playlist-items done))
         (t
          (orpheus-backend-browse backend (orpheus-ui-tab-view tab)
                                  (orpheus-ui-tab-arg tab) done)))
        t))))

(defun orpheus-ui-refresh (&optional force)
  "Read the current tab again and redraw when it changed.
Unless FORCE is non-nil, nothing is read while nothing invalidated the tab:
the status timer asks every second, and fetching a queue of thousands on each
ask is what makes an editor stutter."
  (interactive)
  (let ((buffer (get-buffer orpheus-ui-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((tab (orpheus-ui--current-tab)))
          (when (and tab
                     (or force orpheus-ui--needs-refresh
                         (not (orpheus-ui-tab-loaded tab))))
            (when (orpheus-ui--load-tab tab #'orpheus-ui--redraw)
              (setq orpheus-ui--needs-refresh nil))))))))

(defun orpheus-ui-tick ()
  "Bring the player buffer in step with the status.
The rows are read only when something invalidated them; otherwise one redraw
of the tab that is already there is the whole tick.  Transfers are cheap to
read and are refreshed every tick so their bytes and the header summary move."
  (let ((buffer (get-buffer orpheus-ui-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (orpheus-ui--refresh-downloads)
        (orpheus-ui--update-context-art)
        (if orpheus-ui--needs-refresh
            (orpheus-ui-refresh)
          (orpheus-ui--redraw))))))

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

(defun orpheus-ui-mark ()
  "Mark the row at point and move to the next one, as Dired's `m'."
  (interactive)
  (let ((tab (orpheus-ui--current-tab))
        (item (orpheus-ui--item-at-point)))
    (if (not (and tab item (orpheus-ui--item-key item)))
        (message "orpheus: nothing to mark here")
      (let ((key (orpheus-ui--item-key item)))
        (unless (member key (orpheus-ui-tab-marks tab))
          (push key (orpheus-ui-tab-marks tab)))
        (orpheus-ui--render)
        (orpheus-ui-next-line)))))

(defun orpheus-ui-unmark ()
  "Unmark the row at point and move to the next one, as Dired's `u'."
  (interactive)
  (let ((tab (orpheus-ui--current-tab))
        (item (orpheus-ui--item-at-point)))
    (if (not (and tab item (orpheus-ui--item-key item)))
        (message "orpheus: nothing to unmark here")
      (setf (orpheus-ui-tab-marks tab)
            (delete (orpheus-ui--item-key item) (orpheus-ui-tab-marks tab)))
      (orpheus-ui--render)
      (orpheus-ui-next-line))))

(defun orpheus-ui-unmark-all ()
  "Remove every mark in this tab, as Dired's `U'."
  (interactive)
  (let ((tab (orpheus-ui--current-tab)))
    (when tab
      (setf (orpheus-ui-tab-marks tab) nil)
      (orpheus-ui--render))))

(defun orpheus-ui-toggle-marks ()
  "Mark every unmarked row and unmark every marked one, as Dired does."
  (interactive)
  (let ((tab (orpheus-ui--current-tab)))
    (when tab
      (let ((marks (orpheus-ui-tab-marks tab)))
        (setf (orpheus-ui-tab-marks tab)
              (delq nil
                    (mapcar (lambda (item)
                              (let ((key (orpheus-ui--item-key item)))
                                (and key (not (member key marks)) key)))
                            (orpheus-ui-tab-items tab)))))
      (orpheus-ui--render))))

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
      (orpheus-ui-invalidate)
      (orpheus-ui--render)
      (orpheus-ui-refresh))))

(defun orpheus-ui-next-tab ()
  "Show the next tab."
  (interactive)
  (when (vectorp orpheus-ui--tabs)
    (setq orpheus-ui--tab (% (1+ orpheus-ui--tab)
                             (length orpheus-ui--tabs)))
    (orpheus-ui-invalidate)
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-previous-tab ()
  "Show the previous tab."
  (interactive)
  (when (vectorp orpheus-ui--tabs)
    (setq orpheus-ui--tab (% (+ (1- orpheus-ui--tab)
                                (length orpheus-ui--tabs))
                             (length orpheus-ui--tabs)))
    (orpheus-ui-invalidate)
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-select-tab (n)
  "Show the Nth tab."
  (interactive "nTab: ")
  (when (and (vectorp orpheus-ui--tabs)
             (> n 0) (<= n (length orpheus-ui--tabs)))
    (setq orpheus-ui--tab (1- n))
    (orpheus-ui-invalidate)
    (orpheus-ui--render)
    (orpheus-ui-refresh)))

(defun orpheus-ui-confirm ()
  "Play the row at point, or descend into it."
  (interactive)
  (let ((item (orpheus-ui--item-at-point))
        (tab (orpheus-ui--current-tab)))
    (cond
      ((null item) (message "orpheus: no row here"))
      ((and (eq (orpheus-ui-tab-view tab) 'playlists)
            (alist-get 'file (orpheus-item-data item)))
       (let ((file (alist-get 'file (orpheus-item-data item))))
         (condition-case err
             (let ((count (orpheus-playlist-play file)))
               (orpheus-ui-invalidate)
               (if (> count 0)
                   (message "orpheus: loaded %d track(s)" count)
                 (message "orpheus: playlist is empty")))
           (error
            (message "orpheus: cannot load %s: %s" file
                     (error-message-string err))))))
      ((orpheus-item-browse item)
       (pcase-let ((`(,kind . ,arg) (orpheus-item-browse item)))
         (setf (orpheus-ui-tab-stack tab)
               (cons (list (orpheus-ui-tab-view tab) (orpheus-ui-tab-arg tab))
                     (orpheus-ui-tab-stack tab)))
         (setf (orpheus-ui-tab-view tab) kind
               (orpheus-ui-tab-arg tab) (if (listp arg) arg (list arg))
               (orpheus-ui-tab-index tab) 0)
         (orpheus-ui-invalidate)
         (orpheus-ui--render)
         (orpheus-ui-refresh)))
      ((eq (orpheus-ui-tab-view tab) 'downloads)
       (let ((uri (alist-get 'uri (orpheus-item-data item)))
             (state (alist-get 'state (orpheus-item-data item))))
         (if uri
             (progn
               (orpheus-backend-play-uri orpheus-backend-current uri)
               (message "orpheus: %s"
                        (pcase state
                          ('downloading "will play when the download finishes")
                          ('queued "queued to play")
                          ('failed "retrying the download")
                          (_ "playing from the cache"))))
           (message "orpheus: this transfer has no track"))))
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
               (orpheus-ui-invalidate)
               (orpheus-ui--render)
               (orpheus-ui-refresh))
             (message "orpheus: already at the top"))))

(defun orpheus-ui--confirm-add (count)
  "Return non-nil when COUNT rows may be queued without asking."
  (or (<= count orpheus-ui-add-confirm-threshold)
      (y-or-n-p (format "Queue %d tracks? " count))))

(defun orpheus-ui-add ()
  "Add the marked rows, or the row at point, to the queue.
The queue tab is not a source: a queue row added to the queue appends the
whole queue to itself."
  (interactive)
  (let* ((tab (orpheus-ui--current-tab))
         (items (or (orpheus-ui--marked-items)
                    (list (orpheus-ui--item-at-point))))
         (uris (delq nil (mapcan #'orpheus-ui--item-uris items))))
    (cond
      ((eq (orpheus-ui-tab-kind tab) 'queue)
       (message "orpheus: this is the queue already; nothing to add"))
      ((null uris) (message "orpheus: nothing to queue here"))
      ((not (orpheus-ui--confirm-add (length uris)))
       (message "orpheus: kept the queue as it was"))
      (t
        (orpheus-backend-add orpheus-backend-current
                             (if (= 1 (length uris)) (car uris) uris))
        (message "orpheus: queued %d track(s)" (length uris))
        (orpheus-ui-invalidate)))))

(defun orpheus-ui-add-all ()
  "Add every playable row of this tab to the queue.
Like `orpheus-ui-add', this does nothing on the queue tab, where every row
is a copy of a row that is there already."
  (interactive)
  (let ((uris (delq nil (mapcan #'orpheus-ui--item-uris
                                (orpheus-ui--current-items))))
        (tab (orpheus-ui--current-tab)))
    (cond
      ((eq (orpheus-ui-tab-kind tab) 'queue)
       (message "orpheus: this is the queue already; nothing to add"))
      ((not (orpheus-ui--confirm-add (length uris)))
       (message "orpheus: kept the queue as it was"))
      (uris
        (orpheus-backend-add orpheus-backend-current uris)
        (message "orpheus: queued %d track(s)" (length uris))
        (orpheus-ui-invalidate))
      (t (message "orpheus: nothing to queue here")))))

(defun orpheus-ui-delete ()
  "Remove queue rows, or cancel transfer rows, at point or marked."
  (interactive)
  (let* ((tab (orpheus-ui--current-tab))
         (items (or (orpheus-ui--marked-items)
                    (delq nil (list (orpheus-ui--item-at-point))))))
    (cond
      ((null items) (message "orpheus: no row here"))
      ((and (eq (orpheus-ui-tab-view tab) 'playlists)
            (cl-some (lambda (item)
                       (alist-get 'file (orpheus-item-data item)))
                     items))
       (let ((files (delq nil
                          (mapcar (lambda (item)
                                    (alist-get 'file (orpheus-item-data item)))
                                  items))))
         (if (y-or-n-p (format "Delete %d playlist(s)? " (length files)))
             (progn
               (dolist (file files)
                 (orpheus-playlist-delete file))
               (orpheus-ui-invalidate)
               (orpheus-ui-refresh t))
           (message "orpheus: kept the playlist(s)"))))
      ((eq (orpheus-ui-tab-view tab) 'downloads)
       (dolist (item items)
         (when-let* ((uri (alist-get 'uri (orpheus-item-data item))))
                    (orpheus-backend-cancel-download orpheus-backend-current uri)))
       (orpheus-ui-invalidate)
       (orpheus-ui-refresh t))
      ((not (eq (orpheus-ui-tab-view tab) 'queue))
       (message "orpheus: only the queue can be edited here"))
      (t
        (let ((positions (delq nil
                               (mapcar (lambda (item)
                                         (or (alist-get 'pos (orpheus-item-data item))
                                             (orpheus-ui-tab-index tab)))
                                       items))))
          ;; Higher positions first, so the lower ones keep their meaning.
          (dolist (pos (sort positions #'>))
            (orpheus-backend-command orpheus-backend-current 'delete pos))
          (orpheus-ui-invalidate))))))

(defun orpheus-ui-clear ()
  "Empty the queue, after asking."
  (interactive)
  (when (y-or-n-p "Clear the queue? ")
    (orpheus-backend-command orpheus-backend-current 'clear)
    (orpheus-ui-invalidate)))

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
      (orpheus-ui-invalidate)
      (orpheus-ui--render)
      (orpheus-ui-refresh))))

(defun orpheus-ui-save-playlist (name)
  "Save the queue as the portable playlist NAME."
  (interactive (list (read-string "Save queue as playlist: ")))
  (when (string-empty-p name)
    (user-error "No playlist name"))
  (orpheus-playlist-save-queue
   name
   (lambda (file)
     (if file
         (progn
           (orpheus-ui-invalidate)
           (message "orpheus: saved %s" (file-name-nondirectory file)))
       (message "orpheus: the queue is empty")))))

(defun orpheus-ui-edit-playlist ()
  "Edit the portable playlist at point in its own buffer."
  (interactive)
  (let ((file (alist-get 'file (orpheus-item-data
                                (orpheus-ui--item-at-point)))))
    (if file
        (orpheus-playlist-edit file)
      (message "orpheus: only portable playlists can be edited"))))

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

(defun orpheus-ui-mouse-tab (event)
  "Select the tab clicked on in EVENT."
  (interactive "e")
  (let* ((position (posn-point (event-start event)))
         (index (and (integer-or-marker-p position)
                     (get-text-property position 'orpheus-tab))))
    (when (integerp index)
      (orpheus-ui-select-tab (1+ index)))))

;;; Help

(defconst orpheus-ui--help-lines
  '(    ("RET" "play the track or playlist, or open the album, artist or directory")
    ("n / C-n / j" "next row")
    ("p / C-p / k" "previous row")
    ("C-v / M-v" "one screen down and up")
    ("M-< / M->" "first and last row")
    ("TAB / S-TAB" "next and previous tab")
    ("1 … 7" "the tabs, in the order shown; SSH adds Downloads")
    ("Downloads tab" "transfers: RET plays or retries, d cancels")
    ("^ / C-b" "back up one level")
    ("m / u" "mark and unmark the row")
    ("U" "unmark every row")
    ("t" "invert the marks")
    ("a / A" "queue the marked rows, or this row; queue everything here")
    ("e" "edit a portable playlist in its own buffer")
    ("d / C-k" "delete from the queue, remove a playlist, or cancel a download")
    ("D" "clear the queue")
    ("/ / C-s" "search")
    ("C" "jump to the playing row")
    ("?" "this help")
    ("SPC" "play or pause")
    ("s" "stop")
    ("> / <" "next and previous track")
    ("f / b" "seek forward and back")
    ("M-<up> / M-<down>, + / -" "volume up and down")
    ("z / x / c / v" "repeat, random, consume, single")
    ("M-u / M-U" "update the library, rescan it")
    ("C-x C-s" "save the queue as a portable M3U8 playlist")
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
    (define-key map (kbd "<down>") 'orpheus-ui-next-line)
    (define-key map (kbd "<up>") 'orpheus-ui-previous-line)
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
    (define-key map (kbd "^") 'orpheus-ui-up)
    (define-key map (kbd "m") 'orpheus-ui-mark)
    (define-key map (kbd "u") 'orpheus-ui-unmark)
    (define-key map (kbd "U") 'orpheus-ui-unmark-all)
    (define-key map (kbd "t") 'orpheus-ui-toggle-marks)
    (define-key map (kbd "a") 'orpheus-ui-add)
    (define-key map (kbd "A") 'orpheus-ui-add-all)
    (define-key map (kbd "e") 'orpheus-ui-edit-playlist)
    (define-key map (kbd "d") 'orpheus-ui-delete)
    (define-key map (kbd "C-k") 'orpheus-ui-delete)
    (define-key map (kbd "D") 'orpheus-ui-clear)
    (define-key map (kbd "/") 'orpheus-ui-search)
    (define-key map (kbd "C-s") 'orpheus-ui-search)
    (define-key map (kbd "C") 'orpheus-ui-jump-to-current)
    (define-key map (kbd "?") 'orpheus-ui-help)
    (define-key map (kbd "SPC") 'orpheus-play-pause)
    (define-key map (kbd "p") 'orpheus-ui-previous-line)
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
    (define-key map (kbd "M-u") 'orpheus-update-database)
    (define-key map (kbd "M-U") 'orpheus-rescan-database)
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
  (setq-local orpheus-ui--downloads nil)
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
    (orpheus-ui--update-context-art)
    (orpheus-ui-refresh)))

(defun orpheus-ui--art-first-uri (tab)
  "Return the first playable URI in TAB, or nil."
  (when tab
    (cl-loop for item in (orpheus-ui-tab-items tab)
             for uri = (alist-get 'uri (orpheus-item-data item))
             when (and uri (orpheus-item-playable item))
             return uri)))

(defun orpheus-ui--folder-art-request (callback)
  "Find cover art for the current folder or album, then call CALLBACK."
  (let ((player (and (not (local-variable-p 'orpheus-ui--tabs))
                     (get-buffer orpheus-ui-buffer-name))))
    (if (and player (not (eq player (current-buffer))) (buffer-live-p player))
        ;; Status callbacks run in a process buffer; the tabs are in the player.
        (with-current-buffer player
          (orpheus-ui--folder-art-request callback))
      (let* ((backend orpheus-backend-current)
             (tab (orpheus-ui--current-tab))
             (view (and tab (orpheus-ui-tab-view tab)))
             (folder (and (eq view 'directories)
                          (car (orpheus-ui-tab-arg tab))))
             (uri (and (memq view '(directories albums artists albumartists
                                              playlists))
                       (orpheus-ui--art-first-uri tab))))
        (cond
         ((null backend) (funcall callback nil))
         (folder
          (orpheus-backend-folder-art
           backend folder
           (lambda (file)
             (if (or file (null uri))
                 (funcall callback file)
               (orpheus-backend-album-art
                backend (orpheus-track-create :uri uri) callback)))))
         (uri
          (orpheus-backend-album-art
           backend (orpheus-track-create :uri uri) callback))
         (t (funcall callback nil)))))))

(defun orpheus-ui--show-art (key request &optional uri)
  "Show the cover art under KEY, fetching it with REQUEST when needed.
URI names the current track, when there is one."
  (let* ((backend orpheus-backend-current)
         (cached (and key (gethash key orpheus-ui--art-cache))))
    (cond
     ((or (null key) (null backend) (not orpheus-ui-show-art)
          (not (display-graphic-p)))
      (when orpheus-ui--art
        (setq orpheus-ui--art nil)
        (orpheus-ui--redraw)))
     ((equal key (plist-get orpheus-ui--art :key)))
     (cached
      (setq orpheus-ui--art (list :key key :uri uri :file cached))
      (orpheus-ui--redraw))
     (t
      (setq orpheus-ui--art (list :key key :uri uri :loading t))
      (orpheus-ui--redraw)
      (funcall request
               (lambda (file)
                 (when (equal key (plist-get orpheus-ui--art :key))
                   (when file (puthash key file orpheus-ui--art-cache))
                   (setq orpheus-ui--art (list :key key :uri uri :file file))
                   (orpheus-ui--redraw))))))))

(defun orpheus-ui--update-art (track)
  "Fetch cover art for TRACK, when it is not the one already shown.
A track with no art falls back to the folder being browsed.  A terminal has
nowhere to draw it, so nothing is fetched there, and a cover already cached is
put back without asking the backend a second time."
  (let* ((backend orpheus-backend-current)
         (uri (and track (orpheus-track-uri track)))
         (key (and backend uri
                   (format "%s:track:%s"
                           (orpheus-backend-name backend) uri))))
    (if key
        (orpheus-ui--show-art
         key
         (lambda (callback)
           (orpheus-backend-album-art
            backend track
            (lambda (file)
              (if file
                  (funcall callback file)
                (orpheus-ui--folder-art-request callback)))))
         uri)
      (orpheus-ui--show-art nil nil))))

(defun orpheus-ui--update-context-art ()
  "Show the cover for the current track, folder or album."
  (let* ((backend orpheus-backend-current)
         (track orpheus-track-current)
         (uri (and track (orpheus-track-uri track)))
         (tab (orpheus-ui--current-tab))
         (view (and tab (orpheus-ui-tab-view tab))))
    (cond
     ((and backend uri) (orpheus-ui--update-art track))
     ((and backend tab
           (memq view '(directories albums artists albumartists playlists)))
      (orpheus-ui--show-art
       (format "%s:%s:%S:%s" (orpheus-backend-name backend) view
               (orpheus-ui-tab-arg tab)
               (or (orpheus-ui--art-first-uri tab) ""))
       (lambda (callback) (orpheus-ui--folder-art-request callback))))
     (t (orpheus-ui--show-art nil nil)))))

(provide 'orpheus-ui)

;;; orpheus-ui.el ends here
