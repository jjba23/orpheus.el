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
(require 'orpheus-transient)
(require 'orpheus-equalizer)
(require 'button)

(declare-function orpheus "orpheus" ())

;; The image primitives live in C, so a build that does not preload image.el
;; still knows them at run time and the byte-compiler does not.
(declare-function image-size "image.c" (spec &optional pixels))

;; The seek bar sends its command through the session, which loads last.
(declare-function orpheus--command "orpheus" (action &optional value))

(defconst orpheus-ui--logo-file
  (expand-file-name "docs/orpheus-logo.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The project logo, as a PNG beside the package source.")

(defvar orpheus-ui--logo-image-cache nil
  "The scaled player logo, once this frame has made one.")

;;; Look

(defgroup orpheus-faces nil
  "Faces of the orpheus player.
Customize this group to give the player a palette of its own; every face
inherits a built-in face where it can, so a theme that does not know orpheus
still gives a readable player."
  :group 'orpheus
  :prefix "orpheus-")

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

(defface orpheus-button-face
  '((t :inherit shadow :box t))
  "Face of a control under the cover that is off or inert."
  :group 'orpheus)

(defface orpheus-button-on-face
  '((((background dark)) :inherit bold :box t :foreground "#e6c78a")
    (((background light)) :inherit bold :box t :foreground "#6b4f1f")
    (t :inherit bold :box t))
  "Face of a control under the cover that is on: gold, like the status line."
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

;; The faces keep `orpheus' as a group for `customize-group' and are gathered
;; under `orpheus-faces' so a reader looking for the look finds the whole set
;; in one place.
(dolist (face '(orpheus-status-face orpheus-title-face orpheus-artist-face
                orpheus-album-face orpheus-current-face orpheus-tab-face
                orpheus-tab-active-face orpheus-muted-face
                orpheus-directory-face orpheus-error-face orpheus-marked-face
                orpheus-progress-filled-face orpheus-progress-empty-face
                orpheus-button-face orpheus-button-on-face
                orpheus-download-face orpheus-download-active-face))
  (custom-add-to-group 'orpheus-faces face 'custom-face))

(defcustom orpheus-ui-buffer-name "*orpheus*"
  "Name of the player's buffer."
  :type 'string :group 'orpheus)

(defcustom orpheus-ui-plain-ascii nil
  "Whether to draw glyphs with ASCII instead of Unicode.
Set it in a terminal whose font has no play, pause or block characters."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ui-show-art t
  "Whether to draw cover art in a graphical frame.
The art keeps a box of `orpheus-ui-art-size', so the list below it does not
move while a cover loads and does not move when a track has none."
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

(defcustom orpheus-ui-display-action '(display-buffer-same-window)
  "How `orpheus-ui-open' shows the player.
The default reuses the selected window, so opening orpheus replaces what is
there instead of splitting the frame.  A value such as
`(display-buffer-pop-up-window)' gives the player a window of its own."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "New window" (display-buffer-pop-up-window))
                 (sexp :tag "A `display-buffer' action"))
  :group 'orpheus)

(defcustom orpheus-ui-cache-seconds 30
  "Seconds a loaded tab is reused before it is read again.
Switching tabs within this window costs no backend request, which is what
makes moving between a large queue and a directory feel instant.  `g' always
reads; set this to 0 to read on every switch."
  :type 'number :group 'orpheus)

(defcustom orpheus-ui-image-cache-size 64
  "Number of decoded cover images kept for redraws.
The cache is emptied when it fills, so a long session holds at most this many
images and decodes a cover again only after that."
  :type 'integer :group 'orpheus)

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

(defun orpheus-ui--status-string (status track &optional width)
  "Return the one-line status of STATUS and TRACK, fitted into WIDTH.
WIDTH is in columns; nil keeps the natural width."
  (let* ((elapsed (and status (orpheus-status-elapsed status)))
         (total (and status (orpheus-status-total status)))
         (flags (concat (orpheus-ui--flag (and status (orpheus-status-repeat status)) "R")
                        (orpheus-ui--flag (and status (orpheus-status-random status)) "Z")
                        (orpheus-ui--flag (and status (orpheus-status-single status)) "S")
                        (orpheus-ui--flag (and status (orpheus-status-consume status)) "C")))
         (left (format " %s  %s"
                       (orpheus-status-glyph status)
                       (if (and track (not (string-empty-p
                                            (orpheus-track-label track))))
                           (orpheus-track-label track)
                         (if status "nothing playing" "connecting…"))))
         (right (format "%s / %s  [%s] %3d%%  vol %d  %s%s"
                        (orpheus-format-time elapsed)
                        (orpheus-format-time total)
                        (orpheus-ui--progress-string
                         elapsed total orpheus-ui-progress-width)
                        (orpheus-progress-percent elapsed total)
                        (if status (orpheus-status-volume status) 0)
                        flags
                        (or (orpheus-ui--download-summary-string) ""))))
    (if width
        (orpheus-ui--columns left right width)
      (concat left "  " right))))

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
  (loaded-at 0)
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

(defvar orpheus-ui--image-cache (make-hash-table :test #'equal)
  "Decoded cover images, keyed by file, size, modification time and scale.
A redraw every second must not decode the same cover again.")

(defvar orpheus-ui--rendering nil
  "Whether the buffer is being drawn, so a redraw asked for now must wait.")

(defvar orpheus-ui--redraw-pending nil
  "Whether a redraw was asked for while the buffer was being drawn.")

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

(defun orpheus-ui--portable-uri (uri)
  "Return URI in the active backend's portable playlist form."
  (if (and uri orpheus-backend-current)
      (orpheus-backend-portable-uri orpheus-backend-current uri)
    uri))

(defun orpheus-ui--item-tracks (item)
  "Return portable tracks ITEM contributes, or nil.
A portable playlist row contributes all its tracks with their tags; a track
keeps its own tags.  A row that is only a folder or an album contributes
nothing."
  (let ((file (alist-get 'file (orpheus-item-data item)))
        (track (alist-get 'track (orpheus-item-data item))))
    (cond
     (file
      (delq nil
            (mapcar
             (lambda (entry)
               (let ((uri (orpheus-track-uri entry)))
                 (when uri
                   (orpheus-playlist--track-with-uri
                    entry (orpheus-ui--portable-uri uri)))))
             (orpheus-playlist-read file))))
     (track
      (let ((uri (orpheus-track-uri track)))
        (when uri
          (list (orpheus-playlist--track-with-uri
                 track (orpheus-ui--portable-uri uri))))))
     ((orpheus-item-playable item)
      (let ((uri (alist-get 'uri (orpheus-item-data item))))
        (when uri
          (list (orpheus-track-create
                 :uri (orpheus-ui--portable-uri uri)))))))))

(defun orpheus-ui-selected-tracks ()
  "Return the player's marked tracks, or the track at point.
The result is in portable playlist form, ready for the playlist editor.  Error
when the player buffer does not exist."
  (let ((buffer (get-buffer orpheus-ui-buffer-name)))
    (unless (buffer-live-p buffer)
      (user-error "Open the player first"))
    (with-current-buffer buffer
      (let ((items (or (orpheus-ui--marked-items)
                       (delq nil (list (orpheus-ui--item-at-point))))))
        (delq nil (mapcan #'orpheus-ui--item-tracks items))))))

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
    (define-key map [mouse-3] 'orpheus-ui-mouse-menu)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each row, so the mouse works on it.")

(defvar orpheus-ui-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-ui-mouse-tab)
    (define-key map [mouse-3] 'orpheus-ui-mouse-tab-menu)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each tab, so the mouse can select it.")

(defvar orpheus-ui-row-menu
  (let ((map (make-sparse-keymap "Track")))
    (define-key-after map [play] '(menu-item "Play" orpheus-ui-confirm))
    (define-key-after map [queue] '(menu-item "Queue" orpheus-ui-add))
    (define-key-after map [mark] '(menu-item "Mark" orpheus-ui-mark))
    (define-key-after map [unmark] '(menu-item "Unmark" orpheus-ui-unmark))
    (define-key-after map [edit]
      '(menu-item "Edit playlist" orpheus-ui-edit-playlist))
    (define-key-after map [delete] '(menu-item "Delete" orpheus-ui-delete))
    (define-key-after map [refresh] '(menu-item "Refresh" orpheus-ui-refresh))
    (define-key-after map [new-playlist]
      '(menu-item "New playlist…" orpheus-ui-new-playlist))
    (define-key-after map [menu] '(menu-item "Orpheus menu" orpheus-menu))
    map)
  "Menu opened by a right click on a player row.")

(defvar orpheus-ui-tab-menu
  (let ((map (make-sparse-keymap "Tabs")))
    (define-key-after map [previous]
      '(menu-item "Previous tab" orpheus-ui-previous-tab))
    (define-key-after map [next] '(menu-item "Next tab" orpheus-ui-next-tab))
    (define-key-after map [refresh] '(menu-item "Refresh" orpheus-ui-refresh))
    (define-key-after map [menu] '(menu-item "Orpheus menu" orpheus-menu))
    map)
  "Menu opened by a right click on a tab.")

(defun orpheus-ui--logo-image ()
  "Return the project logo at player size, or nil.
The image is made once and cached; a terminal frame gets nil, because an
image would be a box of escape codes there rather than a logo."
  (when (and (display-graphic-p)
             (not orpheus-ui--logo-image-cache)
             (file-readable-p orpheus-ui--logo-file))
    (setq orpheus-ui--logo-image-cache
          (ignore-errors
            (create-image orpheus-ui--logo-file nil nil
                          :height 20 :ascent 'center))))
  orpheus-ui--logo-image-cache)

(defun orpheus-ui--logo-string ()
  "Return the logo as a string, or nil when this frame cannot draw it."
  (let ((image (orpheus-ui--logo-image)))
    (when image
      (propertize " " 'display image 'help-echo "orpheus.el"))))

(defun orpheus-ui--header-line ()
  "Return the header line of the player."
  (if (and orpheus-backend-current
           (orpheus-backend-connected-p orpheus-backend-current))
      (let ((window (get-buffer-window)))
        (orpheus-ui--status-string
         orpheus-status-current orpheus-track-current
         (and window (window-body-width window))))
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
  "Return the cover art as an image, or nil.
The decoded image is cached by file, size, modification time and scale, so a
redraw every second does not decode the cover again."
  (let ((file (plist-get orpheus-ui--art :file)))
    (when (and file (file-readable-p file) (display-graphic-p))
      (let* ((window (get-buffer-window))
             (max (if window
                      (max 1 (min orpheus-ui-art-size
                                  (window-body-width window t)))
                    orpheus-ui-art-size))
             (attributes (file-attributes file))
             (key (list (expand-file-name file)
                        (file-attribute-size attributes)
                        (file-attribute-modification-time attributes)
                        max)))
        (or (gethash key orpheus-ui--image-cache)
            (let ((image (ignore-errors
                           (create-image file nil nil :max-width max
                                         :max-height max :ascent 'center))))
              (when image
                (when (>= (hash-table-count orpheus-ui--image-cache)
                          (max 1 orpheus-ui-image-cache-size))
                  ;; shortcut: the cache is emptied rather than evicted in LRU
                  ;; order; one redraw then decodes each cover once more.
                  (clrhash orpheus-ui--image-cache))
                (puthash key image orpheus-ui--image-cache))
              image))))))

(defun orpheus-ui--insert-art (width)
  "Insert the cover art centred in WIDTH, in a box that keeps its size.
The box is as tall as the art whether it is drawn, still loading or missing,
so the list below it never moves while a cover arrives."
  (when (display-graphic-p)
    (let* ((image (orpheus-ui--art-image))
           (size (and image (ignore-errors (image-size image t))))
           (rest (- orpheus-ui-art-size (or (cdr size) 0))))
      (when image
        (let ((columns (if size
                           (ceiling (/ (float (car size))
                                       (max 1 (frame-char-width))))
                         1)))
          (insert (make-string (max 0 (/ (- width columns) 2)) ?\s))
          (insert-image image " ")
          (insert "\n")))
      (when (> rest 0)
        ;; A bare number here is a multiple of the line height, not pixels;
        ;; a one-element list is the pixel specification.
        (insert (propertize " " 'display
                            (list 'space :width 1 :height (list rest)))
                "\n")))))

(defun orpheus-ui--center (text width)
  "Return TEXT centred in WIDTH, keeping the properties it carries."
  (concat (make-string (max 0 (/ (- width (string-width text)) 2)) ?\s)
          text))

(defun orpheus-ui--center-line (text width face)
  "Return TEXT centred in WIDTH, with FACE."
  (orpheus-ui--center (propertize (orpheus-ui--elide text width) 'face face)
                      width))

(defun orpheus-ui--button (label help command &optional face)
  "Return LABEL as a button that runs COMMAND, with HELP as its tooltip.
A control in a player is a thing to click, so the label carries its own
keymap, a hand for the pointer and the box of FACE, which defaults to
`orpheus-button-face'."
  (propertize (format "  %s  " label)
              'face (or face 'orpheus-button-face)
              'mouse-face 'highlight
              'pointer 'hand
              'help-echo help
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1]
                                    (lambda (_event)
                                      (interactive "e")
                                      (call-interactively command)))
                        (define-key map [follow-link] 'mouse-face)
                        map)))

(defun orpheus-ui--transport (status)
  "Return the transport buttons for STATUS, as a row the mouse can press.
The four mode toggles show their state in their face, the way the flags in
the status line spell it out.  Stop sits with the other transport controls,
and a gap keeps the modes from reading as part of the transport."
  (let* ((ascii orpheus-ui-plain-ascii)
         (playing (and status (eq (orpheus-status-state status) 'play)))
         (mood (lambda (on) (if on 'orpheus-button-on-face 'orpheus-button-face))))
    (concat
     (orpheus-ui--button (if ascii "<<" "⏮") "Previous track"
                         'orpheus-previous)
     " "
     (orpheus-ui--button (cond (playing (if ascii "||" "⏸"))
                               (ascii ">")
                               (t "▶"))
                         (if playing "Pause" "Play") 'orpheus-play-pause)
     " "
     (orpheus-ui--button (if ascii ">>" "⏭") "Next track" 'orpheus-next)
     " "
     (orpheus-ui--button (if ascii "X" "⏹") "Stop" 'orpheus-stop)
     "   "
     (orpheus-ui--button "R" "Repeat" 'orpheus-toggle-repeat
                         (funcall mood (and status
                                            (orpheus-status-repeat status))))
     " "
     (orpheus-ui--button "Z" "Shuffle" 'orpheus-toggle-random
                         (funcall mood (and status
                                            (orpheus-status-random status))))
     " "
     (orpheus-ui--button "S" "Single track" 'orpheus-toggle-single
                         (funcall mood (and status
                                            (orpheus-status-single status))))
     " "
     (orpheus-ui--button "C" "Consume" 'orpheus-toggle-consume
                         (funcall mood (and status
                                            (orpheus-status-consume status)))))))

(defun orpheus-ui--seek-seconds (point start width total elapsed)
  "Return the seconds to seek for a click at POINT on a bar at START.
WIDTH is the bar's width, TOTAL the track's length and ELAPSED where it is
now; the answer is relative, which every backend already takes."
  (let* ((column (max 0 (min (1- width) (- point start))))
         (target (* total (/ (float column) (max 1 (1- width))))))
    (round (- target (or elapsed 0)))))

(defvar orpheus-ui-seek-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-ui-seek-at)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on the progress bar, so a click there seeks.")

(defun orpheus-ui-seek-at (event)
  "Seek to the point of the progress bar clicked in EVENT."
  (interactive "e")
  (let* ((point (posn-point (event-start event)))
         (bar (and (integer-or-marker-p point)
                   (get-text-property point 'orpheus-bar))))
    (when bar
      (pcase-let ((`(,start ,width ,total ,elapsed) bar))
        (orpheus--command 'seek
                          (orpheus-ui--seek-seconds point start width total
                                                    elapsed))))))

(defun orpheus-ui--insert-now-playing (width)
  "Insert a centered now-playing card in WIDTH.
The card is drawn even with nothing playing, and its second line is always
there, so the list below it does not jump when the first track starts."
  (let* ((track orpheus-track-current)
         (status orpheus-status-current)
         (title (or (and track (orpheus-track-title track))
                    (and track (orpheus-track-label track))
                    "nothing playing"))
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
    (insert (orpheus-ui--center-line sub width 'orpheus-artist-face) "\n")
    ;; The bar keeps its two faces, so the line is centred without
    ;; re-propertising it, and a click on the bar seeks.
    (let* ((bar (orpheus-ui--progress-string elapsed total
                                             orpheus-ui-progress-width))
           (times (propertize (format "%s / %s"
                                      (orpheus-format-time elapsed)
                                      (orpheus-format-time total))
                              'face 'orpheus-muted-face))
           (line (concat bar "  " times)))
      (insert (make-string (max 0 (/ (- width (string-width line)) 2)) ?\s))
      (let ((from (point)))
        (insert bar)
        (when (and total (> total 0))
          (add-text-properties
           from (+ from (length bar))
           (list 'keymap orpheus-ui-seek-map
                 'mouse-face 'highlight
                 'pointer 'hand
                 'help-echo "mouse-1: seek"
                 'orpheus-bar (list from (length bar) total elapsed)))))
      (insert "  " times "\n"))
    (insert (orpheus-ui--center (orpheus-ui--transport status) width) "\n\n")))

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
          " RET play  e edit  N new  a add  d delete  C-x C-s save queue  ? menu  q bury")
         ('search
          " RET play  a add  m mark  / search again  g refresh  ? menu  q bury")
         (_
          " RET play  a add  m mark  d delete  / search  g refresh  ? menu  q bury")))

(defun orpheus-ui--render ()
  "Draw the buffer from the tabs and rows it holds.
The line at the top of the window is put back afterwards, so a redraw while a
cover arrives does not scroll the list under the reader.  A redraw asked for
while this one runs is done once after it, never inside it."
  (let ((orpheus-ui--rendering t))
    (let* ((inhibit-read-only t)
           (window (get-buffer-window))
           (line (and window (count-lines (point-min)
                                          (window-start window))))
           (tab (orpheus-ui--current-tab))
           (width (max 24 (if window (window-body-width window) 80))))
      (erase-buffer)
      (insert (or (orpheus-ui--logo-string) "")
              (orpheus-ui--tab-string) "\n")
      (when (orpheus-ui-tab-arg tab)
        (insert (propertize (orpheus-ui--path-string)
                            'face 'orpheus-muted-face)
                "\n"))
      (when orpheus-ui-show-art
        (orpheus-ui--insert-art width))
      (when orpheus-ui-show-now-playing
        (orpheus-ui--insert-now-playing width))
      (insert (propertize (make-string width ?─) 'face 'orpheus-muted-face)
              "\n")
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
      (when window
        ;; A buffer position is not a place: the art box and the path line
        ;; change the height above the list, but the line stays the same.
        ;; NOFORCE must stay nil, or redisplay may scroll to point and take
        ;; the cover away with it.
        (save-excursion
          (goto-char (point-min))
          (forward-line (or line 0))
          (set-window-start window (point))))
      (setq orpheus-ui--render-key (orpheus-ui--key))))
  (when orpheus-ui--redraw-pending
    (setq orpheus-ui--redraw-pending nil)
    (run-at-time 0 nil #'orpheus-ui--redraw))
  orpheus-ui--render-key)

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
          (plist-get orpheus-ui--art :file))))

(defun orpheus-ui--redraw ()
  "Draw the buffer again, but only when what it shows has changed."
  (when (buffer-live-p (get-buffer orpheus-ui-buffer-name))
    (with-current-buffer orpheus-ui-buffer-name
      (cond
       (orpheus-ui--rendering
        ;; A modeline or timer hook asked for a redraw from inside one; the
        ;; next pass picks it up instead of erasing the buffer twice.
        (setq orpheus-ui--redraw-pending t))
       ((not (equal (orpheus-ui--key) orpheus-ui--render-key))
        (orpheus-ui--render))))))

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
          (orpheus-ui-tab-loaded tab) t
          (orpheus-ui-tab-loaded-at tab) (float-time))
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
  "Remember ITEMS as the current transfer rows.
Redraw the player only when the Downloads tab is visible; otherwise the
header line is enough, so transfer progress does not repaint a long queue."
  (unless (equal items orpheus-ui--downloads)
    (setq orpheus-ui--downloads items)
    (let ((tab (orpheus-ui--current-tab)))
      (if (and tab (eq (orpheus-ui-tab-view tab) 'downloads))
          (progn
            (setf (orpheus-ui-tab-items tab) items
                  (orpheus-ui-tab-loading tab) nil
                  (orpheus-ui-tab-loaded tab) t
                  (orpheus-ui-tab-loaded-at tab) (float-time))
            (orpheus-ui--redraw))
        (force-mode-line-update t)))))

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
Unless FORCE is non-nil, a tab already loaded is reused for
`orpheus-ui-cache-seconds': the status timer asks every second, and fetching a
queue of thousands on each ask is what makes an editor stutter."
  (interactive)
  (let ((buffer (get-buffer orpheus-ui-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((tab (orpheus-ui--current-tab)))
          (when (and tab
                     (or force orpheus-ui--needs-refresh
                         (not (orpheus-ui-tab-loaded tab))
                         (> (- (float-time)
                               (orpheus-ui-tab-loaded-at tab))
                            orpheus-ui-cache-seconds)))
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
      (orpheus-ui--render)
      (orpheus-ui-refresh))))

(defun orpheus-ui-next-tab ()
  "Show the next tab.
A tab loaded within `orpheus-ui-cache-seconds' is shown from memory, so
moving between a large queue and a directory does not read either again."
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

(defun orpheus-ui-save-playlist (name &optional directory)
  "Save the queue as the portable playlist NAME.
An existing playlist is overwritten where it is.  A new one is stored in
DIRECTORY, which the player asks for, starting in
`orpheus-playlist-new-directory'."
  (interactive
   (let ((name (read-string "Save queue as playlist: ")))
     (list name
           (unless (orpheus-playlist-find name)
             (read-directory-name
              "Store playlist in: "
              (file-name-as-directory
               (expand-file-name orpheus-playlist-new-directory))
              nil nil)))))
  (when (string-empty-p name)
    (user-error "No playlist name"))
  (orpheus-playlist-save-queue
   name
   (lambda (file)
     (if file
         (progn
           (orpheus-ui-invalidate)
           (message "orpheus: saved %s" (file-name-nondirectory file)))
       (message "orpheus: the queue is empty")))
   directory))

(defun orpheus-ui-new-playlist (name directory)
  "Create an empty portable playlist called NAME in DIRECTORY.
The player asks for NAME and then for DIRECTORY to keep it in, starting at
`orpheus-playlist-new-directory' (=~/Music= by default).  DIRECTORY is made
when it does not exist, so a playlist can go in a folder of its own.  The new
playlist opens in its editor."
  (interactive
   (list (read-string "New playlist name: ")
         (read-directory-name
          "Store playlist in: "
          (file-name-as-directory
           (expand-file-name orpheus-playlist-new-directory))
          nil nil)))
  (let ((file (orpheus-playlist-create name directory)))
    (orpheus-ui-invalidate)
    (orpheus-playlist-edit file)
    (message "orpheus: created %s" (file-name-nondirectory file))))

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

(defun orpheus-ui-mouse-menu (event)
  "Open the row menu for the row clicked in EVENT."
  (interactive "e")
  (orpheus-ui-mouse-select event)
  (popup-menu orpheus-ui-row-menu (event-start event)))

(defun orpheus-ui-mouse-tab-menu (event)
  "Open the tab menu for the tab clicked in EVENT."
  (interactive "e")
  (orpheus-ui-mouse-tab event)
  (popup-menu orpheus-ui-tab-menu (event-start event)))

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
    ("N" "create a new portable playlist, asking where to keep it")
    ("d / C-k" "delete from the queue, remove a playlist, or cancel a download")
    ("D" "clear the queue")
    ("/ / C-s" "search")
    ("C" "jump to the playing row")
    ("?" "the menu: transport, modes, queue, playlists, views")
    ("H" "this help: concepts, keys, troubleshooting and the tutorial")
    ("E" "the equalizer: toggle it, load a preset, save your own")
    ("M-x orpheus-tutorial" "a guided tour through the player")
    ("SPC" "play or pause")
    ("s" "stop")
    ("> / <" "next and previous track")
    ("f / b" "seek forward and back")
    ("mouse-1 on the card" "press a transport button or seek on the bar")
    ("M-<up> / M-<down>, + / -" "volume up and down")
    ("z / x / c / v" "repeat, random, consume, single")
    ("M-u / M-U" "update the library, rescan it")
    ("C-x C-s" "save the queue as a portable M3U8 playlist")
    ("g" "refresh the status and this tab")
    ("V" "spectrum visualizer")
    ("mouse-3" "open the row, tab or playlist context menu")
    ("q" "bury the player"))
  "The keys `orpheus-ui-mode' binds, as rows of key and meaning.")

(defun orpheus-ui--help-button (label function &optional help)
  "Insert LABEL as a button that calls FUNCTION, with HELP as its tooltip."
  (insert-text-button label
                      'action (lambda (_button) (funcall function))
                      'follow-link t
                      'help-echo help))

(defun orpheus-ui--show-guide (buffer)
  "Show BUFFER in the selected window at its first line."
  (pop-to-buffer buffer '(display-buffer-same-window))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((window (get-buffer-window buffer)))
      (when window
        (set-window-start window (point-min))))))

(defun orpheus-help ()
  "Show a guide to the player, its keys, its equalizer and its failures."
  (interactive)
  (let ((buffer (get-buffer-create "*orpheus help*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "orpheus help\n" 'face 'orpheus-status-face))
        (insert
         "A music player for Emacs with three backends: MPD, local files and\n"
         "URLs through mpv, and music copied from another machine over ssh.\n\n")
        (insert (propertize "Getting started\n" 'face 'orpheus-title-face))
        (insert "  M-x orpheus opens the player in this window.  The tutorial\n"
                "  walks through it step by step: ")
        (orpheus-ui--help-button "Open the tutorial" #'orpheus-tutorial
                                 "A guided tour of the player")
        (insert ".  The menu on ? groups every command, and ")
        (orpheus-ui--help-button "Customize orpheus"
                                 (lambda () (customize-group 'orpheus))
                                 "Every option and face in one place")
        (insert " shows the options.\n\n")
        (insert (propertize "The tabs\n" 'face 'orpheus-title-face))
        (insert
         "  1-7 choose a tab, TAB and S-TAB move between them, and each tab\n"
         "  keeps its own place.  RET plays the row, or enters a directory,\n"
         "  album or artist; ^ goes back up one level.  SSH adds Downloads\n"
         "  after Queue, where RET plays or retries and d cancels a transfer.\n\n")
        (insert (propertize "The transport\n" 'face 'orpheus-title-face))
        (insert
         "  SPC plays or pauses, s stops, > and < change track, f and b seek,\n"
         "  and + and - change the volume.  z, x, c and v toggle repeat,\n"
         "  random, consume and single.  The card's buttons do the same, and\n"
         "  mouse-1 selects a row, mouse-2 plays it and mouse-3 opens its menu.\n\n")
        (insert (propertize "The queue and playlists\n" 'face 'orpheus-title-face))
        (insert
         "  m and u mark and unmark rows, a queues the marks or the row, A\n"
         "  queues everything on the tab, d removes a row, D empties the\n"
         "  queue, and C jumps to what is playing.  N makes a portable\n"
         "  playlist, e edits the one at point, and C-x C-s saves the queue.\n\n")
        (insert (propertize "Equalizer\n" 'face 'orpheus-title-face))
        (insert
         "  E opens the equalizer: enable it, load flat, bass, vocal, treble\n"
         "  or loudness, save your own preset, or type the ten gains.  It\n"
         "  works with the local and ssh backends, which play through mpv.\n"
         "  MPD's audio leaves through the daemon's own output, so Emacs\n"
         "  cannot filter it.  Saved presets live in\n"
         (format "  %s.\n\n" orpheus-equalizer-preset-file))
        (insert (propertize "If something is wrong\n" 'face 'orpheus-title-face))
        (insert
         "  A backend that does not answer is retried in the background, so\n"
         "  the editor stays usable.  M-x orpheus-ssh-diagnose checks the\n"
         "  remote commands, and mpv's own complaints are in *orpheus mpv*.\n"
         "  M-x orpheus-quit closes the player and stops reading the status.\n\n")
        (insert (propertize "Every key\n" 'face 'orpheus-title-face))
        (dolist (row orpheus-ui--help-lines)
          (insert "  "
                  (propertize (format "%-24s" (car row))
                              'face 'orpheus-status-face)
                  " "
                  (propertize (cadr row) 'face 'orpheus-title-face)
                  "\n"))
        (insert "\n")
        (insert (propertize " q buries the player; g reads everything again\n"
                            'face 'orpheus-muted-face))
        (goto-char (point-min))
        (special-mode)
        (visual-line-mode 1)
        (setq-local line-spacing 2
                    wrap-prefix "  ")))
    (orpheus-ui--show-guide buffer)))

(defun orpheus-ui-help ()
  "Show the orpheus help buffer (an alias for `orpheus-help')."
  (interactive)
  (orpheus-help))

(defun orpheus-tutorial ()
  "Show a short guided tour of orpheus, one step per heading."
  (interactive)
  (let ((buffer (get-buffer-create "*orpheus tutorial*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "orpheus tutorial\n" 'face 'orpheus-status-face))
        (insert
         "Work through these in order; each button runs the step.  The\n"
         "player keeps its keys, so the same commands work from the menu,\n"
         "from M-x, and from the mouse.\n\n")
        (insert (propertize "1. Open the player\n" 'face 'orpheus-title-face))
        (insert "   M-x orpheus shows the queue in this window; q buries it.  ")
        (orpheus-ui--help-button "Open it now" #'orpheus
                                 "Run M-x orpheus")
        (insert ".\n\n")
        (insert (propertize "2. Choose a backend\n" 'face 'orpheus-title-face))
        (insert
         "   orpheus-backend is mpd, local or ssh.  MPD owns a library and\n"
         "   its queue; local plays files and URLs with mpv; ssh copies a\n"
         "   track from another machine and plays it here.  ")
        (orpheus-ui--help-button "Customize it"
                                 (lambda () (customize-option 'orpheus-backend))
                                 "Choose and configure the backend")
        (insert ".\n\n")
        (insert (propertize "3. Browse and play\n" 'face 'orpheus-title-face))
        (insert
         "   Press 2 for the files or directories tab, RET on a directory to\n"
         "   enter it, ^ to go back, and RET on a track to play it.  TAB\n"
         "   moves between tabs.\n\n")
        (insert (propertize "4. Build a queue\n" 'face 'orpheus-title-face))
        (insert
         "   m marks a row, a queues it, A queues the whole tab, d removes\n"
         "   the row, D empties the queue, and C jumps to what is playing.\n"
         "   SPC pauses, s stops, and > and < change track.\n\n")
        (insert (propertize "5. Make a playlist\n" 'face 'orpheus-title-face))
        (insert
         "   N asks for a name and a directory, then opens the playlist in\n"
         "   its editor.  C-x C-s saves the queue as a playlist.  ")
        (orpheus-ui--help-button "Make one"
                                 (lambda () (call-interactively
                                             #'orpheus-ui-new-playlist))
                                 "Create a new portable playlist")
        (insert ".\n\n")
        (insert (propertize "6. Shape the sound\n" 'face 'orpheus-title-face))
        (insert
         "   E opens the equalizer.  Load a preset, save the current gains,\n"
         "   or type ten values.  It needs the local or ssh backend.  ")
        (orpheus-ui--help-button "Open the equalizer"
                                 #'orpheus-equalizer-menu
                                 "Load, save and edit equalizer presets")
        (insert ".\n\n")
        (insert (propertize "7. Get help\n" 'face 'orpheus-title-face))
        (insert
         "   ? opens the menu, H opens the help buffer, and M-x\n"
         "   orpheus-tutorial returns here.  ")
        (orpheus-ui--help-button "Open the help" #'orpheus-help
                                 "Concepts, keys and troubleshooting")
        (insert ".\n\n")
        (insert (propertize "q buries this buffer; the music keeps playing.\n"
                            'face 'orpheus-muted-face))
        (goto-char (point-min))
        (special-mode)
        (visual-line-mode 1)
        (setq-local line-spacing 2
                    wrap-prefix "  ")))
    (orpheus-ui--show-guide buffer)))

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
    (define-key map (kbd "N") 'orpheus-ui-new-playlist)
    (define-key map (kbd "d") 'orpheus-ui-delete)
    (define-key map (kbd "C-k") 'orpheus-ui-delete)
    (define-key map (kbd "D") 'orpheus-ui-clear)
    (define-key map (kbd "/") 'orpheus-ui-search)
    (define-key map (kbd "C-s") 'orpheus-ui-search)
    (define-key map (kbd "C") 'orpheus-ui-jump-to-current)
    (define-key map (kbd "?") 'orpheus-menu)
    (define-key map (kbd "H") 'orpheus-help)
    (define-key map (kbd "E") 'orpheus-equalizer-menu)
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

(defcustom orpheus-ui-mode-bindings nil
  "Keys to bind in `orpheus-ui-mode-map', as (KEY . COMMAND) pairs.
KEY is a string as `kbd' reads it, and COMMAND nil unbinds it.  These are
applied over the defaults when `orpheus-ui-mode' starts, so a setting in your
init remaps a key without a hook.  For a key that needs a closure, call
`define-key' on `orpheus-ui-mode-map' instead; every key is available there."
  :type '(alist :key-type string
                :value-type (choice (command :tag "Command")
                                    (const :tag "Unbound" nil)))
  :group 'orpheus)

(defun orpheus-ui--apply-bindings ()
  "Apply `orpheus-ui-mode-bindings' over the default keys."
  (dolist (binding orpheus-ui-mode-bindings)
    (define-key orpheus-ui-mode-map (kbd (car binding)) (cdr binding))))

(define-derived-mode orpheus-ui-mode special-mode "Orpheus"
  "Major mode for the orpheus music player."
  (setq-local header-line-format '(:eval (orpheus-ui--header-line)))
  (setq-local truncate-lines t)
  (setq-local line-spacing nil)
  (buffer-disable-undo)
  (setq-local orpheus-ui--tabs nil)
  (setq-local orpheus-ui--tab 0)
  (setq-local orpheus-ui--downloads nil)
  (setq-local orpheus-ui--render-key nil)
  (orpheus-ui--apply-bindings))

;;; Opening

(defun orpheus-ui--fit-window (window)
  "Grow WINDOW when the cover leaves too few rows of list under it.
In a graphical frame the cover block is tall, and a split window can be left
showing almost none of the queue; the frame is the limit."
  (let* ((match (save-excursion
                  (goto-char (point-min))
                  (text-property-search-forward 'orpheus-item 0 #'eq)))
         (header (if match
                     (count-lines (point-min) (prop-match-beginning match))
                   0))
         (missing (- (+ header 8) (window-body-height window))))
    (when (> missing 0)
      (ignore-errors (enlarge-window missing)))))

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
    (pop-to-buffer buffer orpheus-ui-display-action)
    (let ((window (get-buffer-window buffer)))
      (when window
        (with-current-buffer buffer
          (orpheus-ui--fit-window window))))
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
      ;; The cover already drawn stays until the next one arrives: a blank box
      ;; for as long as a fetch takes is the flicker this avoids.
      (setq orpheus-ui--art (list :key key :uri uri :loading t
                                  :file (plist-get orpheus-ui--art :file)))
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
