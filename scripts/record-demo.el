;;; record-demo.el --- Record the reproducible orpheus demo -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; This file is run by =scripts/record-demo.sh=, never by a user directly.  It
;;; draws the real player over a fixed library: no MPD, no mpv, no network and
;;; no clock, so the same checkout produces the same frames on any machine with
;;; the fonts and the tools the script checks for.
;;;
;;; Every player preset is shown over the queue, the directories, an album and
;;; the transfer tab, so the transport controls, the card, the artwork and
;;; the palette move together.  Two layout presets and two visualizer ramps end
;;; the loop.  Selected frames are also written under screenshots/ as the
;;; stills the manual links to.

;;; Code:

(require 'cl-lib)
(require 'orpheus)

(declare-function x-export-frames "xfns.c" (&optional frames type))

(defvar orpheus-demo-output-directory (getenv "ORPHEUS_DEMO_OUTPUT")
  "Directory the frames are written to; set by the shell script.")

(defvar orpheus-demo-stills-directory (getenv "ORPHEUS_DEMO_STILLS")
  "Directory the named stills are written to, or nil for frames only.")

(defvar orpheus-demo-frame 0
  "Number of the frame being written.")

(defvar orpheus-demo--items nil
  "The queue the demo backend returns.")

(defconst orpheus-demo--cover
  (expand-file-name "screenshots/demo-cover.png")
  "The demo cover, next to the repository's other screenshots.")

(defvar orpheus-demo-themes
  '((modus-vivendi . aegean)
    (modus-vivendi . nord)
    (modus-vivendi . gruvbox)
    (modus-vivendi . rose-pine)
    (modus-operandi . solarized-light)
    (modus-vivendi . graphite))
  "Base Emacs theme and player preset shown together, in order.")

(defconst orpheus-demo--levels
  '(14 20 26 22 30 38 34 42 50 44 56 52 64 60 72 68 80 74 86 82 90 96 100 94
    88 80 74 68 62 58 52 46 40 44 50 56 50 42 36 30 38 46 54 48 40 34 28 22)
  "A fixed visualizer frame: a spectrum, quiet to loud and back.")

(cl-defstruct (orpheus-demo-backend (:include orpheus-backend)
                                    (:constructor orpheus-demo--make))
  "A backend that answers from the fixed demo library.")

(defun orpheus-demo--track (n title artist album duration)
  "Return the Nth demo track called TITLE by ARTIST on ALBUM of DURATION."
  (orpheus-track-create :uri (format "%02d %s.flac" n title)
                        :title title :artist artist :album album
                        :duration duration))

(defun orpheus-demo--queue ()
  "Return the demo queue, built once so every frame shows the same rows."
  (or orpheus-demo--items
      (setq orpheus-demo--items
            (list
             (orpheus-item-create
              :label "Nightfall" :detail "4:12" :kind 'track
              :data (list (cons 'id 1) (cons 'pos 0)
                          (cons 'uri "01 Nightfall.flac")
                          (cons 'track (orpheus-demo--track
                                        1 "Nightfall" "Aegean Quartet"
                                        "Marble Halls" 252))))
             (orpheus-item-create
              :label "The Long Way Home" :detail "5:06" :kind 'track
              :data (list (cons 'id 2) (cons 'pos 1)
                          (cons 'uri "02 The Long Way Home.flac")
                          (cons 'track (orpheus-demo--track
                                        2 "The Long Way Home" "Aegean Quartet"
                                        "Marble Halls" 306))))
             (orpheus-item-create
              :label "Iris" :detail "3:48" :kind 'track
              :data (list (cons 'id 3) (cons 'pos 2)
                          (cons 'uri "03 Iris.flac")
                          (cons 'track (orpheus-demo--track
                                        3 "Iris" "Naxos Choir"
                                        "Aegean Blue" 228))))
             (orpheus-item-create
              :label "Olive Grove" :detail "4:31" :kind 'track
              :data (list (cons 'id 4) (cons 'pos 3)
                          (cons 'uri "04 Olive Grove.flac")
                          (cons 'track (orpheus-demo--track
                                        4 "Olive Grove" "Naxos Choir"
                                        "Aegean Blue" 271))))
             (orpheus-item-create
              :label "Marble Halls" :detail "6:02" :kind 'track
              :data (list (cons 'id 5) (cons 'pos 4)
                          (cons 'uri "05 Marble Halls.flac")
                          (cons 'track (orpheus-demo--track
                                        5 "Marble Halls" "Iris Ensemble"
                                        "Marble Halls" 362))))
             (orpheus-item-create
              :label "Gold Leaf" :detail "2:57" :kind 'track
              :data (list (cons 'id 6) (cons 'pos 5)
                          (cons 'uri "06 Gold Leaf.flac")
                          (cons 'track (orpheus-demo--track
                                        6 "Gold Leaf" "Iris Ensemble"
                                        "Marble Halls" 177))))
             (orpheus-item-create
              :label "Aegean Blue" :detail "4:44" :kind 'track
              :data (list (cons 'id 7) (cons 'pos 6)
                          (cons 'uri "07 Aegean Blue.flac")
                          (cons 'track (orpheus-demo--track
                                        7 "Aegean Blue" "Iris Ensemble"
                                        "Aegean Blue" 284))))))))

(defun orpheus-demo--playlists ()
  "Return the demo playlist rows."
  (mapcar
   (lambda (entry)
     (orpheus-item-create
      :label (nth 0 entry) :detail (nth 1 entry) :kind 'playlist
      :data (list (cons 'file (expand-file-name (nth 0 entry) "~/Music")))))
   '(("Late Night" "12 tracks")
     ("Sunday Morning" "8 tracks")
     ("Aegean Drive" "19 tracks")
     ("Rainy Window" "6 tracks"))))

(defun orpheus-demo--track-rows (indexes)
  "Return the queue rows at INDEXES, for use as browsable tracks."
  (mapcar (lambda (index) (nth index (orpheus-demo--queue))) indexes))

(defun orpheus-demo--rows (kind)
  "Return the fixed rows for the browse KIND."
  (pcase kind
    ('directories
     (append
      (list
       (orpheus-item-create
        :label "Aegean Blue/" :detail "12 tracks" :kind 'directory
        :data '((browse directories "Aegean Blue")))
       (orpheus-item-create
        :label "Marble Halls/" :detail "9 tracks" :kind 'directory
        :data '((browse directories "Marble Halls")))
       (orpheus-item-create
        :label "Live at Naxos/" :detail "7 tracks" :kind 'directory
        :data '((browse directories "Live at Naxos"))))
      (orpheus-demo--track-rows '(0 5))))
    ('artists
     (list
      (orpheus-item-create :label "Aegean Quartet" :detail "2 albums"
                           :kind 'artist :data '((browse artists "Aegean Quartet")))
      (orpheus-item-create :label "Naxos Choir" :detail "1 album"
                           :kind 'artist :data '((browse artists "Naxos Choir")))
      (orpheus-item-create :label "Iris Ensemble" :detail "2 albums"
                           :kind 'artist :data '((browse artists "Iris Ensemble")))))
    ('albumartists
     (list
      (orpheus-item-create :label "Aegean Quartet" :detail "2 albums"
                           :kind 'albumartist
                           :data '((browse albumartists "Aegean Quartet")))
      (orpheus-item-create :label "Naxos Choir" :detail "1 album"
                           :kind 'albumartist
                           :data '((browse albumartists "Naxos Choir")))))
    ('albums
     (list
      (orpheus-item-create :label "Marble Halls" :detail "Aegean Quartet · 2024"
                           :kind 'album :data '((browse albums "Marble Halls")))
      (orpheus-item-create :label "Aegean Blue" :detail "Naxos Choir · 2023"
                           :kind 'album :data '((browse albums "Aegean Blue")))
      (orpheus-item-create :label "Live at Naxos" :detail "Iris Ensemble · 2022"
                           :kind 'album :data '((browse albums "Live at Naxos")))))
    ('search (orpheus-demo--track-rows '(1 2 6)))))

(defun orpheus-demo--downloads ()
  "Return the demo transfer rows, one in each state."
  (list
   (orpheus-item-create
    :label "The Long Way Home.flac" :detail "68%  3.1 MB/s  ETA 0:04"
    :kind 'download :data '((state . downloading) (uri . "02 The Long Way Home.flac")))
   (orpheus-item-create
    :label "Iris.flac" :detail "queued  position 2"
    :kind 'download :data '((state . queued) (uri . "03 Iris.flac")))
   (orpheus-item-create
    :label "Olive Grove.flac" :detail "done  28.4 MB"
    :kind 'download :data '((state . done) (uri . "04 Olive Grove.flac")))
   (orpheus-item-create
    :label "Gold Leaf.flac" :detail "failed  connection reset"
    :kind 'download :data '((state . failed) (uri . "06 Gold Leaf.flac")))))

(cl-defmethod orpheus-backend-connected-p ((_backend orpheus-demo-backend))
  "The demo backend is always ready."
  t)

(cl-defmethod orpheus-backend-capabilities ((_backend orpheus-demo-backend))
  "The demo shows every tab the interface knows."
  '(queue downloads directories artists albumartists albums playlists search))

(cl-defmethod orpheus-backend-status ((_backend orpheus-demo-backend) callback)
  "Report the fixed status the demo was drawn from."
  (funcall callback
           (orpheus-status-create :state 'play :elapsed 83 :total 252
                                  :volume 72 :repeat t :random t :songid 2)))

(cl-defmethod orpheus-backend-track ((_backend orpheus-demo-backend) callback)
  "Report the track the demo is playing."
  (funcall callback
           (orpheus-demo--track 2 "The Long Way Home" "Aegean Quartet"
                                "Marble Halls" 306)))

(cl-defmethod orpheus-backend-queue ((_backend orpheus-demo-backend) callback)
  "Report the fixed queue."
  (funcall callback (orpheus-demo--queue)))

(cl-defmethod orpheus-backend-browse ((_backend orpheus-demo-backend)
                                      kind _arg callback)
  "Report the fixed rows for KIND, or none for a server-side playlist list."
  (funcall callback (unless (eq kind 'playlists) (orpheus-demo--rows kind))))

(cl-defmethod orpheus-backend-downloads ((_backend orpheus-demo-backend) callback)
  "Report the fixed transfer rows."
  (funcall callback (orpheus-demo--downloads)))

(cl-defmethod orpheus-backend-album-art ((_backend orpheus-demo-backend)
                                         _track callback)
  "Report the demo cover for a track."
  (funcall callback (and (file-exists-p orpheus-demo--cover)
                         orpheus-demo--cover)))

(cl-defmethod orpheus-backend-folder-art ((_backend orpheus-demo-backend)
                                          _folder callback)
  "Report the demo cover for a folder."
  (funcall callback (and (file-exists-p orpheus-demo--cover)
                         orpheus-demo--cover)))

(cl-defmethod orpheus-backend-command ((_backend orpheus-demo-backend)
                                       _action &optional _value)
  "The demo needs no transport."
  nil)

(defun orpheus-demo--font ()
  "Use a monospace font when the machine has one, so glyph widths agree."
  (when (find-font (font-spec :family "DejaVu Sans Mono"))
    (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110))
  ;; The media symbols are not all in one font; these are the usual fallbacks.
  (dolist (family '("JuliaMono" "Symbola" "Noto Sans Symbols2"))
    (when (find-font (font-spec :family family))
      (set-fontset-font t nil (font-spec :family family) nil 'append))))

(defun orpheus-demo--tab (kind)
  "Return the tab of KIND in the player."
  (cl-find-if (lambda (tab) (eq (orpheus-ui-tab-kind tab) kind))
              (append orpheus-ui--tabs nil)))

(defun orpheus-demo--fill-tabs ()
  "Load the fixed rows into every tab the demo shows."
  (let ((queue (orpheus-demo--tab 'queue)))
    (when queue
      (setf (orpheus-ui-tab-items queue) (orpheus-demo--queue)
            (orpheus-ui-tab-loaded queue) t
            (orpheus-ui-tab-loaded-at queue) (float-time))))
  (dolist (kind '(directories artists albumartists albums playlists downloads))
    (when-let* ((tab (orpheus-demo--tab kind)))
      (setf (orpheus-ui-tab-items tab)
            (pcase kind
              ('playlists (orpheus-demo--playlists))
              ('downloads (orpheus-demo--downloads))
              (_ (orpheus-demo--rows kind)))
            (orpheus-ui-tab-loaded tab) t
            (orpheus-ui-tab-loaded-at tab) (float-time)
            (orpheus-ui-tab-index tab) 0)
      (when (eq kind 'directories)
        (setf (orpheus-ui-tab-arg tab) '("Aegean Blue"))))))

(defun orpheus-demo--setup ()
  "Open the player on the demo backend and prepare every scene."
  (setq orpheus-ui-icons t
        orpheus-ui-plain-ascii nil
        orpheus-progress-filled-char ?━
        orpheus-progress-empty-char ?─
        orpheus-notifications nil
        orpheus-ui-show-art t
        orpheus-ui-show-now-playing t
        orpheus-ui-art-size 200
        orpheus-ui-layout (alist-get 'classic orpheus-ui-layout-presets)
        orpheus-theme 'aegean
        orpheus-vis-theme nil
        orpheus-vis-bars 48
        orpheus-backend 'demo
        orpheus-backend-current (orpheus-demo--make :name 'demo)
        orpheus-status-current
        (orpheus-status-create :state 'play :elapsed 83 :total 252
                               :volume 72 :repeat t :random t :songid 2)
        orpheus-track-current
        (orpheus-demo--track 2 "The Long Way Home" "Aegean Quartet"
                             "Marble Halls" 306))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (orpheus-ui-open)
  (orpheus-demo--fill-tabs)
  (set-window-buffer (selected-window) (get-buffer orpheus-ui-buffer-name))
  (with-current-buffer orpheus-ui-buffer-name
    (setq cursor-type nil
          orpheus-ui--art (list :key "demo"
                                :uri "02 The Long Way Home.flac"
                                :file orpheus-demo--cover))))

(defun orpheus-demo--show (kind)
  "Show the demo scene for KIND and draw it."
  (let ((tab (orpheus-demo--tab kind)))
    (when tab
      (setq orpheus-ui--tab (cl-position tab orpheus-ui--tabs))
      (orpheus-ui--render)
      (with-current-buffer orpheus-ui-buffer-name
        (setq orpheus-ui--render-key (orpheus-ui--key))))))

(defun orpheus-demo--theme (base player)
  "Load BASE, then apply the PLAYER colour preset, and settle the frame."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (condition-case error
      (load-theme base t)
    (error (error "cannot load theme %s: %s"
                  base (error-message-string error))))
  (orpheus-apply-theme player)
  (force-mode-line-update t)
  (redisplay t)
  (sit-for 0.2))

(defun orpheus-demo--visualizer (theme)
  "Show the visualizer with THEME's ramp over the fixed frame."
  (setq orpheus-vis-theme theme
        orpheus-vis--data orpheus-demo--levels
        orpheus-vis--accum ""
        orpheus-vis--dirty t
        orpheus-vis--image nil
        orpheus-vis--image-key nil)
  (let ((buffer (get-buffer-create orpheus-vis-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq-local mode-line-format " orpheus visualizer")
      (set-window-buffer (selected-window) buffer)
      (orpheus-vis--render t))))

(defun orpheus-demo--capture (&optional still)
  "Export the selected frame into the next frame file.
When STILL is non-nil and `orpheus-demo-stills-directory' is set, copy the
frame there under that name too.  Emacs writes the PNG itself, so the pixels
are the frame and not a region of the screen, and no window manager or screen
geometry takes part."
  (message nil)
  (redisplay t)
  (sit-for 0.15)
  (setq orpheus-demo-frame (1+ orpheus-demo-frame))
  (let ((file (expand-file-name
               (format "frame-%03d.png" orpheus-demo-frame)
               orpheus-demo-output-directory))
        (data (condition-case error
                  (x-export-frames nil 'png)
                (error
                 (error "this Emacs cannot export PNG frames: %s"
                        (error-message-string error))))))
    (unless (and (stringp data) (> (length data) 0))
      (error "x-export-frames returned no PNG data"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (write-region (point-min) (point-max) file nil 'silent))
    (when (and still orpheus-demo-stills-directory)
      (copy-file file (expand-file-name (concat still ".png")
                                        orpheus-demo-stills-directory)
                 t))
    file))

(defun orpheus-demo-run ()
  "Record every theme and scene, then leave Emacs."
  (condition-case error
      (progn
        (unless (and orpheus-demo-output-directory
                     (file-directory-p orpheus-demo-output-directory))
          (error "ORPHEUS_DEMO_OUTPUT is not a directory"))
        (when orpheus-demo-stills-directory
          (make-directory orpheus-demo-stills-directory t))
        (orpheus-demo--font)
        (set-frame-size (selected-frame) 144 38)
        (orpheus-demo--setup)
        (dolist (pair orpheus-demo-themes)
          (orpheus-demo--theme (car pair) (cdr pair))
          (dolist (scene '(queue directories albums downloads))
            (orpheus-demo--show scene)
            (orpheus-demo--capture
             (and (eq scene 'directories)
                  (format "theme-%s" (cdr pair))))))
        (orpheus-demo--theme 'modus-vivendi 'aegean)
        (orpheus-ui-apply-layout 'compact)
        (orpheus-demo--show 'queue)
        (orpheus-demo--capture "layout-compact")
        (orpheus-ui-apply-layout 'cover)
        (orpheus-demo--show 'directories)
        (orpheus-demo--capture "layout-cover")
        (orpheus-ui-apply-layout 'classic)
        (orpheus-demo--visualizer 'ember)
        (orpheus-demo--capture "visualizer-ember")
        (orpheus-demo--visualizer 'ice)
        (orpheus-demo--capture "visualizer-ice"))
    (error
     (princ (format "record-demo: %s\n" (error-message-string error))
            #'external-debugging-output)
     (kill-emacs 1)))
  (kill-emacs 0))

(provide 'record-demo)

;;; record-demo.el ends here
