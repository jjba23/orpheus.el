;;; record-demo.el --- Record the reproducible orpheus demo -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; This file is run by =scripts/record-demo.sh=, never by a user directly.  It
;;; draws the real player over a fixed library: no MPD, no mpv, no network and
;;; no clock, so the same checkout produces the same frames on any machine with
;;; the fonts and the tools the script checks for.  Every theme gets the queue
;;; and the playlists, and Emacs exports each frame as a PNG after redisplay
;;; has painted it.

;;; Code:

(require 'cl-lib)
(require 'orpheus)

(declare-function x-export-frames "xfns.c" (&optional frames type))

(defvar orpheus-demo-output-directory (getenv "ORPHEUS_DEMO_OUTPUT")
  "Directory the frames are written to; set by the shell script.")

(defvar orpheus-demo-themes '(modus-vivendi modus-operandi leuven wombat)
  "Themes shown in the demo, in order.")

(defvar orpheus-demo-frame 0
  "Number of the frame being written.")

(defvar orpheus-demo--items nil
  "The queue the demo backend returns.")

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

(cl-defmethod orpheus-backend-connected-p ((_backend orpheus-demo-backend))
  "The demo backend is always ready."
  t)

(cl-defmethod orpheus-backend-capabilities ((_backend orpheus-demo-backend))
  "The demo shows a queue and the portable playlists."
  '(queue playlists))

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
                                      _kind _arg callback)
  "Report no server-side rows; the demo shows the portable playlists."
  (funcall callback nil))

(cl-defmethod orpheus-backend-album-art ((_backend orpheus-demo-backend)
                                         _track callback)
  "Report no embedded art; the demo uses its own cover."
  (funcall callback nil))

(cl-defmethod orpheus-backend-folder-art ((_backend orpheus-demo-backend)
                                          _folder callback)
  "Report no folder art."
  (funcall callback nil))

(cl-defmethod orpheus-backend-command ((_backend orpheus-demo-backend)
                                       _action &optional _value)
  "The demo needs no transport."
  nil)

(defun orpheus-demo--font ()
  "Use a monospace font when the machine has one, so glyph widths agree."
  (when (find-font (font-spec :family "DejaVu Sans Mono"))
    (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)))

(defun orpheus-demo--setup ()
  "Open the player on the demo backend and prepare both scenes."
  (setq orpheus-ui-icons nil
        orpheus-ui-plain-ascii t
        orpheus-progress-filled-char ?#
        orpheus-progress-empty-char ?-
        orpheus-notifications nil
        orpheus-ui-show-art t
        orpheus-ui-show-now-playing t
        orpheus-ui-art-size 200
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
  ;; Opening asked the demo backend for art, and it has none; the cover goes
  ;; back after it so the scene is drawn with one.
  (setq orpheus-ui--art
        (list :key "demo" :uri "02 The Long Way Home.flac"
              :file (expand-file-name "screenshots/visualizer.png")))
  (set-window-buffer (selected-window) (get-buffer orpheus-ui-buffer-name))
  (with-current-buffer orpheus-ui-buffer-name
    (setq cursor-type nil))
  (let ((tab (orpheus-demo--tab 'playlists)))
    (when tab
      (setf (orpheus-ui-tab-items tab) (orpheus-demo--playlists)
            (orpheus-ui-tab-loaded tab) t
            (orpheus-ui-tab-loaded-at tab) (float-time)))))

(defun orpheus-demo--tab (kind)
  "Return the tab of KIND in the player."
  (cl-find-if (lambda (tab) (eq (orpheus-ui-tab-kind tab) kind))
              (append orpheus-ui--tabs nil)))

(defun orpheus-demo--show (kind)
  "Show the demo scene for KIND and draw it."
  (let ((tab (orpheus-demo--tab kind)))
    (when tab
      (setq orpheus-ui--tab (cl-position tab orpheus-ui--tabs))
      (orpheus-ui--render)
      (with-current-buffer orpheus-ui-buffer-name
        (setq orpheus-ui--render-key (orpheus-ui--key))))))

(defun orpheus-demo--theme (theme)
  "Load THEME and settle the frame before the next capture."
  (mapc #'disable-theme custom-enabled-themes)
  (condition-case error
      (load-theme theme t)
    (error (error "cannot load theme %s: %s"
                  theme (error-message-string error))))
  (force-mode-line-update t)
  (redisplay t)
  (sit-for 0.2))

(defun orpheus-demo--capture ()
  "Export the selected frame into the next frame file.
Emacs writes the PNG itself, so the pixels are the frame and not a region of
the screen, and no window manager or screen geometry takes part."
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
    file))

(defun orpheus-demo-run ()
  "Record every theme and scene, then leave Emacs."
  (condition-case error
      (progn
        (unless (and orpheus-demo-output-directory
                     (file-directory-p orpheus-demo-output-directory))
          (error "ORPHEUS_DEMO_OUTPUT is not a directory"))
        (orpheus-demo--font)
        (set-frame-size (selected-frame) 112 38)
        (orpheus-demo--setup)
        (dolist (theme orpheus-demo-themes)
          (orpheus-demo--theme theme)
          (dolist (scene '(queue playlists))
            (orpheus-demo--show scene)
            (orpheus-demo--capture))))
    (error
     (princ (format "record-demo: %s\n" (error-message-string error))
            #'external-debugging-output)
     (kill-emacs 1)))
  (kill-emacs 0))

(provide 'record-demo)

;;; record-demo.el ends here
