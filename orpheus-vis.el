;;; orpheus-vis.el --- A spectrum visualizer for orpheus -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; cava listens to what the machine is playing and prints one frame of bar
;;; heights per line; this file runs it, reads those frames as they arrive and
;;; draws them.  In a terminal the bars are block characters, which every font
;;; has; in a graphical frame they are an SVG bar graph with a little rounding
;;; and a colour ramp, which is the fanciest thing here and costs nothing when
;;; it is not available.
;;;
;;; Where cava listens is the platform's business: PipeWire on Linux (which is
;;; what makes it work with any player, MPD and mpv alike), PortAudio on macOS
;;; (where a loopback device such as BlackHole is needed to hear the system
;;; output), WASAPI on Windows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)

(declare-function svg-image "svg" (svg &rest args))
(declare-function svg-rectangle "svg" (svg x y width height &rest args))
(declare-function svg-create "svg" (width height &rest args))

;;; Settings

(defcustom orpheus-vis-program "cava"
  "Program name or path of cava."
  :type 'string :group 'orpheus)

(defcustom orpheus-vis-bars 32
  "Number of bars to ask cava for."
  :type 'integer :group 'orpheus)

(defcustom orpheus-vis-framerate 30
  "Frames per second to read from cava and draw."
  :type 'integer :group 'orpheus)

(defcustom orpheus-vis-input nil
  "Capture method for cava, or nil to choose one for this system.
The value is a cava input method: `pipewire', `pulse', `alsa',
`portaudio', `wasapi' or `fifo'."
  :type '(choice (const :tag "Choose for this system" nil) symbol)
  :group 'orpheus)

(defcustom orpheus-vis-empty-char ?·
  "Character for the part of a bar that is not filled."
  :type 'character :group 'orpheus)

(defcustom orpheus-vis-bar-char ?█
  "Character a filled bar is drawn with in a terminal."
  :type 'character :group 'orpheus)

(defcustom orpheus-vis-colors
  '("#23395d" "#3f6f9e" "#7aa8c9" "#c3aede" "#e8cf9a")
  "Colours of the SVG bars, from quiet to loud: deep Aegean to gold."
  :type '(repeat color) :group 'orpheus)

(defcustom orpheus-vis-graphical t
  "Whether to draw SVG bars in a graphical frame."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-vis-buffer-name "*orpheus visualizer*"
  "Name of the visualizer's buffer."
  :type 'string :group 'orpheus)

(defvar orpheus-vis--process nil
  "The cava process, or nil when it is not running.")

(defvar orpheus-vis--timer nil
  "Timer that redraws the visualizer.")

(defvar orpheus-vis--data nil
  "The last frame of bar heights.")

(defvar orpheus-vis--accum ""
  "What is left of a frame that has not arrived whole.")

(defvar orpheus-vis--dirty nil
  "Whether a frame arrived that has not been drawn yet.")

(defvar orpheus-vis--image nil
  "The image of the last frame drawn, reused when the next one looks the same.")

(defvar orpheus-vis--image-key nil
  "What `orpheus-vis--image' was built from.")

;;; Platform and configuration

(defun orpheus-vis--input ()
  "Return the cava input method to use."
  (or orpheus-vis-input
      (pcase system-type
             ('gnu/linux 'pipewire)
             ('darwin 'portaudio)
             ('windows-nt 'wasapi)
             (_ 'pulse))))

(defun orpheus-vis--config-string (input bars framerate)
  "Return a cava configuration reading INPUT into BARS at FRAMERATE."
  (format "[general]
bars = %d
framerate = %d

[input]
method = %s

[output]
method = raw
raw_target = /dev/stdout
data_format = ascii
ascii_max_range = 100
bar_delimiter = 59
frame_delimiter = 10
channels = mono
"
          bars framerate input))

;;; Reading frames

(defun orpheus-vis--levels (accum chunk)
  "Return the levels in ACCUM followed by CHUNK, and the unfinished rest.
The car is the list of bar heights, or nil while no frame is complete."
  (let* ((text (concat accum chunk))
         (newline (string-match "\n" text)))
    (if (null newline)
      (cons nil text)
      ;; The match data must be read before anything else parses a string.
      (let ((frame (substring text 0 newline))
            (rest (substring text (match-end 0))))
        (cons (mapcar #'string-to-number
                      (split-string frame ";" t))
              rest)))))

(defun orpheus-vis--text-bar (level height)
  "Return a bar of HEIGHT characters filled to LEVEL percent."
  (let* ((height (max 1 height))
         (level (max 0 (min 100 (or level 0))))
         (filled (min height (round (* height (/ level 100.0))))))
    (concat (make-string filled orpheus-vis-bar-char)
            (make-string (- height filled) orpheus-vis-empty-char))))

(defun orpheus-vis--text (levels width height)
  "Return LEVELS as a block-character graph WIDTH by HEIGHT."
  (let* ((levels (if (and levels (> (length levels) 0))
                   levels (make-list (max 1 width) 0)))
         (bars (mapcar (lambda (level) (orpheus-vis--text-bar level height))
                       (seq-take levels (max 1 width))))
         (rows nil))
    (dotimes (row height)
      (push (apply #'string (mapcar (lambda (bar) (aref bar row)) bars)) rows))
    (string-join (nreverse rows) "\n")))

(defun orpheus-vis--color (level)
  "Return the colour for a bar at LEVEL."
  (let* ((colors (or orpheus-vis-colors '("#3f6f9e")))
         (index (min (1- (length colors))
                     (/ (* (max 0 (min 100 level)) (length colors)) 100))))
    (nth index colors)))

(defun orpheus-vis--svg (levels width height)
  "Return LEVELS as an SVG bar graph WIDTH by HEIGHT."
  (require 'svg)
  (let* ((svg (svg-create width height))
         (count (max 1 (length levels)))
         (slot (/ width count))
         (bar-width (max 1 (1- slot))))
    (svg-rectangle svg 0 0 width height :fill (face-attribute 'default :background))
    (dotimes (index count)
      (let* ((level (or (nth index levels) 0))
             (bar-height (max 1 (round (* height (/ level 100.0))))))
        (svg-rectangle svg (* index slot) (- height bar-height)
                       bar-width bar-height
                       :fill (orpheus-vis--color level)
                       :rx 1)))
    svg))

(defun orpheus-vis--svg-image (levels width height)
  "Return LEVELS as an image, an SVG bar graph WIDTH by HEIGHT.
Return nil when this Emacs cannot draw SVG, so the caller falls back to text
instead of signalling once per frame."
  ;; The require comes first: Emacs resolves `svg-image' before it evaluates
  ;; the arguments, so loading svg inside the argument would be too late.
  (require 'svg)
  (when (image-type-available-p 'svg)
    (svg-image (orpheus-vis--svg levels width height) :ascent 'center)))

(defun orpheus-vis--image (levels width height)
  "Return LEVELS as an image WIDTH by HEIGHT, reusing the last one when it fits.
Frames differ by a percent or two without looking different, and rebuilding
the SVG and its image for each of them is what costs the frame rate.

shortcut: a bar may lag a real change under 4% until the next bucket; revisit
if the bars look steppy."
  (let ((key (list width height
                   (mapcar (lambda (level)
                             (min 25 (/ (max 0 (min 100 (or level 0))) 4)))
                           levels))))
    (unless (equal key orpheus-vis--image-key)
      (setq orpheus-vis--image-key key
            orpheus-vis--image (orpheus-vis--svg-image levels width height)))
    orpheus-vis--image))

;;; Running cava

(defun orpheus-vis--filter (_process chunk)
  "Take CHUNK, one piece of cava's output, into the current frame."
  (let ((parsed (orpheus-vis--levels orpheus-vis--accum chunk)))
    (setq orpheus-vis--accum (cdr parsed))
    (when (car parsed)
      (setq orpheus-vis--data (car parsed)
            orpheus-vis--dirty t))))

(defun orpheus-vis--sentinel (process _event)
  "Forget the cava PROCESS that ended, and say where cava complained."
  (unless (process-live-p process)
    (setq orpheus-vis--process nil)
    (orpheus-vis-stop)
    (message "orpheus: cava stopped; see *orpheus cava*")))

(defun orpheus-vis-start ()
  "Start cava, unless it is already running."
  (interactive)
  (unless (process-live-p orpheus-vis--process)
    (orpheus-vis-stop)
    (let ((program (or (executable-find orpheus-vis-program)
                       orpheus-vis-program))
          (config (orpheus-cache-file "cava-config" "config")))
      (with-temp-file config
        (insert (orpheus-vis--config-string
                 (orpheus-vis--input) orpheus-vis-bars
                 orpheus-vis-framerate)))
      (setq orpheus-vis--accum ""
        orpheus-vis--data nil
        orpheus-vis--dirty t
        orpheus-vis--image nil
        orpheus-vis--image-key nil
        orpheus-vis--process
        (condition-case err
          (make-process :name "orpheus-cava"
            :command (list program "-p" config)
            :coding 'binary
            :buffer nil
            :noquery t
            ;; cava explains a missing device here.
            :stderr (get-buffer-create "*orpheus cava*")
            :filter #'orpheus-vis--filter
            :sentinel #'orpheus-vis--sentinel)
          (error (message "orpheus: cannot start cava: %s"
                          (error-message-string err))
            nil)))
      (when orpheus-vis--process
        ;; Two checks per frame: a timer that fires only at the frame rate
        ;; itself misses every other frame whenever the phases drift, and a
        ;; check costs nothing until a frame has actually arrived.
        (setq orpheus-vis--timer
          (run-with-timer 0 (/ 0.5 (max 1 orpheus-vis-framerate))
                          #'orpheus-vis--render))))))

(defun orpheus-vis-stop ()
  "Stop cava and its redrawing."
  (interactive)
  (when (timerp orpheus-vis--timer)
    (cancel-timer orpheus-vis--timer))
  (setq orpheus-vis--timer nil)
  (let ((process orpheus-vis--process))
    (setq orpheus-vis--process nil)
    (when (process-live-p process)
      (ignore-errors (delete-process process)))))

;;; Drawing

(defun orpheus-vis--render (&optional force)
  "Draw the last frame in the visualizer's buffer.
FORCE draws even when the buffer is not shown and even when no new frame
arrived.  Otherwise a frame is drawn once: cava's frames arrive at its own
pace, and rebuilding an image the reader cannot tell apart is what makes the
visualizer feel slow."
  (let ((buffer (get-buffer orpheus-vis-buffer-name)))
    (when (and (buffer-live-p buffer)
               (or force
                   (and (get-buffer-window buffer) orpheus-vis--dirty)))
      (setq orpheus-vis--dirty nil)
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (window (get-buffer-window buffer))
               (width (max 20 (if window (window-body-width window) 60)))
               (height (max 4 (if window (- (window-body-height window) 2) 12)))
               (levels (if (and orpheus-vis--data
                                (> (length orpheus-vis--data) 0))
                         orpheus-vis--data
                         (make-list orpheus-vis-bars 0))))
          (erase-buffer)
          (let ((image (and orpheus-vis-graphical (display-graphic-p)
                            (orpheus-vis--image (seq-take levels width)
                                                (* 8 width) (* 8 height)))))
            (if image
              (insert-image image " ")
              (insert (orpheus-vis--text levels width height))))
          (insert "\n")
          (insert (propertize
                   (format " %s, %d bars  (q stops)" (orpheus-vis--input)
                           (length levels))
                   'face 'orpheus-muted-face))
          (goto-char (point-min)))))))

(defun orpheus-visualizer ()
  "Show a spectrum visualizer of what the machine is playing."
  (interactive)
  (orpheus-vis-start)
  (let ((buffer (get-buffer-create orpheus-vis-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq-local mode-line-format " orpheus visualizer")
      (add-hook 'kill-buffer-hook #'orpheus-vis-stop nil t)
      (orpheus-vis--render t))
    (display-buffer buffer)
    (with-current-buffer buffer
      (orpheus-vis--render t))))

(provide 'orpheus-vis)

;;; orpheus-vis.el ends here
