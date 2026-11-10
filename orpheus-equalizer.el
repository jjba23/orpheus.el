;;; orpheus-equalizer.el --- Equalizer presets for the mpv backends -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; mpv carries ffmpeg's peaking equalizer, so the local and SSH backends can
;;; shape their sound.  The MPD backend cannot: its audio leaves through the
;;; daemon's own output, and Emacs never sees the samples.  The backend
;;; protocol says so by returning nil from `orpheus-backend-equalizer'.
;;;
;;; A preset is one gain per fixed band.  Five presets ship with the package.
;;; A preset the user saves lives in a JSON file, so it can be edited or shared
;;; without evaluating Lisp, and it replaces a built-in one of the same name.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'transient)
(require 'orpheus-backend)

(defcustom orpheus-equalizer-bands
  '(31 62 125 250 500 1000 2000 4000 8000 16000)
  "Frequencies, in Hz, that one preset gain controls.
The built-in presets assume the default ten bands; a saved preset needs one
gain for every frequency here."
  :type '(repeat integer) :group 'orpheus)

(defcustom orpheus-equalizer-max-gain 12
  "Largest boost or cut, in dB, that a preset may ask for."
  :type 'integer :group 'orpheus)

(defcustom orpheus-equalizer-width 1
  "Width of each band, in octaves.
A wider band overlaps its neighbours, which smooths the curve; the default
leaves the ten bands apart, as a graphic equalizer does."
  :type 'number :group 'orpheus)

(defcustom orpheus-equalizer-gain-step 1
  "Decibels one keystroke moves a band in the editor."
  :type 'integer :group 'orpheus)

(defcustom orpheus-equalizer-shift-step 3
  "Decibels one shifted keystroke moves a band in the editor."
  :type 'integer :group 'orpheus)

(defcustom orpheus-equalizer-tilt-step 3
  "Decibels `orpheus-equalizer-tilt' adds to the lowest band.
The highest band gets the same amount cut, and the bands between move in a
straight line from one to the other."
  :type 'integer :group 'orpheus)

(defcustom orpheus-equalizer-buffer-name "*orpheus equalizer*"
  "Name of the buffer that edits the bands visually."
  :type 'string :group 'orpheus)

(defcustom orpheus-equalizer-enabled nil
  "Whether the equalizer is applied to the current backend."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-equalizer-preset "flat"
  "Preset applied when the equalizer is enabled and none was loaded yet."
  :type 'string :group 'orpheus)

(defcustom orpheus-equalizer-preset-file
  (expand-file-name "orpheus/equalizer.json"
                    (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "JSON file that holds presets saved by the user.
Built-in presets are not written here; a saved preset with the same name
replaces the built-in one when the menu is built."
  :type 'file :group 'orpheus)

(defconst orpheus-equalizer-builtin-presets
  '(("flat" . (0 0 0 0 0 0 0 0 0 0))
    ("bass" . (6 5 3 1 0 0 0 0 0 0))
    ("vocal" . (-2 -1 0 2 4 4 2 1 0 0))
    ("treble" . (0 0 0 0 0 1 2 4 5 6))
    ("loudness" . (4 3 1 0 -1 0 1 2 3 4))
    ("night" . (-6 -4 -2 0 1 1 0 -1 -2 -3))
    ("podcast" . (-6 -4 -1 2 3 3 2 1 0 -2))
    ("acoustic" . (2 1 0 1 2 2 1 1 1 0))
    ("club" . (0 0 2 4 4 2 0 0 0 0))
    ("v-shape" . (5 4 2 0 -2 -2 -1 1 3 4)))
  "Presets that ship with orpheus, as (NAME . GAINS).")

(defvar orpheus-equalizer--gains nil
  "Gains of the preset last applied, or nil before the first apply.")

(defun orpheus-equalizer--clean (gains)
  "Return GAINS rounded and clamped to `orpheus-equalizer-max-gain'.
Signal a `user-error' unless GAINS holds one number per band."
  (unless (and (listp gains)
               (= (length gains) (length orpheus-equalizer-bands))
               (cl-every #'numberp gains))
    (user-error "Orpheus: a preset needs %d gains, one per band"
                (length orpheus-equalizer-bands)))
  (let ((limit orpheus-equalizer-max-gain))
    (mapcar (lambda (gain)
              (max (- limit) (min limit (round gain))))
            gains)))

(defun orpheus-equalizer-adjust (gains index delta)
  "Return GAINS with band INDEX moved by DELTA dB.
The result is rounded and clamped like any other preset."
  (let ((gains (append gains nil)))
    (unless (nth index gains)
      (user-error "Orpheus: there is no band %s" index))
    (setf (nth index gains) (+ (nth index gains) delta))
    (orpheus-equalizer--clean gains)))

(defun orpheus-equalizer-invert-gains (gains)
  "Return GAINS with every band flipped around zero."
  (orpheus-equalizer--clean (mapcar #'- gains)))

(defun orpheus-equalizer-tilt-gains (gains)
  "Return GAINS tilted: the bass lifted and the treble cut.
The middle stays where it is, so tilting then inverting is a treble lift."
  (let* ((gains (orpheus-equalizer--clean gains))
         (last (1- (length gains))))
    (orpheus-equalizer--clean
     (cl-mapcar (lambda (gain index)
                  (+ gain (if (zerop last)
                              0
                            (round (* orpheus-equalizer-tilt-step
                                      (- 1 (/ (* 2.0 index) last)))))))
                gains (number-sequence 0 last)))))

(defun orpheus-equalizer--filter (gains)
  "Return mpv's audio filter string for GAINS, or an empty string.
A zero gain is left out, so a flat preset removes the filter instead of
installing ten filters that do nothing."
  (let ((filters
         (cl-mapcar
          (lambda (frequency gain)
            (unless (zerop gain)
              (format "equalizer=frequency=%d:width_type=o:width=%s:gain=%d"
                      frequency orpheus-equalizer-width gain)))
          orpheus-equalizer-bands gains)))
    (mapconcat #'identity (delq nil filters) ",")))

(defun orpheus-equalizer--read-presets ()
  "Return the user presets in `orpheus-equalizer-preset-file'.
Each entry is (NAME . GAINS).  A file that does not parse, or one whose
entries do not fit the bands, signals a `user-error' instead of vanishing."
  (let ((file (expand-file-name orpheus-equalizer-preset-file)))
    (cond
     ((not (file-exists-p file)) nil)
     ((not (file-readable-p file))
      (user-error "Orpheus: cannot read equalizer presets in %s" file))
     (t
      (let* ((text (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
             (data (condition-case error
                       ;; json-parse-string turns object keys into symbols;
                       ;; json-read with this binding keeps the names strings.
                       (let ((json-object-type 'alist)
                             (json-key-type 'string))
                         (json-read-from-string text))
                     (error
                      (user-error
                       "Orpheus: cannot read equalizer presets in %s: %s"
                       file (error-message-string error))))))
        (unless (listp data)
          (user-error "Orpheus: %s does not hold a preset object" file))
        (mapcar
         (lambda (entry)
           (unless (and (stringp (car entry))
                        (not (string-empty-p (car entry))))
             (user-error "Orpheus: a preset in %s has no name" file))
           (cons (car entry)
                 (orpheus-equalizer--clean (append (cdr entry) nil))))
         data))))))

(defun orpheus-equalizer--write-presets (presets)
  "Write PRESETS to `orpheus-equalizer-preset-file' as JSON.
PRESETS is an alist of (NAME . GAINS), as `orpheus-equalizer--read-presets'
returns it.  The file is replaced atomically, so a failed write leaves the
old presets intact."
  (let* ((file (expand-file-name orpheus-equalizer-preset-file))
         (directory (file-name-directory file)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name ".equalizer-" directory)
                      nil ".json")))
      (unwind-protect
          (progn
            (with-temp-file temporary
              ;; json-serialize wants symbol keys for an object, so write the
              ;; braces and colons here; it still quotes the names and arrays.
              (insert "{"
                      (mapconcat
                       (lambda (entry)
                         (concat (json-serialize (car entry))
                                 ":"
                                 (json-serialize (vconcat (cdr entry)))))
                       presets ",")
                      "}\n"))
            (rename-file temporary file t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun orpheus-equalizer-presets ()
  "Return every preset as (NAME . GAINS), with user presets first.
A user preset replaces a built-in one of the same name."
  (let ((user (orpheus-equalizer--read-presets)))
    (append user
            (cl-remove-if (lambda (entry)
                            (assoc (car entry) user))
                          orpheus-equalizer-builtin-presets))))

(defun orpheus-equalizer--preset (name)
  "Return the gains of preset NAME."
  (let ((entry (assoc name (orpheus-equalizer-presets))))
    (unless entry
      (user-error "Orpheus: no equalizer preset called %s" name))
    (cdr entry)))

(defun orpheus-equalizer--current-gains ()
  "Return the gains in effect, resolving the configured preset once."
  (or orpheus-equalizer--gains
      (setq orpheus-equalizer--gains
            (orpheus-equalizer--clean
             (orpheus-equalizer--preset orpheus-equalizer-preset)))))

(defun orpheus-equalizer--redraw-editor ()
  "Draw the equalizer editor again when it is open."
  (when (buffer-live-p (get-buffer orpheus-equalizer-buffer-name))
    (with-current-buffer orpheus-equalizer-buffer-name
      (when (derived-mode-p 'orpheus-equalizer-mode)
        (orpheus-equalizer--render)))))

(defun orpheus-equalizer--commit (gains note)
  "Store GAINS, apply them to the backend, and say NOTE."
  (setq orpheus-equalizer--gains (orpheus-equalizer--clean gains)
        orpheus-equalizer-enabled t)
  (orpheus-equalizer--apply)
  (orpheus-equalizer--redraw-editor)
  (message "Orpheus: equalizer %s" note)
  orpheus-equalizer--gains)

(defun orpheus-equalizer--apply ()
  "Send the current equalizer to the backend.
Return non-nil when the backend took it.  A backend stores the filter even
while its player is not running, so the next start is already equalized."
  (let* ((gains (orpheus-equalizer--current-gains))
         (filters (if orpheus-equalizer-enabled
                      (orpheus-equalizer--filter gains)
                    ""))
         (backend orpheus-backend-current))
    (cond
     ((null backend)
      (message "Orpheus: no backend yet; run M-x orpheus")
      nil)
     ((orpheus-backend-equalizer backend filters) t)
     (t
      (message "Orpheus: %s cannot apply an equalizer; use the local or ssh backend"
               (orpheus-backend-name backend))
      nil))))

;;;###autoload
(defun orpheus-equalizer-enable ()
  "Turn the equalizer on and apply the current preset."
  (interactive)
  (setq orpheus-equalizer-enabled t)
  (orpheus-equalizer--apply)
  (orpheus-equalizer--redraw-editor))

;;;###autoload
(defun orpheus-equalizer-disable ()
  "Turn the equalizer off and leave the backend's audio unfiltered."
  (interactive)
  (setq orpheus-equalizer-enabled nil)
  (orpheus-equalizer--apply)
  (orpheus-equalizer--redraw-editor))

;;;###autoload
(defun orpheus-equalizer-toggle ()
  "Turn the equalizer on or off."
  (interactive)
  (if orpheus-equalizer-enabled
      (orpheus-equalizer-disable)
    (orpheus-equalizer-enable)))

;;;###autoload
(defun orpheus-equalizer-load-preset (name)
  "Load the equalizer preset NAME and turn the equalizer on."
  (interactive
   (list (completing-read "Equalizer preset: "
                          (mapcar #'car (orpheus-equalizer-presets))
                          nil t nil nil orpheus-equalizer-preset)))
  (let ((gains (orpheus-equalizer--preset name)))
    (setq orpheus-equalizer--gains (orpheus-equalizer--clean gains)
          orpheus-equalizer-preset name
          orpheus-equalizer-enabled t)
    (orpheus-equalizer--apply)
    (orpheus-equalizer--redraw-editor)
    (message "Orpheus: equalizer preset %s" name)))

;;;###autoload
(defun orpheus-equalizer-save-preset (name)
  "Save the current gains as the user preset NAME.
NAME is trimmed, and an empty one is refused.  Saving under a built-in's
name replaces that built-in preset in the menu."
  (interactive (list (read-string "Save equalizer preset as: ")))
  (let ((name (string-trim name)))
    (when (string-empty-p name)
      (user-error "Orpheus: a preset needs a name"))
    (let ((presets (cl-remove name (orpheus-equalizer--read-presets)
                              :key #'car :test #'equal)))
      (orpheus-equalizer--write-presets
       (cons (cons name (orpheus-equalizer--current-gains)) presets))
      (setq orpheus-equalizer-preset name)
      (orpheus-equalizer--redraw-editor)
      (message "Orpheus: saved equalizer preset %s" name))))

;;;###autoload
(defun orpheus-equalizer-delete-preset (name)
  "Delete the user preset NAME; a built-in preset cannot be deleted."
  (interactive
   (list (completing-read "Delete equalizer preset: "
                          (mapcar #'car (orpheus-equalizer--read-presets))
                          nil t)))
  (let ((presets (orpheus-equalizer--read-presets)))
    (unless (assoc name presets)
      (user-error "Orpheus: %s is not a saved equalizer preset" name))
    (orpheus-equalizer--write-presets
     (cl-remove name presets :key #'car :test #'equal))
    (message "Orpheus: deleted equalizer preset %s" name)))

;;;###autoload
(defun orpheus-equalizer-set-gains (gains)
  "Set the equalizer to GAINS, one dB value per band, and turn it on.
Interactive input is a line of numbers, for example
`0 2 3 1 0 -1 0 1 2 3'."
  (interactive
   (list (mapcar #'string-to-number
                 (split-string
                  (read-string (format "Gains for %d bands: "
                                       (length orpheus-equalizer-bands)))
                  "[ ,]+" t))))
  (orpheus-equalizer--commit
   gains
   (mapconcat (lambda (gain) (format "%+d" gain))
              (orpheus-equalizer--clean gains) " ")))

;;; Editing the curve

(defvar orpheus-equalizer--band 0
  "Band the editor's point is on, as an index into `orpheus-equalizer-bands'.")

(defvar orpheus-equalizer--markers nil
  "Vector of markers, one at the start of each band's row.")

(defvar orpheus-equalizer-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-equalizer-mouse-set)
    (define-key map [mouse-3] 'orpheus-equalizer-mouse-menu)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each band's row, so the mouse can shape it.")

(defvar orpheus-equalizer--context-menu
  (let ((map (make-sparse-keymap "Equalizer")))
    (define-key-after map [zero]
      '(menu-item "Zero this band" orpheus-equalizer-zero-band))
    (define-key-after map [flatten]
      '(menu-item "Reset every band" orpheus-equalizer-flatten))
    (define-key-after map [invert]
      '(menu-item "Invert the curve" orpheus-equalizer-invert))
    (define-key-after map [tilt]
      '(menu-item "Tilt: bass up, treble down" orpheus-equalizer-tilt))
    (define-key-after map [separator] '(menu-item "--"))
    (define-key-after map [toggle]
      '(menu-item "Enable or disable" orpheus-equalizer-toggle))
    (define-key-after map [width]
      '(menu-item "Band width in octaves…" orpheus-equalizer-set-width))
    (define-key-after map [range]
      '(menu-item "Largest gain in dB…" orpheus-equalizer-set-max-gain))
    map)
  "Menu opened by a right click on a band.")

(defun orpheus-equalizer--bar-string (gain)
  "Return GAIN as a bar of columns, each carrying the gain it means.
The middle column is zero; a positive gain fills to the right and a negative
one to the left, one column per dB.  A click reads the gain from the column
it lands on, so the bar is also the control."
  (let* ((max (abs orpheus-equalizer-max-gain))
         (parts nil))
    (dotimes (column (1+ (* 2 max)))
      (let* ((value (- column max))
             (filled (if (> gain 0)
                         (and (> value 0) (<= value gain))
                       (and (< gain 0) (>= value gain) (< value 0)))))
        (push (propertize (cond ((zerop value) "│")
                                (filled "█")
                                (t " "))
                          'orpheus-equalizer-gain value)
              parts)))
    (apply #'concat (nreverse parts))))

(defun orpheus-equalizer--band-index ()
  "Return the band the editor is on, kept inside the band list."
  (let ((count (length orpheus-equalizer-bands)))
    (setq orpheus-equalizer--band
          (max 0 (min (1- count) (or orpheus-equalizer--band 0))))
    orpheus-equalizer--band))

(defun orpheus-equalizer--render ()
  "Draw the equalizer editor from the gains in effect."
  (let ((inhibit-read-only t)
        (gains (orpheus-equalizer--current-gains))
        (band (orpheus-equalizer--band-index)))
    (setq orpheus-equalizer--markers nil)
    (erase-buffer)
    (insert
     (propertize "equalizer" 'face 'orpheus-status-face)
     "   "
     (if orpheus-equalizer-enabled
         (propertize "on" 'face 'orpheus-button-on-face)
       (propertize "off" 'face 'orpheus-muted-face))
     (format "   preset %s   %d bands   ±%d dB   %g octave%s\n\n"
             orpheus-equalizer-preset (length orpheus-equalizer-bands)
             (abs orpheus-equalizer-max-gain) orpheus-equalizer-width
             (if (equal 1 orpheus-equalizer-width) "" "s")))
    (cl-loop for frequency in orpheus-equalizer-bands
             for gain in gains
             for index from 0
             do
             (let ((start (point)))
               (insert (propertize (format "%6d Hz  " frequency)
                                   'face (if (= index band)
                                             'orpheus-current-face
                                           'orpheus-time-face)))
               (insert (orpheus-equalizer--bar-string gain))
               (insert (format "  %+3d dB\n" gain))
               (add-text-properties
                start (1- (point))
                (list 'keymap orpheus-equalizer-item-map
                      'orpheus-equalizer-band index
                      'mouse-face 'highlight))
               (push (copy-marker start) orpheus-equalizer--markers)))
    (setq orpheus-equalizer--markers
          (vconcat (nreverse orpheus-equalizer--markers)))
    (insert
     "\n"
     (propertize
      (concat " up, down or left, right: gain   M-up, M-down: 3 dB"
              "   n, p: band\n"
              " 0: zero this band   r: reset   i: invert   t: tilt"
              "   e: on or off\n"
              " P: load a preset   s: save   d: delete   q: bury"
              "   mouse: click a bar")
      'face 'orpheus-muted-face))
    (goto-char (if (and (vectorp orpheus-equalizer--markers)
                        (< band (length orpheus-equalizer--markers)))
                   (aref orpheus-equalizer--markers band)
                 (point-min)))))

(defun orpheus-equalizer--nudge (delta)
  "Move the selected band by DELTA dB and apply it at once."
  (let* ((index (orpheus-equalizer--band-index))
         (gains (orpheus-equalizer-adjust
                 (orpheus-equalizer--current-gains) index delta)))
    (orpheus-equalizer--commit
     gains (format "%d Hz %+d dB" (nth index orpheus-equalizer-bands)
                   (nth index gains)))))

(defun orpheus-equalizer-next-band (&optional n)
  "Move to the next band, or N bands down."
  (interactive "p")
  (setq orpheus-equalizer--band (+ (orpheus-equalizer--band-index) (or n 1)))
  (orpheus-equalizer--render))

(defun orpheus-equalizer-previous-band (&optional n)
  "Move to the previous band, or N bands up."
  (interactive "p")
  (setq orpheus-equalizer--band (- (orpheus-equalizer--band-index) (or n 1)))
  (orpheus-equalizer--render))

(defun orpheus-equalizer-increase (&optional n)
  "Raise the selected band by N steps."
  (interactive "p")
  (orpheus-equalizer--nudge (* (or n 1) orpheus-equalizer-gain-step)))

(defun orpheus-equalizer-decrease (&optional n)
  "Lower the selected band by N steps."
  (interactive "p")
  (orpheus-equalizer--nudge (* (- (or n 1)) orpheus-equalizer-gain-step)))

(defun orpheus-equalizer-increase-a-lot (&optional n)
  "Raise the selected band by N shifted steps."
  (interactive "p")
  (orpheus-equalizer--nudge (* (or n 1) orpheus-equalizer-shift-step)))

(defun orpheus-equalizer-decrease-a-lot (&optional n)
  "Lower the selected band by N shifted steps."
  (interactive "p")
  (orpheus-equalizer--nudge (* (- (or n 1)) orpheus-equalizer-shift-step)))

(defun orpheus-equalizer-zero-band ()
  "Set the selected band's gain to zero."
  (let* ((index (orpheus-equalizer--band-index))
         (gains (append (orpheus-equalizer--current-gains) nil)))
    (setf (nth index gains) 0)
    (orpheus-equalizer--commit
     gains (format "%d Hz flat" (nth index orpheus-equalizer-bands)))))

(defun orpheus-equalizer-mouse-set (event)
  "Set the band clicked in EVENT to the gain of the column clicked."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (point (posn-point start))
         (gain (and (integerp point)
                    (get-text-property point 'orpheus-equalizer-gain)))
         (index (and (integerp point)
                     (get-text-property point 'orpheus-equalizer-band))))
    (when (and (windowp window) (integerp point))
      (with-selected-window window (goto-char point)))
    (cond
     ((not (integerp gain))
      (message "orpheus: click inside a band's bar"))
     ((not (integerp index))
      (message "orpheus: there is no band here"))
     (t
      (setq orpheus-equalizer--band index)
      (let ((gains (append (orpheus-equalizer--current-gains) nil)))
        (setf (nth index gains) gain)
        (orpheus-equalizer--commit
         gains (format "%d Hz %+d dB" (nth index orpheus-equalizer-bands)
                       gain)))))))

(defun orpheus-equalizer-mouse-menu (event)
  "Open the equalizer menu for the click in EVENT."
  (interactive "e")
  (popup-menu orpheus-equalizer--context-menu (event-start event)))

(defun orpheus-equalizer-flatten ()
  "Set every band to zero."
  (interactive)
  (orpheus-equalizer--commit
   (make-list (length orpheus-equalizer-bands) 0) "flat"))

(defun orpheus-equalizer-invert ()
  "Flip the curve around zero: a cut becomes the same boost."
  (interactive)
  (orpheus-equalizer--commit
   (orpheus-equalizer-invert-gains (orpheus-equalizer--current-gains))
   "inverted"))

(defun orpheus-equalizer-tilt ()
  "Lift the bass and cut the treble a little further each time."
  (interactive)
  (orpheus-equalizer--commit
   (orpheus-equalizer-tilt-gains (orpheus-equalizer--current-gains))
   "tilted"))

(defun orpheus-equalizer-set-width (width)
  "Set the width of every band to WIDTH octaves and apply it."
  (interactive "nBand width in octaves: ")
  (setq orpheus-equalizer-width (abs width))
  (orpheus-equalizer--apply)
  (orpheus-equalizer--redraw-editor)
  (message "Orpheus: equalizer bands %.2g octaves wide"
           orpheus-equalizer-width))

(defun orpheus-equalizer-set-max-gain (gain)
  "Set the largest boost or cut to GAIN dB and clamp the curve to it."
  (interactive "nLargest equalizer gain in dB: ")
  (setq orpheus-equalizer-max-gain (abs gain)
        orpheus-equalizer--gains (orpheus-equalizer--clean
                                  (orpheus-equalizer--current-gains)))
  (orpheus-equalizer--apply)
  (orpheus-equalizer--redraw-editor)
  (message "Orpheus: equalizer range ±%d dB" orpheus-equalizer-max-gain))

(defvar orpheus-equalizer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") 'orpheus-equalizer-next-band)
    (define-key map (kbd "C-n") 'orpheus-equalizer-next-band)
    (define-key map (kbd "j") 'orpheus-equalizer-next-band)
    (define-key map (kbd "p") 'orpheus-equalizer-previous-band)
    (define-key map (kbd "C-p") 'orpheus-equalizer-previous-band)
    (define-key map (kbd "k") 'orpheus-equalizer-previous-band)
    (define-key map (kbd "<up>") 'orpheus-equalizer-increase)
    (define-key map (kbd "<right>") 'orpheus-equalizer-increase)
    (define-key map (kbd "<down>") 'orpheus-equalizer-decrease)
    (define-key map (kbd "<left>") 'orpheus-equalizer-decrease)
    (define-key map (kbd "M-<up>") 'orpheus-equalizer-increase-a-lot)
    (define-key map (kbd "M-<down>") 'orpheus-equalizer-decrease-a-lot)
    (define-key map (kbd "0") 'orpheus-equalizer-zero-band)
    (define-key map (kbd "r") 'orpheus-equalizer-flatten)
    (define-key map (kbd "i") 'orpheus-equalizer-invert)
    (define-key map (kbd "t") 'orpheus-equalizer-tilt)
    (define-key map (kbd "e") 'orpheus-equalizer-toggle)
    (define-key map (kbd "P") 'orpheus-equalizer-load-preset)
    (define-key map (kbd "s") 'orpheus-equalizer-save-preset)
    (define-key map (kbd "d") 'orpheus-equalizer-delete-preset)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap of `orpheus-equalizer-mode'.")

(define-derived-mode orpheus-equalizer-mode special-mode "Orpheus-Equalizer"
  "Major mode for shaping the equalizer with the keyboard or the mouse.
Up and down move the selected band; n and p change band; a right click opens
the curve-wide commands."
  (setq-local truncate-lines t)
  (buffer-disable-undo)
  (setq-local orpheus-equalizer--markers nil))

(defconst orpheus-equalizer--display-action
  '(display-buffer-in-side-window
    (side . bottom)
    (window-height . 0.35)
    (dedicated . t)
    (inhibit-same-window . t))
  "Where the equalizer is shown.
A side window keeps the player visible while giving the menu or the editor
room; the default transient side window is only as tall as its contents.")

(defun orpheus-equalizer-editor ()
  "Open the equalizer as a visual editor of every band.
The menu on E is the simple mode: presets, on and off.  This is the advanced
one, with the curve itself, its width and the range it may move in."
  (interactive)
  (let ((buffer (get-buffer-create orpheus-equalizer-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'orpheus-equalizer-mode)
        (orpheus-equalizer-mode))
      (orpheus-equalizer--render))
    (display-buffer buffer orpheus-equalizer--display-action)))

(transient-define-prefix orpheus-equalizer-menu--transient ()
  "The equalizer: presets, saved presets and gain editing."
  [["Equalizer"
    ("e" "Enable or disable" orpheus-equalizer-toggle)
    ("f" "Flat" (lambda () (interactive) (orpheus-equalizer-load-preset "flat")))
    ("b" "Bass" (lambda () (interactive) (orpheus-equalizer-load-preset "bass")))
    ("v" "Vocal" (lambda () (interactive) (orpheus-equalizer-load-preset "vocal")))
    ("t" "Treble" (lambda () (interactive) (orpheus-equalizer-load-preset "treble")))
    ("l" "Loudness" (lambda () (interactive) (orpheus-equalizer-load-preset "loudness")))
    ("n" "Night" (lambda () (interactive) (orpheus-equalizer-load-preset "night")))
    ("c" "Club" (lambda () (interactive) (orpheus-equalizer-load-preset "club")))
    ("k" "Podcast" (lambda () (interactive) (orpheus-equalizer-load-preset "podcast")))]
   ["Your presets"
    ("p" "Load a preset..." orpheus-equalizer-load-preset)
    ("s" "Save the current gains..." orpheus-equalizer-save-preset)
    ("d" "Delete a saved preset..." orpheus-equalizer-delete-preset)
    ("g" "Set the ten gains..." orpheus-equalizer-set-gains)]]
  [["Advanced"
    ("v" "Edit the bands visually" orpheus-equalizer-editor)
    ("w" "Band width in octaves..." orpheus-equalizer-set-width)
    ("m" "Largest gain in dB..." orpheus-equalizer-set-max-gain)
    ("0" "Reset every band" orpheus-equalizer-flatten)
    ("i" "Invert the curve" orpheus-equalizer-invert)
    ("t" "Tilt: bass up, treble down" orpheus-equalizer-tilt)]])

;;;###autoload
(defun orpheus-equalizer-menu ()
  "The equalizer: presets, saved presets and gain editing."
  (interactive)
  (let ((transient-display-buffer-action orpheus-equalizer--display-action))
    (orpheus-equalizer-menu--transient)))

(provide 'orpheus-equalizer)

;;; orpheus-equalizer.el ends here
