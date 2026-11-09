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
    ("loudness" . (4 3 1 0 -1 0 1 2 3 4)))
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

(defun orpheus-equalizer--filter (gains)
  "Return mpv's audio filter string for GAINS, or an empty string.
A zero gain is left out, so a flat preset removes the filter instead of
installing ten filters that do nothing."
  (let ((filters
         (cl-mapcar
          (lambda (frequency gain)
            (unless (zerop gain)
              (format "equalizer=frequency=%d:width_type=o:width=1:gain=%d"
                      frequency gain)))
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
  (orpheus-equalizer--apply))

;;;###autoload
(defun orpheus-equalizer-disable ()
  "Turn the equalizer off and leave the backend's audio unfiltered."
  (interactive)
  (setq orpheus-equalizer-enabled nil)
  (orpheus-equalizer--apply))

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
  (setq orpheus-equalizer--gains (orpheus-equalizer--clean gains)
        orpheus-equalizer-enabled t)
  (orpheus-equalizer--apply)
  (message "Orpheus: equalizer gains %s"
           (mapconcat (lambda (gain) (format "%+d" gain))
                      orpheus-equalizer--gains " ")))

(defconst orpheus-equalizer--display-action
  '(display-buffer-in-side-window
    (side . bottom)
    (window-height . 0.35)
    (dedicated . t)
    (inhibit-same-window . t))
  "Where the equalizer menu is shown.
A side window keeps the player visible while giving the two columns room;
the default transient side window is only as tall as its contents.")

(transient-define-prefix orpheus-equalizer-menu--transient ()
  "The equalizer: presets, saved presets and gain editing."
  [["Equalizer"
    ("e" "Enable or disable" orpheus-equalizer-toggle)
    ("f" "Flat" (lambda () (interactive) (orpheus-equalizer-load-preset "flat")))
    ("b" "Bass" (lambda () (interactive) (orpheus-equalizer-load-preset "bass")))
    ("v" "Vocal" (lambda () (interactive) (orpheus-equalizer-load-preset "vocal")))
    ("t" "Treble" (lambda () (interactive) (orpheus-equalizer-load-preset "treble")))
    ("l" "Loudness" (lambda () (interactive) (orpheus-equalizer-load-preset "loudness")))]
   ["Your presets"
    ("p" "Load a preset..." orpheus-equalizer-load-preset)
    ("s" "Save the current gains..." orpheus-equalizer-save-preset)
    ("d" "Delete a saved preset..." orpheus-equalizer-delete-preset)
    ("g" "Set the ten gains..." orpheus-equalizer-set-gains)]])

;;;###autoload
(defun orpheus-equalizer-menu ()
  "The equalizer: presets, saved presets and gain editing."
  (interactive)
  (let ((transient-display-buffer-action orpheus-equalizer--display-action))
    (orpheus-equalizer-menu--transient)))

(provide 'orpheus-equalizer)

;;; orpheus-equalizer.el ends here
