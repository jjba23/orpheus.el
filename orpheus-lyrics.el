;;; orpheus-lyrics.el --- Lyrics for the track that is playing -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Lyrics are read where each backend keeps them, because no single place
;;; holds them everywhere.  The local backend looks beside the audio file and
;;; then inside it; SSH runs one quoted command on the remote shell for the
;;; same two names; MPD either reads the sidecar under its library root or asks
;;; the daemon for the tags of the file, which include the ones it does not
;;; index.  A backend only has to answer `orpheus-backend-lyrics'.
;;;
;;; Two formats cover most libraries: LRC, whose `[mm:ss.xx]' marks make the
;;; lines follow the music, and plain text.  An embedded tag is read as LRC
;;; when it carries times and as text otherwise, so a file that says which is
;;; which once is not asked twice.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'orpheus-backend)

(declare-function orpheus--command "orpheus" (action &optional value))
(declare-function orpheus-play-pause "orpheus" ())
(declare-function orpheus-next "orpheus" ())
(declare-function orpheus-previous "orpheus" ())

;;; Settings

(defcustom orpheus-lyrics-follow t
  "Whether the lyrics window follows the line that is playing.
When nil the text stays where it is and the active line moves on its own,
which is easier to read while browsing."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-lyrics-buffer-name "*orpheus lyrics*"
  "Name of the buffer that shows the lyrics."
  :type 'string :group 'orpheus)

;;; Reading the formats

(defconst orpheus-lyrics--time-regexp
  "\\[\\([0-9]+\\):\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\\]"
  "LRC time tag: minutes, seconds and an optional fraction.")

(defun orpheus-lyrics--offset (text)
  "Return the LRC offset TEXT declares, in seconds, or zero.
A positive offset means the times are late, so it is subtracted; that is what
players do with `[offset:+500]'."
  (let ((position 0)
        (offset 0))
    (while (string-match "\\[offset:\\([+-]?[0-9]+\\)\\]" text position)
      (setq offset (/ (string-to-number (match-string 1 text)) 1000.0)
            position (match-end 0)))
    offset))

(defun orpheus-lyrics--tag-line-p (line)
  "Return non-nil when LINE is LRC metadata such as `[ar:Band]'."
  (string-match-p "\\`\\[[A-Za-z]+:.*\\]\\'" line))

(defun orpheus-lyrics--parse (text)
  "Return TEXT as a list of (SECONDS . LINE).
SECONDS is nil for lyrics without timing.  Timed lines are kept in the order
the file gives them, and a line with several times yields one entry each."
  (let* ((text (string-replace "\r" "" (or text "")))
         (offset (orpheus-lyrics--offset text))
         (lines nil))
    (dolist (line (split-string text "\n"))
      (let ((rest line)
            (times nil))
        (while (string-match (concat "\\`" orpheus-lyrics--time-regexp) rest)
          (let* ((fraction (or (match-string 3 rest) ""))
                 (seconds (+ (* 60 (string-to-number (match-string 1 rest)))
                             (string-to-number (match-string 2 rest))
                             (if (string-empty-p fraction)
                                 0
                               (/ (string-to-number fraction)
                                  (expt 10.0 (length fraction))))
                             (- offset))))
            (push seconds times))
          (setq rest (substring rest (match-end 0))))
        (cond
         (times
          (let ((line-text (string-trim rest)))
            (dolist (time (nreverse times))
              (push (cons time line-text) lines))))
         ((orpheus-lyrics--tag-line-p line) nil)
         ((string-empty-p (string-trim line)) (push (cons nil "") lines))
         (t (push (cons nil (string-trim line)) lines)))))
    (let ((lines (nreverse lines)))
      (while (and lines
                  (null (car (car (last lines))))
                  (equal (cdr (car (last lines))) ""))
        (setq lines (butlast lines)))
      (while (and lines
                  (null (car (car lines)))
                  (equal (cdr (car lines)) ""))
        (pop lines))
      lines)))

(defun orpheus-lyrics--index-at (lines elapsed)
  "Return the index in LINES of the last line timed at or before ELAPSED.
Answer nil before the first timed line, or when LINES carries no times."
  (let ((index nil)
        (position 0))
    (dolist (line lines)
      (when (and (numberp (car line)) (<= (car line) elapsed))
        (setq index position))
      (setq position (1+ position)))
    index))

(defun orpheus-lyrics--from-text (text source)
  "Return TEXT as an `orpheus-lyrics' read from SOURCE."
  (let ((lines (orpheus-lyrics--parse text)))
    (orpheus-lyrics-create
     :source source
     :synced (cl-some (lambda (line) (numberp (car line))) lines)
     :lines lines)))

(defun orpheus-lyrics--sidecar (file)
  "Return the lyrics file beside FILE, or nil.
A `.lrc' file wins over a `.txt' one.  Names are compared without regard to
case, because libraries are rarely consistent about it."
  (let* ((directory (file-name-directory (expand-file-name file)))
         (stem (downcase (file-name-base file)))
         (candidates nil))
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t))
        (when (and (file-regular-p path)
                   (equal (downcase (file-name-base path)) stem)
                   (member (downcase (or (file-name-extension path) ""))
                           '("lrc" "txt")))
          (push path candidates))))
    (or (cl-find-if (lambda (path)
                      (equal "lrc" (downcase (or (file-name-extension path) ""))))
                    candidates)
        (car candidates))))

(defun orpheus-lyrics--tag-value (tags)
  "Return the lyrics in TAGS, an alist of tag names and values, or nil.
The names are compared without regard to case, which covers ID3's USLT,
Vorbis' LYRICS and MP4's ©lyr as ffprobe reports them."
  (cl-loop for (name . value) in tags
           when (and (stringp value)
                     (member (downcase (format "%s" name))
                             '("lyrics" "lyric" "unsyncedlyrics" "©lyr")))
           return value))

(defun orpheus-lyrics--embedded (file)
  "Return the lyrics tag inside FILE as a string, or nil.
ffprobe reads the formats a music library really uses, and a file with no tag
answers nil rather than an error."
  (when (and (file-regular-p file) (executable-find "ffprobe"))
    (let ((output (ignore-errors
                    (with-temp-buffer
                      (when (zerop (call-process
                                    "ffprobe" nil t nil "-v" "quiet"
                                    "-of" "json"
                                    "-show_entries" "format_tags:stream_tags"
                                    file))
                        (buffer-string))))))
      (when output
        (let ((data (ignore-errors
                      (json-parse-string output :object-type 'alist))))
          (when data
            (cl-loop for container in (append (alist-get 'streams data)
                                              (list (alist-get 'format data)))
                     for tags = (and container (alist-get 'tags container))
                     for text = (orpheus-lyrics--tag-value tags)
                     when text return text)))))))

(defun orpheus-lyrics--from-file (file)
  "Return the lyrics of FILE, from the file beside it or from inside it."
  (when (and file (file-regular-p file))
    (let ((sidecar (orpheus-lyrics--sidecar file)))
      (if sidecar
          (orpheus-lyrics--from-text
           (with-temp-buffer
             (insert-file-contents sidecar)
             (buffer-string))
           sidecar)
        (let ((embedded (orpheus-lyrics--embedded file)))
          (when embedded
            (orpheus-lyrics--from-text embedded file)))))))

(defun orpheus-lyrics--from-tags (lines)
  "Return the lyrics in LINES, a daemon's tag reply, or nil.
LINES is a list of (NAME . VALUE) as MPD's `readcomments' returns it."
  (let ((text (cl-loop for (name . value) in lines
                       when (and (stringp value)
                                 (member (downcase (format "%s" name))
                                         '("lyrics" "lyric" "unsyncedlyrics")))
                       return value)))
    (when text
      (orpheus-lyrics--from-text text "tags"))))

;;; Showing them

(defconst orpheus-lyrics--display-action
  '(display-buffer-in-side-window
    (side . bottom)
    (window-height . 0.4)
    (dedicated . t)
    (inhibit-same-window . t))
  "Where the lyrics are shown.
A side window keeps the player and its tab visible while the words scroll.")


(defvar orpheus-lyrics--track-uri nil
  "URI the shown lyrics belong to, or nil before the first read.")

(defvar orpheus-lyrics--lyrics nil
  "The `orpheus-lyrics' the buffer is drawn from, or nil.")

(defvar orpheus-lyrics--notice nil
  "Sentence shown when there are no lyrics to draw.")

(defvar orpheus-lyrics--index 'unknown
  "The line that was marked when the buffer was last drawn.")

(defvar orpheus-lyrics--markers nil
  "Vector of markers, one at the start of each drawn line.")

(defvar orpheus-lyrics-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'orpheus-lyrics-mouse-seek)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap put on each lyrics line, so a click seeks to it.")

(defvar orpheus-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'orpheus-lyrics-seek)
    (define-key map (kbd "SPC") 'orpheus-play-pause)
    (define-key map (kbd ">") 'orpheus-next)
    (define-key map (kbd "<") 'orpheus-previous)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "g") 'orpheus-lyrics-reload)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap of `orpheus-lyrics-mode'.")

(define-derived-mode orpheus-lyrics-mode special-mode "Orpheus-Lyrics"
  "Major mode for the lyrics of the track that is playing.
RET seeks to the line at point, SPC plays or pauses, and g reads the lyrics
again."
  (setq-local truncate-lines nil)
  (setq-local line-spacing 3)
  (buffer-disable-undo)
  (setq-local orpheus-lyrics--track-uri nil)
  (setq-local orpheus-lyrics--lyrics nil)
  (setq-local orpheus-lyrics--markers nil)
  (setq-local orpheus-lyrics--index 'unknown))

(defun orpheus-lyrics--elapsed ()
  "Return how far the current track has played, in seconds, or nil."
  (let ((status orpheus-status-current))
    (and status (orpheus-status-elapsed status))))

(defun orpheus-lyrics--current-index ()
  "Return the index of the lyrics line that is playing, or nil."
  (let ((lyrics orpheus-lyrics--lyrics)
        (elapsed (orpheus-lyrics--elapsed)))
    (when (and lyrics elapsed (orpheus-lyrics-synced lyrics))
      (orpheus-lyrics--index-at (orpheus-lyrics-lines lyrics) elapsed))))

(defun orpheus-lyrics--header ()
  "Return the two lines that name the track and where the lyrics came from."
  (let ((track orpheus-track-current)
        (lyrics orpheus-lyrics--lyrics))
    (concat
     (propertize "lyrics" 'face 'orpheus-status-face)
     (if track (concat "  " (orpheus-track-label track)) "")
     (if (and lyrics (stringp (orpheus-lyrics-source lyrics)))
         (format "  (%s)" (file-name-nondirectory (orpheus-lyrics-source lyrics)))
       "")
     "\n\n")))

(defun orpheus-lyrics--follow ()
  "Scroll the lyrics window to the line that is playing, when asked to."
  (let ((markers orpheus-lyrics--markers)
        (index orpheus-lyrics--index))
    (when (and orpheus-lyrics-follow
               (vectorp markers)
               (integerp index)
               (< index (length markers)))
      (let ((window (get-buffer-window (current-buffer))))
        (when window
          (let ((position (aref markers index)))
            (unless (and (>= position (window-start window))
                         (< position (window-end window)))
              (set-window-start window position))))))))

(defun orpheus-lyrics--render ()
  "Draw the lyrics, marking the line that is playing."
  (let ((inhibit-read-only t)
        (lyrics orpheus-lyrics--lyrics)
        (index (orpheus-lyrics--current-index)))
    (setq orpheus-lyrics--index index
          orpheus-lyrics--markers nil)
    (erase-buffer)
    (insert (orpheus-lyrics--header))
    (cond
     ((null lyrics)
      (insert (propertize
               (or orpheus-lyrics--notice "No lyrics for this track.")
               'face 'orpheus-muted-face)
              "\n"))
     ((null (orpheus-lyrics-lines lyrics))
      (insert (propertize "This track's lyrics are empty."
                          'face 'orpheus-muted-face)
              "\n"))
     (t
      (let ((position 0))
        (dolist (line (orpheus-lyrics-lines lyrics))
          (let ((start (point))
                (time (car line)))
            (insert (if (numberp time)
                        (format "%8s  %s\n" (orpheus-format-time time)
                                (cdr line))
                      (format "%8s  %s\n" "" (cdr line))))
            (add-text-properties
             start (1- (point))
             (list 'orpheus-lyrics-line position
                   'orpheus-lyrics-time (and (numberp time) time)
                   'keymap orpheus-lyrics-line-map
                   'mouse-face 'highlight))
            (when (eql position index)
              (add-text-properties start (1- (point))
                                   '(face orpheus-current-face)))
            (push (copy-marker start) orpheus-lyrics--markers)
            (setq position (1+ position))))
        (setq orpheus-lyrics--markers
              (vconcat (nreverse orpheus-lyrics--markers))))
      (insert "\n"
              (propertize
               (concat " RET seek to this line   SPC play or pause"
                       "   g refresh   q bury")
               'face 'orpheus-muted-face))))
    (orpheus-lyrics--follow)))

(defun orpheus-lyrics--load ()
  "Ask the backend for the current track's lyrics and draw them."
  (let* ((backend orpheus-backend-current)
         (track orpheus-track-current)
         (uri (and track (orpheus-track-uri track)))
         (buffer (current-buffer)))
    (cond
     ((null backend)
      (setq orpheus-lyrics--track-uri nil
            orpheus-lyrics--lyrics nil
            orpheus-lyrics--notice
            "Start the player first, or run M-x orpheus.")
      (orpheus-lyrics--render))
     ((null uri)
      (setq orpheus-lyrics--track-uri nil
            orpheus-lyrics--lyrics nil
            orpheus-lyrics--notice "Nothing is playing yet.")
      (orpheus-lyrics--render))
     (t
      (setq orpheus-lyrics--track-uri uri
            orpheus-lyrics--lyrics nil
            orpheus-lyrics--notice "Looking for lyrics…")
      (orpheus-lyrics--render)
      (orpheus-backend-lyrics
       backend track
       (lambda (lyrics)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (equal uri orpheus-lyrics--track-uri)
               (setq orpheus-lyrics--lyrics lyrics
                     orpheus-lyrics--notice
                     (unless lyrics
                       (concat "No lyrics found.  orpheus reads .lrc or .txt"
                               " beside the track, and lyrics tags inside it.")))
               (orpheus-lyrics--render))))))))))

(defun orpheus-lyrics-tick ()
  "Keep the lyrics window in step with the track and the position."
  (let ((buffer (get-buffer orpheus-lyrics-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((track orpheus-track-current)
               (uri (and track (orpheus-track-uri track))))
          (if (not (equal uri orpheus-lyrics--track-uri))
              (orpheus-lyrics--load)
            (unless (eql (orpheus-lyrics--current-index)
                         orpheus-lyrics--index)
              (orpheus-lyrics--render))))))))

;;; Commands

(defun orpheus-lyrics-open ()
  "Show the lyrics of the current track and follow the playback.
The backend reads the words where it keeps them: a sidecar file next to a
local or SSH track, or the daemon's own tags for MPD."
  (interactive)
  (let ((buffer (get-buffer-create orpheus-lyrics-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'orpheus-lyrics-mode)
        (orpheus-lyrics-mode))
      (orpheus-lyrics--load))
    (display-buffer buffer orpheus-lyrics--display-action)))

(defun orpheus-lyrics-reload ()
  "Read the lyrics of the current track again."
  (interactive)
  (setq orpheus-lyrics--track-uri nil)
  (orpheus-lyrics--load))

(defun orpheus-lyrics-seek ()
  "Play from the time of the lyrics line at point."
  (interactive)
  (let ((time (get-text-property (point) 'orpheus-lyrics-time))
        (status orpheus-status-current))
    (cond
     ((not (numberp time))
      (message "orpheus: this line is not timed"))
     ((null status)
      (message "orpheus: nothing is playing"))
     (t
      (orpheus--command 'seek (- time (or (orpheus-status-elapsed status) 0)))
      (message "orpheus: %s" (orpheus-format-time time))))))

(defun orpheus-lyrics-mouse-seek (event)
  "Seek to the lyrics line clicked in EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (point (posn-point start)))
    (when (and (windowp window) (integerp point))
      (with-selected-window window
        (goto-char point)
        (orpheus-lyrics-seek)))))

(provide 'orpheus-lyrics)

;;; orpheus-lyrics.el ends here
