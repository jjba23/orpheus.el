;;; orpheus-test.el --- ERT tests for orpheus -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT suite for orpheus.el.  The pure helpers are checked directly, and the
;; asynchronous MPD client is driven against a fake MPD server started in this
;; same process on a loopback port, so the suite needs no daemon, no network
;; and no audio device.
;;
;; Run it with:
;;
;;   emacs -Q --batch -L . -L test -l test/orpheus-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; or simply `maak' in the project root.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(when (and load-file-name (not (featurep 'orpheus)))
  (add-to-list 'load-path
               (expand-file-name ".." (file-name-directory load-file-name))))
(require 'orpheus)

(declare-function imagep "image.c" (object))


;;; The fake MPD server
;;
;; MPD speaks a line protocol over TCP or a unix socket: a greeting, then
;; commands answered by `key: value' lines and a final `OK' or `ACK'.  The
;; server below speaks just enough of it to exercise the client's state
;; machine, including a binary payload, which is how album art arrives.

(defvar orpheus-test--mpd-handler nil
  "Function called with each command line the fake MPD receives.")

(defconst orpheus-test--source
  (expand-file-name "../orpheus.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the package source under test.")

(defun orpheus-test--documentation-files ()
  "Return README.org and every Org page under docs/."
  (let* ((root (file-name-directory orpheus-test--source))
         (docs (expand-file-name "docs" root)))
    (cons (expand-file-name "README.org" root)
          (and (file-directory-p docs)
               (directory-files docs t "\\.org\\'")))))

(defvar orpheus-test--mpd-requests nil
  "Command lines the fake MPD received, most recent first.")

(defvar orpheus-test--mpd-accum ""
  "Partial command line the fake MPD has received.")

(defvar orpheus-test--mpd-server nil
  "The fake MPD server process.")

(defvar orpheus-test--mpd-port nil
  "Port the fake MPD listens on.")

(defun orpheus-test--mpd-log (_server client message)
  "Remember CLIENT and greet it, when MESSAGE says it was accepted."
  (when (string-prefix-p "accept" message)
    (process-send-string client "OK MPD 0.23.12\n")))

(defun orpheus-test--mpd-filter (client chunk)
  "Answer each command line in CHUNK through `orpheus-test--mpd-handler'."
  (setq orpheus-test--mpd-accum (concat orpheus-test--mpd-accum chunk))
  (while (string-match "\n" orpheus-test--mpd-accum)
    (let ((line (substring orpheus-test--mpd-accum 0 (match-beginning 0))))
      (setq orpheus-test--mpd-accum (substring orpheus-test--mpd-accum
                                               (match-end 0)))
      (unless (string-empty-p line)
        (push line orpheus-test--mpd-requests)
        (let ((response (funcall orpheus-test--mpd-handler line)))
          (when response
            (process-send-string client response)))))))

(defmacro orpheus-test--with-fake-mpd (handler &rest body)
  "Run BODY with a fake MPD server on `orpheus-test--mpd-port'.
HANDLER is a function of one command line returning the response to send,
or nil to stay silent."
  (declare (indent 1))
  `(let ((orpheus-test--mpd-handler ,handler)
         (orpheus-test--mpd-accum "")
         (orpheus-test--mpd-requests nil))
     (setq orpheus-test--mpd-server
           (make-network-process :name "orpheus-test-mpd"
                                 :server t :host "127.0.0.1" :service t
                                 :family 'ipv4 :coding 'binary :noquery t
                                 :log #'orpheus-test--mpd-log
                                 :filter #'orpheus-test--mpd-filter))
     (setq orpheus-test--mpd-port
           (process-contact orpheus-test--mpd-server :service))
     (unwind-protect
         (progn ,@body)
       (dolist (process (process-list))
         (when (string-prefix-p "orpheus" (process-name process))
           (ignore-errors (delete-process process)))))))

(defun orpheus-test--u64-bytes (value)
  "Return VALUE as eight big-endian bytes."
  (apply #'unibyte-string
         (cl-loop for shift from 56 downto 0 by 8
                  collect (logand (ash value (- shift)) 255))))

(defun orpheus-test--u24-bytes (value)
  "Return VALUE as three big-endian bytes."
  (apply #'unibyte-string
         (cl-loop for shift from 16 downto 0 by 8
                  collect (logand (ash value (- shift)) 255))))

(defun orpheus-test--flac-image (rate total tracks)
  "Return a FLAC file holding a stream of RATE and TOTAL and TRACKS.
TRACKS is a list of (SAMPLE-OFFSET . NUMBER), lead-out track included."
  (let* ((stream (concat
                  (make-string 10 0)
                  (let ((bits (logior (ash rate 44) (ash 1 41) (ash 15 36)
                                      total)))
                    (apply #'unibyte-string
                           (cl-loop for shift from 56 downto 0 by 8
                                    collect (logand (ash bits (- shift)) 255))))
                  (make-string 16 0)))
         (cuesheet (concat
                    (make-string 128 0)
                    (orpheus-test--u64-bytes 88200)
                    (unibyte-string 128)
                    (make-string 258 0)
                    (unibyte-string (length tracks))
                    (mapconcat
                     (lambda (track)
                       (concat (orpheus-test--u64-bytes (car track))
                               (unibyte-string (cdr track))
                               (make-string 12 0)
                               (unibyte-string 0)
                               (make-string 13 0)
                               (unibyte-string 1)
                               (orpheus-test--u64-bytes (car track))
                               (unibyte-string 1 0 0 0)))
                     tracks ""))))
    (concat "fLaC"
            (unibyte-string 0) (orpheus-test--u24-bytes (length stream)) stream
            (unibyte-string 133) (orpheus-test--u24-bytes (length cuesheet))
            cuesheet)))

(defun orpheus-test--write-flac-image (file)
  "Write a small FLAC image with three tracks to FILE."
  (with-temp-file file
    (set-buffer-multibyte nil)
    (let ((coding-system-for-write 'binary))
      (insert (orpheus-test--flac-image
               44100 1323000
               '((0 . 1) (441000 . 2) (882000 . 3)
                 (1323000 . 170)))))))

(defun orpheus-test--le32-bytes (value)
  "Return VALUE as four little-endian bytes."
  (apply #'unibyte-string
         (cl-loop for shift from 0 to 24 by 8
                  collect (logand (ash value (- shift)) 255))))

(defun orpheus-test--flac-cue-comment (text)
  "Return a FLAC Vorbis comment block holding TEXT as a CUESHEET tag."
  (let* ((vendor (encode-coding-string "orpheus-test" 'utf-8))
         (comment (encode-coding-string (concat "CUESHEET=" text) 'utf-8))
         (body (concat (orpheus-test--le32-bytes (length vendor)) vendor
                       (orpheus-test--le32-bytes 1)
                       (orpheus-test--le32-bytes (length comment)) comment)))
    (concat (unibyte-string 132) (orpheus-test--u24-bytes (length body)) body)))

(defun orpheus-test--write-flac-cue (file)
  "Write a FLAC with three titled tracks in a CUESHEET comment to FILE."
  (with-temp-file file
    (set-buffer-multibyte nil)
    (let ((coding-system-for-write 'binary))
      (insert
       "fLaC"
       (orpheus-test--flac-cue-comment
        (concat "PERFORMER \"Band\"\nTITLE \"Record\"\n"
                "TRACK 01 AUDIO\n  TITLE \"First\"\n  INDEX 01 00:00:00\n"
                "TRACK 02 AUDIO\n  TITLE \"Second\"\n  INDEX 01 00:10:00\n"
                "TRACK 03 AUDIO\n  TITLE \"Third\"\n  INDEX 01 00:20:00\n"))))))

(defun orpheus-test--flac-info-bytes (rate samples seeks)
  "Return a FLAC header of RATE and SAMPLES with SEEKS as (SAMPLE . OFFSET).
The header ends with an empty last metadata block, as a real file does."
  (let* ((stream (concat
                  (make-string 10 0)
                  (let ((bits (logior (ash rate 44) (ash 1 41) (ash 15 36)
                                      samples)))
                    (apply #'unibyte-string
                           (cl-loop for shift from 56 downto 0 by 8
                                    collect (logand (ash bits (- shift)) 255))))
                  (make-string 16 0)))
         (table (mapconcat
                 (lambda (seek)
                   (concat (orpheus-test--u64-bytes (car seek))
                           (orpheus-test--u64-bytes (cdr seek))
                           (unibyte-string 16 0)))
                 seeks "")))
    (concat "fLaC"
            (unibyte-string 0) (orpheus-test--u24-bytes (length stream)) stream
            (if seeks
                (concat (unibyte-string 3)
                        (orpheus-test--u24-bytes (length table)) table)
              "")
            (unibyte-string 132 0 0 0))))

(defun orpheus-test--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, or TIMEOUT seconds pass.
Return PREDICATE's last value, so a test can assert on it."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defun orpheus-test--mpd-client ()
  "Return an MPD backend pointed at the fake server."
  (make-orpheus-mpd :host "127.0.0.1" :port orpheus-test--mpd-port))


;;; Package surface

(ert-deftest orpheus-test/provides-feature ()
  "The package loads and provides its feature."
  (should (featurep 'orpheus)))

(ert-deftest orpheus-test/group-and-faces-exist ()
  "The customization group and the UI faces are defined."
  (should (get 'orpheus 'custom-group))
  (dolist (face '(orpheus-status-face orpheus-title-face orpheus-artist-face
                  orpheus-album-face orpheus-current-face orpheus-tab-face
                  orpheus-tab-active-face orpheus-muted-face
                  orpheus-directory-face orpheus-error-face
                  orpheus-time-face orpheus-help-key-face
                  orpheus-vis-status-face))
    (should (facep face)))
  (should (custom-variable-p 'orpheus-theme))
  (should (custom-variable-p 'orpheus-ui-layout))
  (should (custom-variable-p 'orpheus-vis-theme)))

(ert-deftest orpheus-test/theme-presets-recolour-faces-and-the-spectrum ()
  "A preset sets the faces, the visualizer ramp and its background."
  (unwind-protect
      (progn
        (orpheus-apply-theme 'nord)
        (should (eq orpheus-theme 'nord))
        (should (equal (face-attribute 'orpheus-status-face :foreground)
                       "#88c0d0"))
        (should (equal orpheus-vis-colors
                       '("#2e3440" "#4c566a" "#5e81ac" "#88c0d0" "#a3be8c")))
        (should (equal orpheus-vis-background "#2e3440"))
        (orpheus-cycle-theme)
        (should (eq orpheus-theme 'gruvbox)))
    (orpheus-apply-theme 'aegean)))

(ert-deftest orpheus-test/theme-cycle-wraps-around ()
  "The last preset cycles back to the first."
  (unwind-protect
      (progn
        (setq orpheus-theme (caar (last orpheus-theme-presets)))
        (orpheus-cycle-theme)
        (should (eq orpheus-theme (caar orpheus-theme-presets))))
    (orpheus-apply-theme 'aegean)))

(ert-deftest orpheus-test/headers-are-consistent ()
  "The file header carries the version the README and NEWS document."
  (with-temp-buffer
    (insert-file-contents orpheus-test--source)
    (should (re-search-forward "^;;; Version: \\([0-9.]+\\)$" nil t))
    (should (equal (match-string 1) orpheus-version))
    (should (re-search-forward "^;;; Package-Requires: ((emacs \"[0-9.]+\"))$"
                               nil t))))

(ert-deftest orpheus-test/transport-commands-are-interactive ()
  "Every transport command can be bound to a key."
  (dolist (command '(orpheus orpheus-play-pause orpheus-stop orpheus-next
                     orpheus-previous orpheus-seek-forward
                     orpheus-seek-backward orpheus-volume-up
                     orpheus-volume-down orpheus-toggle-repeat
                     orpheus-toggle-random orpheus-toggle-single
                     orpheus-toggle-consume orpheus-visualizer))
    (should (commandp command))))


;;; Formatting helpers

(ert-deftest orpheus-test/format-time ()
  "Durations below an hour lose the hour, and a missing one is dashes."
  (should (equal (orpheus-format-time 0) "0:00"))
  (should (equal (orpheus-format-time 5) "0:05"))
  (should (equal (orpheus-format-time 65) "1:05"))
  (should (equal (orpheus-format-time 3599) "59:59"))
  (should (equal (orpheus-format-time 3725) "1:02:05"))
  (should (equal (orpheus-format-time nil) "-:--"))
  (should (equal (orpheus-format-time -3) "-:--")))

(ert-deftest orpheus-test/progress-percent ()
  "The percentage is rounded, clamped and zero-safe."
  (should (= (orpheus-progress-percent 0 100) 0))
  (should (= (orpheus-progress-percent 30 100) 30))
  (should (= (orpheus-progress-percent 200 100) 100))
  (should (= (orpheus-progress-percent 10 nil) 0))
  (should (= (orpheus-progress-percent 1 3) 33)))

(ert-deftest orpheus-test/progress-bar ()
  "The bar has the requested width, whatever the numbers are."
  (should (= (length (orpheus-progress-bar 30 60 10)) 10))
  (should (= (length (orpheus-progress-bar 0 0 10)) 10))
  (should (= (length (orpheus-progress-bar 999 10 7)) 7))
  (should (equal (orpheus-progress-bar 0 100 5)
                 (make-string 5 orpheus-progress-empty-char)))
  (should (equal (orpheus-progress-bar 100 100 5)
                 (make-string 5 orpheus-progress-filled-char))))

(ert-deftest orpheus-test/track-label ()
  "A track reads as artist and title, falling back to whichever exists."
  (should (equal (orpheus-track-label
                  (orpheus-track-create :title "Song" :artist "Band"))
                 "Band – Song"))
  (should (equal (orpheus-track-label
                  (orpheus-track-create :title "Song"))
                 "Song"))
  (should (equal (orpheus-track-label
                  (orpheus-track-create :uri "/music/a/b.flac"))
                 "b.flac"))
  (should (equal (orpheus-track-label nil) "")))

(ert-deftest orpheus-test/status-glyph ()
  "Play, pause and stop each get their own glyph, ASCII when asked."
  (let ((orpheus-ui-plain-ascii nil))
    (should (equal (orpheus-status-glyph
                    (orpheus-status-create :state 'play))
                   "▶"))
    (should (equal (orpheus-status-glyph
                    (orpheus-status-create :state 'pause))
                   "⏸"))
    (should (equal (orpheus-status-glyph
                    (orpheus-status-create :state 'stop))
                   "⏹")))
  (let ((orpheus-ui-plain-ascii t))
    (should (equal (orpheus-status-glyph
                    (orpheus-status-create :state 'play))
                   ">"))))

(ert-deftest orpheus-test/cache-file-stays-in-cache ()
  "A tracker-supplied name never escapes the cache directory."
  (let* ((orpheus-cache-directory (make-temp-file "orpheus-cache" t))
         (inside (file-name-as-directory
                  (file-truename orpheus-cache-directory))))
    (unwind-protect
        (dolist (key '("a/b/c.mp3" "../../../etc/passwd" ""))
          (let ((path (orpheus-cache-file key "cover.jpg")))
            (should (string-prefix-p inside (file-truename path)))))
      (delete-directory orpheus-cache-directory t))))


;;; The backend registry

(ert-deftest orpheus-test/backends-are-registered ()
  "MPD and local files are known backends, and both can be built."
  (should (orpheus-backend-exists-p 'mpd))
  (should (orpheus-backend-exists-p 'local))
  (should (orpheus-backend-p (orpheus-make-backend 'mpd)))
  (should (orpheus-backend-p (orpheus-make-backend 'local)))
  (should-error (orpheus-make-backend 'nonsense)))

(ert-deftest orpheus-test/backend-capabilities ()
  "MPD can browse everything; local files can walk and search the filesystem."
  (let ((mpd (orpheus-make-backend 'mpd))
        (local (orpheus-make-backend 'local)))
    (dolist (kind '(queue directories artists albumartists albums playlists
                    search))
      (should (memq kind (orpheus-backend-capabilities mpd))))
    (should (memq 'queue (orpheus-backend-capabilities local)))
    (should (memq 'directories (orpheus-backend-capabilities local)))
    (should (memq 'search (orpheus-backend-capabilities local)))))


;;; MPD, the pure parts

(ert-deftest orpheus-test/mpd-quote ()
  "Arguments with spaces and quotes are quoted the way MPD wants them."
  (should (equal (orpheus-mpd--quote "abc") "\"abc\""))
  (should (equal (orpheus-mpd--quote "a b") "\"a b\""))
  (should (equal (orpheus-mpd--quote "a\"b") "\"a\\\"b\""))
  (should (equal (orpheus-mpd--quote "a\\b") "\"a\\\\b\"")))

(ert-deftest orpheus-test/mpd-parse-lines ()
  "Lines split at the first colon and keep their order and duplicates."
  (should (equal (orpheus-mpd--parse-lines "file: /a/b.mp3\nTitle: Song\n")
                 '(("file" . "/a/b.mp3") ("Title" . "Song"))))
  (should (equal (orpheus-mpd--parse-lines "Title: a: b\n")
                 '(("Title" . "a: b"))))
  (should (equal (orpheus-mpd--parse-lines "nonsense without a colon\n")
                 '(("nonsense without a colon" . "")))))

(ert-deftest orpheus-test/mpd-line-value ()
  "The first value of a key wins, and missing keys are nil."
  (let ((lines '(("Title" . "one") ("Title" . "two"))))
    (should (equal (orpheus-mpd--line-value lines "Title") "one"))
    (should (equal (orpheus-mpd--line-value lines "Artist") nil))))

(ert-deftest orpheus-test/mpd-status-from-lines ()
  "An MPD status reply becomes a status record."
  (let ((status (orpheus-mpd--status-from-lines
                 '(("volume" . "70") ("repeat" . "1") ("random" . "0")
                   ("single" . "0") ("consume" . "1") ("state" . "play")
                   ("elapsed" . "12.5") ("duration" . "200.0")
                   ("songid" . "42") ("bitrate" . "320")))))
    (should (eq (orpheus-status-state status) 'play))
    (should (= (orpheus-status-volume status) 70))
    (should (= (orpheus-status-elapsed status) 12.5))
    (should (= (orpheus-status-total status) 200.0))
    (should (= (orpheus-status-songid status) 42))
    (should (orpheus-status-repeat status))
    (should-not (orpheus-status-random status))
    (should (orpheus-status-consume status))))

(ert-deftest orpheus-test/mpd-status-old-time-format ()
  "MPD used to report `time' as elapsed:total; both are read."
  (let ((status (orpheus-mpd--status-from-lines
                 '(("state" . "pause") ("time" . "31:245")))))
    (should (= (orpheus-status-elapsed status) 31))
    (should (= (orpheus-status-total status) 245))))

(ert-deftest orpheus-test/mpd-track-from-lines ()
  "A currentsong reply becomes a track record."
  (let ((track (orpheus-mpd--track-from-lines
                '(("file" . "a/b.mp3") ("Title" . "Song") ("Artist" . "Band")
                  ("Album" . "Record") ("AlbumArtist" . "Band")
                  ("duration" . "245.5") ("Track" . "3") ("Date" . "1994")
                  ("Genre" . "Pop")))))
    (should (equal (orpheus-track-uri track) "a/b.mp3"))
    (should (equal (orpheus-track-title track) "Song"))
    (should (equal (orpheus-track-artist track) "Band"))
    (should (equal (orpheus-track-album track) "Record"))
    (should (= (orpheus-track-duration track) 245.5))
    (should (equal (orpheus-track-year track) "1994")))
  (should-not (orpheus-mpd--track-from-lines nil)))


;;; MPD over the wire

(ert-deftest orpheus-test/mpd-status-over-tcp ()
  "The client connects, sends status, and hands back a status record."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "status" line)
            (concat "volume: 70\nstate: play\nrepeat: 1\nrandom: 0\n"
                    "single: 0\nconsume: 0\nelapsed: 12.5\n"
                    "duration: 200.0\nsongid: 42\nOK\n")
          "ACK [5@0] {} unknown command\n"))
    (let* ((mpd (orpheus-test--mpd-client))
           (result nil))
      (unwind-protect
          (progn
            (orpheus-mpd--status-request mpd
                                         (lambda (status)
                                           (setq result status)))
            (should (orpheus-test--wait (lambda () result)))
            (should (eq (orpheus-status-state result) 'play))
            (should (= (orpheus-status-elapsed result) 12.5))
            (should (= (orpheus-status-volume result) 70))
            (should (orpheus-status-repeat result)))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-track-over-tcp ()
  "The client asks for the current song and parses it."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "currentsong" line)
            (concat "file: a/b.flac\nTitle: Song\nArtist: Band\n"
                    "Album: Record\nduration: 12.0\nOK\n")
          "ACK [5@0] {} unknown command\n"))
    (let* ((mpd (orpheus-test--mpd-client))
           (result 'none))
      (unwind-protect
          (progn
            (orpheus-mpd--track-request mpd
                                        (lambda (track) (setq result track)))
            (should (orpheus-test--wait (lambda () (not (eq result 'none)))))
            (should (equal (orpheus-track-title result) "Song"))
            (should (equal (orpheus-track-artist result) "Band")))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-ack-becomes-an-error ()
  "An ACK is not an empty success: the message reaches the caller."
  (orpheus-test--with-fake-mpd
      (lambda (_line) "ACK [5@0] {} No such song\n")
    (let* ((mpd (orpheus-test--mpd-client))
           (reply nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command mpd "playid" '("999")
                                       (lambda (r) (setq reply r)))
            (should (orpheus-test--wait (lambda () reply)))
            (should-not (plist-get reply :ok))
            (should (string-match-p "No such song"
                                    (plist-get reply :error))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-silent-server-is-dropped ()
  "A daemon that never answers is dropped, and every waiting caller is told."
  (orpheus-test--with-fake-mpd
      (lambda (_line) nil)
    (let* ((orpheus-mpd-timeout 0.3)
           (mpd (orpheus-test--mpd-client))
           (replies nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command mpd "status" nil
                                       (lambda (r) (push r replies)))
            (orpheus-mpd--send-command mpd "currentsong" nil
                                       (lambda (r) (push r replies)))
            (should (orpheus-test--wait
                     (lambda () (= 2 (length replies))) 5))
            (should (cl-every (lambda (reply) (not (plist-get reply :ok)))
                              replies))
            (should (string-match-p "silent"
                                    (plist-get (car replies) :error)))
            (should-not (orpheus-backend-connected-p mpd)))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-watchdog-stops-when-the-reply-arrives ()
  "A reply cancels the watchdog, which must not fire later."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "status" line) "state: play\nOK\n" "OK\n"))
    (let ((orpheus-mpd-timeout 0.2)
          (mpd (orpheus-test--mpd-client))
          (replies nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command mpd "status" nil
                                       (lambda (r) (push r replies)))
            (should (orpheus-test--wait (lambda () replies) 2))
            (accept-process-output nil 0.4)
            (should (= 1 (length replies)))
            (should (orpheus-backend-connected-p mpd)))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-commands-sent-while-connecting-are-kept ()
  "Two commands issued before the greeting both get their reply."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (cond ((string-prefix-p "status" line)
               "state: play\nvolume: 50\nOK\n")
              (t "OK\n")))
    (let* ((mpd (orpheus-test--mpd-client))
           (first nil)
           (second nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command mpd "status" nil
                                       (lambda (r) (setq first r)))
            (orpheus-mpd--send-command mpd "ping" nil
                                       (lambda (r) (setq second r)))
            (should (orpheus-test--wait (lambda () (and first second))))
            (should (plist-get first :ok))
            (should (plist-get second :ok))
            (should (equal (nreverse (cl-copy-list orpheus-test--mpd-requests))
                           '("status" "ping"))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-password-is-sent-first ()
  "A configured password is sent before any queued command."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (cond ((string-prefix-p "password" line) "OK\n")
              ((string-prefix-p "status" line) "state: play\nOK\n")
              (t "OK\n")))
    (let* ((mpd (orpheus-test--mpd-client))
           (result nil))
      (setf (orpheus-mpd-instance-password mpd) "sekrit")
      (unwind-protect
          (progn
            (orpheus-mpd--status-request mpd
                                         (lambda (status)
                                           (setq result status)))
            (should (orpheus-test--wait (lambda () result)))
            ;; Requests are recorded newest first, so the oldest is the last.
            (should (equal (car (last orpheus-test--mpd-requests))
                           "password \"sekrit\"")))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-password-is-not-kept ()
  "The secret leaves the connection once MPD has accepted it."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (cond ((string-prefix-p "password" line) "OK\n")
              ((string-prefix-p "status" line) "state: play\nOK\n")
              (t "OK\n")))
    (let ((mpd (orpheus-test--mpd-client))
          (result nil))
      (setf (orpheus-mpd-instance-password mpd) "sekrit")
      (unwind-protect
          (progn
            (orpheus-mpd--status-request mpd
                                         (lambda (status)
                                           (setq result status)))
            (should (orpheus-test--wait (lambda () result)))
            (should-not (orpheus-mpd-connection-password
                         (orpheus-mpd-instance-connection mpd))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-unreachable-message-is-friendly ()
  "A dead daemon is reported with where it is and why, and nothing else."
  (let ((tcp (orpheus-mpd--unreachable-message
              (make-orpheus-mpd :host "music.example.org" :port 6601)
              (list 'file-error "make client process failed"
                    "Connection refused" :name "orpheus-mpd" :service 6601)))
        (unix (orpheus-mpd--unreachable-message
               (make-orpheus-mpd :socket "/run/user/1000/mpd/socket")
               (list 'file-error "make client process failed"
                     "No such file or directory" :family 'local))))
    (should (string-match-p "music\\.example\\.org:6601" tcp))
    (should (string-match-p "Connection refused" tcp))
    (should-not (string-match-p ":name" tcp))
    (should (string-match-p "/run/user/1000/mpd/socket" unix))
    (should (string-match-p "No such file or directory" unix))))

(ert-deftest orpheus-test/mpd-unreachable-message-hides-the-password ()
  "The failure is reported without the connection the raw error carries.
`make-network-process' puts the arguments it was given into its error, with
the filters and sentinels among them; those closures capture the connection,
and the password with it."
  (let ((secret "sekrit-4471")
        (messages nil))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (signal 'file-error
                         (append (list "make client process failed"
                                       "Connection refused")
                                 args))))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (should-not (orpheus-mpd--open
                   (make-orpheus-mpd :host "localhost" :port 6601
                                     :password secret)
                   #'ignore)))
    (should messages)
    (should-not (string-match-p secret (mapconcat #'identity messages "\n")))))

(ert-deftest orpheus-test/mpd-credential-comes-from-auth-source ()
  "A secret kept as a function is asked for it, as `pass' does."
  ;; Loading `auth-source' inside the call would put its own definition back.
  (require 'auth-source)
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil))
            ((symbol-function 'auth-source-search)
             (lambda (&rest spec)
               (should (equal (plist-get spec :host) "music.example.org"))
               (should (equal (plist-get spec :port) "6601"))
               (list (list :host "music.example.org"
                           :secret (lambda () "from-gpg"))))))
    (should (equal (orpheus-mpd--credential
                    (make-orpheus-mpd :host "music.example.org" :port 6601))
                   "from-gpg"))))

(ert-deftest orpheus-test/mpd-ack-is-reported-once ()
  "A refusal MPD spells out reaches the user, but only once per tick storm."
  (orpheus-test--with-fake-mpd
      (lambda (_line) "ACK [5@0] {} you don't have permission for \"status\"\n")
    (let ((mpd (orpheus-test--mpd-client))
          (orpheus-mpd--last-refusal nil)
          (messages nil))
      (unwind-protect
          (progn
            (orpheus-backend-connect mpd (lambda (_backend) nil))
            (should (orpheus-test--wait (lambda () (orpheus-backend-connected-p mpd))))
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              (dotimes (_ 3) (orpheus-mpd--status-request mpd #'ignore))
              ;; Three reads, one report.
              (let ((deadline (+ (float-time) 5)))
                (while (and (< (length messages) 1) (< (float-time) deadline))
                  (accept-process-output nil 0.02))
                (accept-process-output nil 0.3)))
            (should (= (length messages) 1))
            (should (string-match-p "you don't have permission"
                                    (car messages)))
            ;; The position MPD prefixes to an ACK means nothing to a reader.
            (should (string-match-p
                     "\\`orpheus: MPD refused the status request: you don't have permission"
                     (car messages))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/offline-message-blames-the-refusal ()
  "A daemon that answers and refuses is not reported as silent."
  (orpheus-test--with-fake-mpd (lambda (_line) "OK\n")
    (let* ((mpd (orpheus-test--mpd-client))
           (orpheus-backend-current mpd)
           (orpheus--failures 0)
           (orpheus--retry-at 0)
           (messages nil))
      (unwind-protect
          (progn
            (orpheus-backend-connect mpd (lambda (_backend) nil))
            (should (orpheus-test--wait (lambda () (orpheus-backend-connected-p mpd))))
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              (orpheus--offline))
            (should (= (length messages) 1))
            (should (string-match-p "refus" (car messages)))
            (should-not (string-match-p "not answering" (car messages))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/offline-message-still-reports-silence ()
  "A backend that is not there at all is still reported as not answering."
  (let ((orpheus-backend-current (make-orpheus-mpd :host "localhost" :port 6601))
        (orpheus--failures 0)
        (orpheus--retry-at 0)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (orpheus--offline))
    (should (= (length messages) 1))
    (should (string-match-p "not answering" (car messages)))))

(ert-deftest orpheus-test/mpd-album-art-binary ()
  "Binary album art survives the trip, byte for byte."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "albumart" line)
            (encode-coding-string
             (concat "size: 4\ntype: image/png\nbinary: 4\n"
                     (string 0 1 2 3) "\nOK\n")
             'binary)
          "ACK [5@0] {} No file exists\n"))
    (let* ((mpd (orpheus-test--mpd-client))
           (reply nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command
             mpd "albumart" (list (orpheus-mpd--quote "a/b.mp3") "0")
             (lambda (r) (setq reply r)))
            (should (orpheus-test--wait (lambda () reply)))
            (should (plist-get reply :ok))
            (let ((data (plist-get reply :binary)))
              (should (= (length data) 4))
              (should (= (aref data 0) 0))
              (should (= (aref data 3) 3))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-foreign-reply-keys ()
  "Values keep their case, so cover art names and tags are readable."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "playlistinfo" line)
            (concat "file: a.mp3\nTitle: One\nPos: 0\nId: 7\n"
                    "file: b.mp3\nTitle: Two\nPos: 1\nId: 8\nOK\n")
          "OK\n"))
    (let* ((mpd (orpheus-test--mpd-client))
           (reply nil))
      (unwind-protect
          (progn
            (orpheus-mpd--send-command mpd "playlistinfo" nil
                                       (lambda (r) (setq reply r)))
            (should (orpheus-test--wait (lambda () reply)))
            (should (equal (orpheus-mpd--line-value (plist-get reply :lines)
                                                    "Title")
                           "One")))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-unix-socket ()
  "MPD over a unix socket, which is how most local setups run."
  (let ((socket (make-temp-name "/tmp/orpheus-test-mpd-")))
    (unwind-protect
        (let* ((orpheus-test--mpd-accum "")
               (orpheus-test--mpd-handler
                (lambda (_line) "state: pause\nvolume: 30\nOK\n"))
               (server (make-network-process :name "orpheus-test-unix"
                                             :server t :family 'local
                                             :service socket :coding 'binary
                                             :noquery t
                                             :log #'orpheus-test--mpd-log
                                             :filter #'orpheus-test--mpd-filter))
               (mpd (make-orpheus-mpd :socket socket))
               (result nil))
          (unwind-protect
              (progn
                (should server)
                (orpheus-mpd--status-request mpd
                                             (lambda (status)
                                               (setq result status)))
                (should (orpheus-test--wait (lambda () result)))
                (should (eq (orpheus-status-state result) 'pause)))
            (orpheus-backend-disconnect mpd)
            (ignore-errors (delete-process server))))
      (ignore-errors (delete-file socket)))))


;;; Local files through mpv

(ert-deftest orpheus-test/local-advance ()
  "The queue advances, wraps when asked, and stops at the end."
  (should (= (orpheus-local--advance 3 1 nil nil) 2))
  (should (= (orpheus-local--advance 3 0 nil nil) 1))
  (should-not (orpheus-local--advance 3 2 nil nil))
  (should (= (orpheus-local--advance 3 2 t nil) 0))
  (should-not (orpheus-local--advance 1 0 nil nil))
  (should-not (orpheus-local--advance 0 0 nil nil))
  (let ((next (orpheus-local--advance 4 2 nil t)))
    (should (integerp next))
    (should (<= 0 next))
    (should (< next 4))))

(ert-deftest orpheus-test/local-audio-args ()
  "The audio output follows the platform, and a user choice wins."
  (let ((orpheus-local-audio-driver nil))
    (let ((system-type 'gnu/linux))
      (should (equal (orpheus-local--audio-args) '("--ao=pipewire,pulse,alsa"))))
    (let ((system-type 'darwin))
      (should (equal (orpheus-local--audio-args) '("--ao=coreaudio"))))
    (let ((system-type 'windows-nt))
      (should (equal (orpheus-local--audio-args) '("--ao=wasapi"))))
    (let ((system-type 'berkeley-unix))
      (should (equal (orpheus-local--audio-args) '("--ao=pulse,alsa"))))
    (let ((system-type 'darwin)
          (orpheus-local-audio-driver "coreaudio/1"))
      (should (equal (orpheus-local--audio-args) '("--ao=coreaudio/1"))))))

(ert-deftest orpheus-test/local-socket-path-is-unique-on-windows ()
  "Each Emacs gets its own named pipe, and a chosen pipe is left alone."
  (let* ((orpheus-local-socket nil)
         (system-type 'windows-nt)
         (path (orpheus-local--socket-path)))
    (should (string-prefix-p "\\\\.\\pipe\\orpheus-emacs-" path))
    (should (string-suffix-p (number-to-string (emacs-pid)) path)))
  (let ((orpheus-local-socket "\\\\.\\pipe\\chosen")
        (system-type 'windows-nt))
    (should (equal (orpheus-local--socket-path)
                   "\\\\.\\pipe\\chosen"))))

(ert-deftest orpheus-test/local-request-json ()
  "A request is one JSON object per line, as mpv's IPC wants it."
  (let* ((json (orpheus-local--request-json 7 '("loadfile" "/a/b.mp3")))
         (parsed (json-parse-string json :object-type 'alist)))
    (should (string-suffix-p "\n" json))
    (should (equal (alist-get 'request_id parsed) 7))
    (should (equal (append (alist-get 'command parsed) nil)
                   '("loadfile" "/a/b.mp3")))))

(ert-deftest orpheus-test/local-set-field-maps-pause ()
  "mpv's pause property becomes the state the interface shows."
  (let ((local (make-orpheus-local :name 'local)))
    (orpheus-local--set-field local 'pause nil t)
    (should (eq (orpheus-status-state (orpheus-local-instance-status local))
                'pause))
    (orpheus-local--set-field local 'pause nil nil)
    (should (eq (orpheus-status-state (orpheus-local-instance-status local))
                'play))
    (orpheus-local--set-field local 'elapsed nil 12.5)
    (should (= 12.5 (orpheus-status-elapsed
                     (orpheus-local-instance-status local))))
    (orpheus-local--set-field local 'volume nil 42)
    (should (= 42 (orpheus-status-volume
                   (orpheus-local-instance-status local))))
    ;; A property mpv refuses must not overwrite what is known.
    (orpheus-local--set-field local 'volume "property unavailable" nil)
    (should (= 42 (orpheus-status-volume
                   (orpheus-local-instance-status local))))))

(ert-deftest orpheus-test/local-metadata-fills-track ()
  "Tags from mpv land on the track and leave what is missing alone."
  (let ((local (make-orpheus-local :name 'local)))
    (setf (orpheus-local-instance-track local)
          (orpheus-track-create :uri "/a/b.mp3" :title "b.mp3"))
    (orpheus-local--set-metadata
     local nil '((title . "Song") (artist . "Band")
                 (album . "Record") (date . "1999")
                 (track . "4")))
    (let ((track (orpheus-local-instance-track local)))
      (should (equal "Song" (orpheus-track-title track)))
      (should (equal "Band" (orpheus-track-artist track)))
      (should (equal "Record" (orpheus-track-album track)))
      (should (equal "1999" (orpheus-track-year track)))
      (should (= 4 (orpheus-track-trackno track))))
    (orpheus-local--set-metadata local nil nil)
    (should (equal "Song" (orpheus-track-title
                           (orpheus-local-instance-track local))))))


;;; The equalizer

(ert-deftest orpheus-test/track-range-roundtrip ()
  "A track of a disc names its file and the seconds it plays."
  (let ((uri (orpheus-track-range-uri "/m/disc.flac" 12.5 214.25)))
    (should (equal (orpheus-track-path uri) "/m/disc.flac"))
    (should (equal (orpheus-track-range uri) '(12.5 . 214.25)))
    (should (equal (orpheus-track-range-uri "/m/disc.flac" 30.0)
                   "/m/disc.flac#start=30.000"))
    (should (equal (orpheus-track-range "/m/disc.flac#start=30.000")
                   '(30.0))))
  ;; A whole file has no range, and is still its own path.
  (should-not (orpheus-track-range "https://example.org/a.mp3"))
  (should (equal (orpheus-track-path "https://example.org/a.mp3")
                 "https://example.org/a.mp3")))

(ert-deftest orpheus-test/cue-sheet-names-tracks ()
  "A cue sheet's INDEX lines become tracks with their titles and ends."
  (let* ((text (concat "PERFORMER \"The Band\"\n"
                       "TITLE \"Album\"\n"
                       "FILE \"disc.flac\" WAVE\n"
                       "  TRACK 01 AUDIO\n"
                       "    TITLE \"First\"\n"
                       "    INDEX 01 00:00:00\n"
                       "  TRACK 02 AUDIO\n"
                       "    INDEX 00 04:30:00\n"
                       "    INDEX 01 04:33:12\n"
                       "  TRACK 03 AUDIO\n"
                       "    TITLE \"Third\"\n"
                       "    PERFORMER \"Guest\"\n"
                       "    INDEX 01 10:00:00\n"))
         (tracks (orpheus-local--cue-tracks text)))
    (should (= 3 (length tracks)))
    (should (equal (plist-get (nth 0 tracks) :title) "First"))
    (should (equal (plist-get (nth 0 tracks) :artist) "The Band"))
    (should (equal (plist-get (nth 1 tracks) :title) nil))
    (should (equal (plist-get (nth 2 tracks) :artist) "Guest"))
    (should (= (plist-get (nth 0 tracks) :start) 0))
    (should (< (abs (- (plist-get (nth 1 tracks) :start) 273.16)) 0.01))
    (should (= (plist-get (nth 0 tracks) :end) (plist-get (nth 1 tracks) :start)))
    (should-not (plist-get (nth 2 tracks) :end))))

(ert-deftest orpheus-test/local-cd-folder-lists-tracks ()
  "A folder holding a disc and its cue sheet lists the disc's tracks."
  (let* ((dir (make-temp-file "orpheus-cd" t))
         (audio (expand-file-name "disc.flac" dir))
         (cue (expand-file-name "Disc.CUE" dir)))
    (unwind-protect
        (progn
          (with-temp-file audio (insert "not real audio"))
          (with-temp-file cue
            (insert "TRACK 01 AUDIO\n  TITLE \"First\"\n  INDEX 01 00:00:00\n"
                    "TRACK 02 AUDIO\n  TITLE \"Second\"\n"
                    "  INDEX 01 00:10:00\n"))
          (let* ((items (orpheus-local--directory-items dir))
                 (data (orpheus-item-data (nth 0 items))))
            (should (= 2 (length items)))
            (should (equal (orpheus-item-label (nth 0 items)) "01  First"))
            (should (equal (alist-get 'uri data)
                           (concat audio "#start=0.000&end=10.000")))
            (should (= (orpheus-track-duration (alist-get 'track data)) 10))
            (should (equal (orpheus-item-label (nth 1 items)) "02  Second"))
            (should (equal (orpheus-item-detail (nth 0 items)) "0:10"))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/flac-cuesheet-names-tracks ()
  "The CUESHEET block inside a FLAC file becomes its tracks."
  (let* ((dir (make-temp-file "orpheus-disc" t))
         (file (expand-file-name "disc.flac" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-multibyte nil)
            (let ((coding-system-for-write 'binary))
              (insert (orpheus-test--flac-image
                       44100 1323000
                       '((0 . 1) (441000 . 2) (882000 . 3)
                         (1323000 . 170))))))
          (let ((tracks (orpheus-local--cd-tracks file)))
            (should (= 3 (length tracks)))
            (should (= (plist-get (nth 1 tracks) :start) 10))
            (should (= (plist-get (nth 1 tracks) :end) 20))
            (should (= (plist-get (nth 2 tracks) :number) 3))
            (should (= (plist-get (nth 2 tracks) :end) 30))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-status-measures-a-track-range ()
  "A track of a disc reports its own position and length, not the disc's."
  (let ((local (make-orpheus-local :name 'local)))
    (setf (orpheus-local-instance-range local) '(600.0 . 900.0))
    (orpheus-local--set-field local 'elapsed nil 640.0)
    (orpheus-local--set-field local 'total nil 3600.0)
    (let ((status (orpheus-local-instance-status local)))
      (should (= 40.0 (orpheus-status-elapsed status)))
      (should (= 300.0 (orpheus-status-total status))))
    ;; The last track of a disc runs to the end of the image.
    (setf (orpheus-local-instance-range local) '(3000.0))
    (orpheus-local--set-field local 'total nil 3600.0)
    (should (= 600.0 (orpheus-status-total
                      (orpheus-local-instance-status local))))))

(ert-deftest orpheus-test/local-loadfile-carries-the-track-range ()
  "mpv is told where the part a track plays starts and ends."
  (should (equal (orpheus-local--range-options '(12.5 . 214.25))
                 "start=12.500,end=214.250"))
  (should (equal (orpheus-local--range-options '(30.0)) "start=30.000")))

(ert-deftest orpheus-test/local-cue-title-survives-the-image ()
  "A track of a disc keeps the cue title when mpv reports the album's tags."
  (let ((local (make-orpheus-local :name 'local)))
    (setf (orpheus-local-instance-track local)
          (orpheus-track-create :uri "/m/disc.flac#start=10.000&end=20.000"
                                :title "Song" :artist "Band"))
    (orpheus-local--set-metadata
     local nil '((title . "Album") (artist . "Various")
                 (album . "Record")))
    (let ((track (orpheus-local-instance-track local)))
      (should (equal (orpheus-track-title track) "Song"))
      (should (equal (orpheus-track-artist track) "Band"))
      (should (equal (orpheus-track-album track) "Record")))
    (should (orpheus-local-instance-tags-loaded local))))

(ert-deftest orpheus-test/local-queue-names-a-cue-track ()
  "A queued track of a disc is named by its cue sheet, not the image."
  (let* ((local (make-orpheus-local :name 'local))
         (uri "/m/disc.flac#start=10.000&end=20.000"))
    (orpheus-local--remember-track
     (orpheus-track-create :uri uri :title "Song" :artist "Band"))
    (setf (orpheus-local-instance-queue local) (list uri))
    (let ((item (car (orpheus-local--queue-items local))))
      (should (equal (orpheus-local--uri-name uri) "Song"))
      (should (string-match-p "Song" (orpheus-item-label item))))))

(ert-deftest orpheus-test/local-search-finds-a-cue-track-title ()
  "A cue sheet's song title finds the track, not only the image path."
  (let* ((dir (make-temp-file "orpheus-cue-search" t))
         (image (expand-file-name "disc.flac" dir))
         (cue (expand-file-name "disc.cue" dir)))
    (unwind-protect
        (progn
          (write-region "" nil image nil 'silent)
          (write-region
           (concat "PERFORMER \"Band\"\nTITLE \"Record\"\n"
                   "TRACK 01 AUDIO\n  TITLE \"First\"\n  INDEX 01 00:00:00\n"
                   "TRACK 02 AUDIO\n  TITLE \"Second\"\n  INDEX 01 00:10:00\n")
           nil cue nil 'silent)
          (let ((items (orpheus-local--search-items
                        dir "Second" (concat image "\n" cue "\n"))))
            (should (equal (mapcar #'orpheus-item-label items)
                           '("02  Second")))
            (should (equal (alist-get 'uri (orpheus-item-data (car items)))
                           (concat image "#start=10.000")))
            (should (equal (orpheus-track-album
                            (alist-get 'track (orpheus-item-data (car items))))
                           "Record")))
          ;; The image name still finds the whole disc.
          (should (= 2 (length (orpheus-local--search-items
                                dir "disc.flac"
                                (concat image "\n" cue "\n"))))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-search-indexes-embedded-flac-cues ()
  "A FLAC CUESHEET comment title is searchable after the background index."
  (let* ((dir (make-temp-file "orpheus-embedded-search" t))
         (file (expand-file-name "disc.flac" dir))
         (cache (expand-file-name "cues.json" dir))
         (orpheus-local-directory dir)
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded nil)
         (orpheus-local--cue-index-queue nil)
         (orpheus-local--cue-index-waiters nil)
         (orpheus-local--cue-index-running nil)
         (results nil))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-cue file)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) nil)))
            (orpheus-local--search
             "Second" (lambda (rows) (push rows results))))
          ;; The first answer is immediate; the index then adds the track.
          (should (= 2 (length results)))
          (should-not (nth 1 results))
          (should (equal (mapcar #'orpheus-item-label (car results))
                         '("02  Second")))
          (should (file-readable-p cache))
          ;; A second search reuses the cache instead of reading the FLAC.
          (let ((calls 0))
            (cl-letf (((symbol-function 'orpheus-local--flac-tracks)
                       (lambda (_file)
                         (setq calls (1+ calls)) nil))
                      ((symbol-function 'executable-find)
                       (lambda (_program) nil)))
              (setq results nil)
              (orpheus-local--search
               "Second" (lambda (rows) (push rows results))))
            (should (= 0 calls))
            (should (equal (mapcar #'orpheus-item-label (car results))
                           '("02  Second"))))
          ;; The cache survives a new session, so the first answer has it.
          (let ((orpheus-local--cue-cache (make-hash-table :test #'equal))
                (orpheus-local--cue-cache-loaded nil)
                (orpheus-local--cue-index-queue nil)
                (orpheus-local--cue-index-waiters nil)
                (orpheus-local--cue-index-running nil)
                (calls 0))
            (cl-letf (((symbol-function 'orpheus-local--flac-tracks)
                       (lambda (_file)
                         (setq calls (1+ calls)) nil))
                      ((symbol-function 'executable-find)
                       (lambda (_program) nil)))
              (setq results nil)
              (orpheus-local--search
               "Second" (lambda (rows) (push rows results))))
            (should (= 0 calls))
            (should (= 1 (length results)))
            (should (equal (mapcar #'orpheus-item-label (car results))
                           '("02  Second")))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-invalidates-a-changed-flac ()
  "A FLAC with a new modification time is indexed again."
  (let* ((dir (make-temp-file "orpheus-cue-cache" t))
         (file (expand-file-name "disc.flac" dir))
         (orpheus-local--cue-cache-file (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-image file)
          (orpheus-local--cue-cache-store file nil)
          (should-not (orpheus-local--cue-cache-stale-p file))
          (set-file-times file (time-add (current-time) 60))
          (should (orpheus-local--cue-cache-stale-p file)))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cd-tracks-reuses-the-cue-cache ()
  "Browsing a FLAC reuses cached tracks until its size or time changes."
  (let* ((dir (make-temp-file "orpheus-cd-cache" t))
         (file (expand-file-name "disc.flac" dir))
         (orpheus-local--cue-cache-file (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded nil)
         (calls 0)
         (tracks (list (list :number 1 :title "First" :start 0.0 :end 10.0))))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-cue file)
          (cl-letf (((symbol-function 'orpheus-local--flac-tracks)
                     (lambda (_file)
                       (setq calls (1+ calls))
                       tracks)))
            (should (equal tracks (orpheus-local--cd-tracks file)))
            (should (equal tracks (orpheus-local--cd-tracks file)))
            (should (= 1 calls))
            (set-file-times file (time-add (current-time) 60))
            (should (equal tracks (orpheus-local--cd-tracks file)))
            (should (= 2 calls))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-index-reports-progress ()
  "The indexer reports how many FLACs are left, then clears the report."
  (let* ((dir (make-temp-file "orpheus-cue-progress" t))
         (files (cl-loop for index below 17
                         collect (let ((file (expand-file-name
                                               (format "disc%02d.flac" index)
                                               dir)))
                                   (with-temp-file file)
                                   file)))
         (orpheus-local--cue-cache-file (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded t)
         (orpheus-local--cue-index-queue nil)
         (orpheus-local--cue-index-waiters nil)
         (orpheus-local--cue-index-running nil)
         (orpheus-local--cue-index-total 0)
         (orpheus-local--cue-index-done 0)
         (refreshed 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _arguments) nil)))
          (should (orpheus-local--embedded-cue-index-start
                   files
                   (lambda () (setq refreshed (1+ refreshed)))))
          (should (equal "indexing cues 16/17"
                         (orpheus-local--cue-index-progress)))
          (should (equal "indexing cues 16/17"
                         (orpheus-backend-progress
                          (make-orpheus-local :name 'local))))
          (orpheus-local--cue-index-step)
          (should (= 1 refreshed))
          (should-not (orpheus-local--cue-index-progress)))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-ignores-an-unknown-version ()
  "A cache written by another version is ignored, not half-read."
  (let* ((dir (make-temp-file "orpheus-cue-version" t))
         (cache (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-temp-file cache
            (insert "{\"version\":99,\"files\":{\"/music/disc.flac\":[1,2,null]}}"))
          (orpheus-local--cue-cache-read cache)
          (should (= 0 (hash-table-count orpheus-local--cue-cache))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-saves-atomically ()
  "Saving writes one versioned file and leaves no temporary file."
  (let* ((dir (make-temp-file "orpheus-cue-save" t))
         (file (expand-file-name "disc.flac" dir))
         (cache (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-dirty nil))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-cue file)
          (orpheus-local--cue-cache-store
           file (list (list :number 1 :title "First" :start 0.0 :end 10.0)))
          (should (orpheus-local--cue-cache-save))
          (should (file-readable-p cache))
          (should-not
           (cl-find-if (lambda (name)
                         (string-prefix-p "local-cues.json.tmp-" name))
                       (directory-files dir)))
          (with-temp-buffer
            (insert-file-contents cache)
            (should (string-match-p "\"version\":1" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-prunes-changed-and-missing-files ()
  "Pruning forgets a cached file that is gone or no longer matches."
  (let* ((dir (make-temp-file "orpheus-cue-prune" t))
         (file (expand-file-name "disc.flac" dir))
         (missing (expand-file-name "gone.flac" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-dirty nil))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-image file)
          (orpheus-local--cue-cache-store file nil)
          (puthash missing (list 1 2.0 nil) orpheus-local--cue-cache)
          (should (= 1 (orpheus-local--cue-cache-prune)))
          (should (gethash file orpheus-local--cue-cache))
          (should-not (gethash missing orpheus-local--cue-cache))
          (should orpheus-local--cue-cache-dirty)
          (set-file-times file (time-add (current-time) 60))
          (should (= 1 (orpheus-local--cue-cache-prune)))
          (should (= 0 (hash-table-count orpheus-local--cue-cache))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-tracks-drops-a-changed-entry ()
  "A changed FLAC's old titles are not served before it is indexed again."
  (let* ((dir (make-temp-file "orpheus-cue-tracks" t))
         (file (expand-file-name "disc.flac" dir))
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-dirty nil)
         (tracks (list (list :number 1 :title "Old" :start 0.0 :end 1.0))))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-image file)
          (orpheus-local--cue-cache-store file tracks)
          (should (equal tracks (orpheus-local--cue-cache-tracks file)))
          (set-file-times file (time-add (current-time) 60))
          (should-not (orpheus-local--cue-cache-tracks file))
          (should-not (gethash file orpheus-local--cue-cache)))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-cue-cache-load-prunes-missing-files ()
  "Reading a cache from disk drops entries whose files are gone."
  (let* ((dir (make-temp-file "orpheus-cue-load" t))
         (cache (expand-file-name "cues.json" dir))
         (missing (expand-file-name "gone.flac" dir))
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded nil)
         (orpheus-local--cue-cache-dirty nil))
    (unwind-protect
        (progn
          (puthash missing (list 1 2.0 nil) orpheus-local--cue-cache)
          (setq orpheus-local--cue-cache-dirty t)
          (should (orpheus-local--cue-cache-save))
          (setq orpheus-local--cue-cache (make-hash-table :test #'equal)
                orpheus-local--cue-cache-loaded nil)
          (orpheus-local--cue-cache-load)
          (should (= 0 (hash-table-count orpheus-local--cue-cache)))
          (should orpheus-local--cue-cache-dirty))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-update-prunes-the-cue-cache ()
  "Updating a local library removes entries that no longer match files."
  (let* ((dir (make-temp-file "orpheus-cue-update" t))
         (file (expand-file-name "disc.flac" dir))
         (missing (expand-file-name "gone.flac" dir))
         (cache (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded t)
         (orpheus-local--cue-cache-dirty nil))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-image file)
          (orpheus-local--cue-cache-store file nil)
          (puthash missing (list 1 2.0 nil) orpheus-local--cue-cache)
          (orpheus-backend-command (make-orpheus-local :name 'local)
                                   'update nil)
          (should (gethash file orpheus-local--cue-cache))
          (should-not (gethash missing orpheus-local--cue-cache))
          (should-not orpheus-local--cue-cache-dirty))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-rescan-clears-the-cue-cache ()
  "Rescanning a local library forgets every embedded cue entry."
  (let* ((dir (make-temp-file "orpheus-cue-rescan" t))
         (file (expand-file-name "disc.flac" dir))
         (cache (expand-file-name "cues.json" dir))
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded t)
         (orpheus-local--cue-cache-dirty nil))
    (unwind-protect
        (progn
          (orpheus-test--write-flac-image file)
          (orpheus-local--cue-cache-store file nil)
          (orpheus-backend-command (make-orpheus-local :name 'local)
                                   'rescan nil)
          (should (= 0 (hash-table-count orpheus-local--cue-cache)))
          (should (file-readable-p cache)))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-watch-starts-and-stops-with-the-backend ()
  "Connecting watches the library tree; disconnecting removes every watch."
  (let* ((dir (make-temp-file "orpheus-watch" t))
         (album (expand-file-name "album/" dir))
         (local (make-orpheus-local :name 'local))
         (orpheus-local-directory dir)
         (added 0)
         (removed 0))
    (make-directory album)
    (unwind-protect
        (cl-letf (((symbol-function 'file-notify-add-watch)
                   (lambda (_file _flags _callback)
                     (setq added (1+ added))
                     (list 'watch added)))
                  ((symbol-function 'file-notify-rm-watch)
                   (lambda (_watch) (setq removed (1+ removed))))
                  ((symbol-function 'orpheus-local--ensure)
                   (lambda (_local callback) (funcall callback nil)))
                  ((symbol-function 'orpheus-local--kill) #'ignore))
          (orpheus-backend-connect local #'ignore)
          (should (= 2 added))
          (should (= 2 (length (orpheus-local-instance-watches local))))
          (orpheus-backend-disconnect local)
          (should (= 2 removed))
          (should-not (orpheus-local-instance-watches local)))
      (ignore-errors (orpheus-local--watch-stop local))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-watch-invalidates-cue-and-player-caches ()
  "A filesystem change prunes the cue cache and runs the library hook."
  (let* ((dir (make-temp-file "orpheus-watch-cache" t))
         (missing (expand-file-name "gone.flac" dir))
         (cache (expand-file-name "cues.json" dir))
         (local (make-orpheus-local :name 'local))
         (orpheus-local-directory dir)
         (orpheus-local--cue-cache-file cache)
         (orpheus-local--cue-cache (make-hash-table :test #'equal))
         (orpheus-local--cue-cache-loaded t)
         (orpheus-local--cue-cache-dirty nil)
         (callbacks nil)
         (changes 0)
         (orpheus-backend-library-changed-hook
          (list (lambda () (setq changes (1+ changes))))))
    (unwind-protect
        (cl-letf (((symbol-function 'file-notify-add-watch)
                   (lambda (_file _flags callback)
                     (push callback callbacks)
                     (list 'watch (length callbacks))))
                  ((symbol-function 'file-notify-rm-watch) #'ignore))
          (orpheus-local--watch-start local)
          (puthash missing (list 1 2.0 nil) orpheus-local--cue-cache)
          (funcall (car callbacks) (list 'watch 'changed missing))
          (should (timerp (orpheus-local-instance-watch-timer local)))
          (cancel-timer (orpheus-local-instance-watch-timer local))
          (setf (orpheus-local-instance-watch-timer local) nil)
          (orpheus-local--watch-apply local)
          (should-not (gethash missing orpheus-local--cue-cache))
          (should (= 1 changes)))
      (ignore-errors (orpheus-local--watch-stop local))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-watch-tracks-created-and-deleted-directories ()
  "New directories get their own watch, and deleted ones lose it."
  (let* ((dir (make-temp-file "orpheus-watch-tree" t))
         (new (expand-file-name "new/" dir))
         (local (make-orpheus-local :name 'local))
         (orpheus-local-directory dir)
         (callbacks nil))
    (unwind-protect
        (cl-letf (((symbol-function 'file-notify-add-watch)
                   (lambda (file _flags callback)
                     (push (cons (file-name-as-directory file) callback)
                           callbacks)
                     (list 'watch (length callbacks))))
                  ((symbol-function 'file-notify-rm-watch) #'ignore))
          (orpheus-local--watch-start local)
          (should (= 1 (length (orpheus-local-instance-watches local))))
          (make-directory new)
          (funcall (cdr (assoc (file-name-as-directory dir) callbacks))
                   (list 'watch 'created (directory-file-name new)))
          (should (= 2 (length (orpheus-local-instance-watches local))))
          (should (assoc (file-name-as-directory new)
                         (orpheus-local-instance-watches local)))
          (delete-directory new)
          (funcall (cdr (assoc (file-name-as-directory dir) callbacks))
                   (list 'watch 'deleted (directory-file-name new)))
          (should (= 1 (length (orpheus-local-instance-watches local)))))
      (ignore-errors (orpheus-local--watch-stop local))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-search-walks-the-tree-without-find ()
  "A host without GNU find is searched by walking the tree in Emacs."
  (let* ((dir (make-temp-file "orpheus-search" t))
         (file (expand-file-name "Song.mp3" dir))
         (orpheus-local-directory dir)
         (items 'waiting))
    (unwind-protect
        (progn
          (write-region "" nil file nil 'silent)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) nil)))
            (orpheus-local--search "Song" (lambda (rows) (setq items rows))))
          (should (equal (mapcar #'orpheus-item-label items) '("Song.mp3"))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-extensions-cover-audio-and-video ()
  "The files pane accepts the common audio and video containers."
  (dolist (extension '("flac" "m4a" "ape" "wv" "dsf"
                       "mp4" "mkv" "webm" "avi" "m2ts"))
    (should (member extension orpheus-local-extensions))))

(ert-deftest orpheus-test/local-poll-reads-tags-once ()
  "A tick asks for the position and the modes, not the tags again."
  (let ((local (make-orpheus-local :name 'local))
        (commands nil))
    (setf (orpheus-local-instance-tags-loaded local) t
          (orpheus-local-instance-duration-loaded local) t
          (orpheus-status-state (orpheus-local-instance-status local)) 'play)
    (cl-letf (((symbol-function 'orpheus-local--send)
               (lambda (_local command &optional _callback)
                 (push (cadr command) commands))))
      (orpheus-local--poll local))
    (should (member "time-pos" commands))
    (should (member "pause" commands))
    (should (member "volume" commands))
    (should-not (member "metadata" commands))
    (should-not (member "duration" commands))))

(ert-deftest orpheus-test/equalizer-filter ()
  "The filter string names each non-zero band and nothing else."
  (let ((orpheus-equalizer-bands '(100 1000)))
    (should (equal (orpheus-equalizer--filter '(0 3))
                   "equalizer=frequency=1000:width_type=o:width=1:gain=3"))
    (should (equal (orpheus-equalizer--filter '(0 0)) ""))))

(ert-deftest orpheus-test/equalizer-clamps-gains ()
  "Gains are rounded into the range, and a wrong shape is refused."
  (let ((orpheus-equalizer-bands '(100 1000))
        (orpheus-equalizer-max-gain 6))
    (should (equal (orpheus-equalizer--clean '(99 -99)) '(6 -6)))
    (should (equal (orpheus-equalizer--clean '(1.6 -1.6)) '(2 -2)))
    (should-error (orpheus-equalizer--clean '(1)) :type 'user-error)
    (should-error (orpheus-equalizer--clean '(1 "x")) :type 'user-error)))

(ert-deftest orpheus-test/equalizer-presets-roundtrip ()
  "User presets survive JSON, and a user preset wins over a built-in."
  (let* ((file (make-temp-file "orpheus-eq" nil ".json"))
         (orpheus-equalizer-preset-file file))
    (unwind-protect
        (progn
          (delete-file file)
          (orpheus-equalizer--write-presets
           '(("mine" . (1 2 3 4 5 6 7 8 9 10))
             ("flat" . (1 1 1 1 1 1 1 1 1 1))))
          (should (equal (cdr (assoc "mine"
                                     (orpheus-equalizer--read-presets)))
                         '(1 2 3 4 5 6 7 8 9 10)))
          (should (equal (cdr (assoc "flat" (orpheus-equalizer-presets)))
                         '(1 1 1 1 1 1 1 1 1 1))))
      (ignore-errors (delete-file file)))))

(ert-deftest orpheus-test/equalizer-rejects-broken-preset-file ()
  "A preset file without one gain per band is an error, not silence."
  (let* ((file (make-temp-file "orpheus-eq" nil ".json"))
         (orpheus-equalizer-preset-file file))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"broken\":[1,2]}"))
          (should-error (orpheus-equalizer--read-presets) :type 'user-error))
      (ignore-errors (delete-file file)))))

(ert-deftest orpheus-test/equalizer-sends-the-filter ()
  "Applying the equalizer sends one af property to mpv."
  (let ((orpheus-backend-current (make-orpheus-local :name 'local))
        (orpheus-equalizer-enabled t)
        (orpheus-equalizer--gains '(0 3))
        (orpheus-equalizer-bands '(100 1000))
        (sent nil))
    (cl-letf (((symbol-function 'orpheus-local--ready-p)
               (lambda (_local) t))
              ((symbol-function 'orpheus-local--send)
               (lambda (_local command &optional _callback)
                 (setq sent command))))
      (should (orpheus-equalizer--apply))
      (should (equal sent
                     '("set_property" "af"
                       "equalizer=frequency=1000:width_type=o:width=1:gain=3"))))))

(ert-deftest orpheus-test/equalizer-refuses-a-backend-without-mpv ()
  "MPD's audio leaves through the daemon, so Emacs says it cannot filter it."
  (let ((orpheus-backend-current (make-orpheus-mpd :name 'mpd))
        (orpheus-equalizer-enabled t)
        (orpheus-equalizer--gains '(0 0 0 0 0 0 0 0 0 0))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (should-not (orpheus-equalizer--apply))
      (should (string-match-p "cannot apply an equalizer" (car messages))))))

(ert-deftest orpheus-test/equalizer-load-preset-applies ()
  "Loading a preset sets the gains, its name and the enabled flag."
  (let* ((file (make-temp-file "orpheus-eq" nil ".json"))
         (state (make-temp-file "orpheus-eq-state" nil ".json"))
         (orpheus-equalizer-preset-file file)
         (orpheus-equalizer-state-file state)
         (orpheus-equalizer-enabled nil)
         (orpheus-equalizer--gains nil)
         (applied nil))
    (unwind-protect
        (progn
          (delete-file file)
          (delete-file state)
          (orpheus-equalizer--write-presets
           '(("mine" . (1 2 3 4 5 6 7 8 9 10))))
          (cl-letf (((symbol-function 'orpheus-equalizer--apply)
                     (lambda () (setq applied t))))
            (orpheus-equalizer-load-preset "mine"))
          (should applied)
          (should (equal orpheus-equalizer--gains
                         '(1 2 3 4 5 6 7 8 9 10)))
          (should (equal orpheus-equalizer-preset "mine"))
          (should orpheus-equalizer-enabled)
          ;; The same curve is what the next session restores.
          (should (file-exists-p state)))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file state)))))

(ert-deftest orpheus-test/equalizer-save-preset ()
  "Saving writes the current gains under the given name."
  (let* ((file (make-temp-file "orpheus-eq" nil ".json"))
         (orpheus-equalizer-preset-file file)
         (orpheus-equalizer--gains '(2 2 2 2 2 2 2 2 2 2)))
    (unwind-protect
        (progn
          (delete-file file)
          (orpheus-equalizer-save-preset "mine")
          (should (equal (cdr (assoc "mine"
                                     (orpheus-equalizer--read-presets)))
                         '(2 2 2 2 2 2 2 2 2 2)))
          (should-error (orpheus-equalizer-save-preset "  ")
                        :type 'user-error))
      (ignore-errors (delete-file file)))))

(ert-deftest orpheus-test/equalizer-delete-preset ()
  "A saved preset can be deleted; a built-in one cannot."
  (let* ((file (make-temp-file "orpheus-eq" nil ".json"))
         (orpheus-equalizer-preset-file file))
    (unwind-protect
        (progn
          (delete-file file)
          (orpheus-equalizer--write-presets
           '(("mine" . (1 2 3 4 5 6 7 8 9 10))))
          (orpheus-equalizer-delete-preset "mine")
          (should-not (assoc "mine" (orpheus-equalizer--read-presets)))
          (should-error (orpheus-equalizer-delete-preset "flat")
                        :type 'user-error))
      (ignore-errors (delete-file file)))))

(ert-deftest orpheus-test/local-equalizer-stores-before-mpv ()
  "An equalizer set before mpv starts is kept for the connection."
  (let ((local (make-orpheus-local :name 'local))
        (sent nil))
    (cl-letf (((symbol-function 'orpheus-local--ready-p)
               (lambda (_local) nil))
              ((symbol-function 'orpheus-local--send)
               (lambda (&rest _arguments) (setq sent t))))
      (should (orpheus-backend-equalizer local "anull"))
      (should (equal (orpheus-local-instance-equalizer local) "anull"))
      (should-not sent))))

;;; Help and the tutorial

(ert-deftest orpheus-test/help-is-a-guided-buffer ()
  "The help buffer explains the concepts and links to the tutorial."
  (unwind-protect
      (progn
        (orpheus-help)
        (with-current-buffer "*orpheus help*"
          (let ((text (buffer-string)))
            (should (string-match-p "Getting started" text))
            (should (string-match-p "Equalizer" text))
            (should (string-match-p "If something is wrong" text))
            (should (next-button (point-min))))))
    (when (get-buffer "*orpheus help*")
      (kill-buffer "*orpheus help*"))))

(ert-deftest orpheus-test/guides-wrap-and-open-at-the-top ()
  "The guides use the window, wrap long lines and start at their title."
  (dolist (case '((orpheus-help . "*orpheus help*")
                  (orpheus-tutorial . "*orpheus tutorial*")))
    (unwind-protect
        (progn
          (funcall (car case))
          (with-current-buffer (cdr case)
            (should visual-line-mode)
            (should (equal wrap-prefix "  "))
            (should-not truncate-lines)
            (let ((window (get-buffer-window (current-buffer))))
              (when window
                (should (= (point-min) (window-start window)))))))
      (when (get-buffer (cdr case))
        (kill-buffer (cdr case))))))

(ert-deftest orpheus-test/tutorial-is-a-checklist ()
  "The tutorial walks through the player and can run its own steps."
  (unwind-protect
      (progn
        (orpheus-tutorial)
        (with-current-buffer "*orpheus tutorial*"
          (let ((text (buffer-string)))
            (should (string-match-p "Open the player" text))
            (should (string-match-p "Shape the sound" text))
            (should (next-button (point-min))))))
    (when (get-buffer "*orpheus tutorial*")
      (kill-buffer "*orpheus tutorial*"))))

;;; The visualizer

(ert-deftest orpheus-test/vis-parse-frame ()
  "cava's ascii frames split into levels, and partial frames wait."
  (pcase-let ((`(,levels . ,rest) (orpheus-vis--levels "" "10;20;30\n")))
    (should (equal levels '(10 20 30)))
    (should (equal rest "")))
  (pcase-let ((`(,levels . ,rest) (orpheus-vis--levels "" "10;20;3")))
    (should-not levels)
    (should (equal rest "10;20;3")))
  (pcase-let ((`(,levels . ,_) (orpheus-vis--levels "10;20;" "30\n")))
    (should (equal levels '(10 20 30)))))

(ert-deftest orpheus-test/vis-text-bar ()
  "A level fills the bar in proportion, never past its height."
  (should (equal (orpheus-vis--text-bar 100 8) (make-string 8 ?█)))
  (should (equal (orpheus-vis--text-bar 0 8) (make-string 8 orpheus-vis-empty-char)))
  (should (= (length (orpheus-vis--text-bar 50 8)) 8))
  (should (= (length (orpheus-vis--text-bar 100 5)) 5))
  (should (= (length (orpheus-vis--text-bar 999 5)) 5)))

(ert-deftest orpheus-test/vis-config-uses-pipewire ()
  "The generated cava configuration names the platform's capture source."
  (let ((linux (orpheus-vis--config-string 'pipewire 32 30)))
    (should (string-match-p "^\\[input\\]" linux))
    (should (string-match-p "method = pipewire" linux))
    (should (string-match-p "bars = 32" linux))
    (should (string-match-p "data_format = ascii" linux))
    (should (string-match-p "^\\[output\\]" linux)))
  (let ((mac (orpheus-vis--config-string 'portaudio 16 60)))
    (should (string-match-p "method = portaudio" mac))
    (should (not (string-match-p "pipewire" mac))))
  (let ((windows (orpheus-vis--config-string nil 16 60)))
    (should-not (string-match-p "\\[input\\]" windows))
    (should (string-match-p "^\\[output\\]" windows))))

(ert-deftest orpheus-test/vis-input-for-platform ()
  "Linux captures pipewire; macOS falls back to portaudio."
  (let ((orpheus-vis-input nil))
    (let ((system-type 'gnu/linux)) (should (eq (orpheus-vis--input) 'pipewire)))
    (let ((system-type 'darwin)) (should (eq (orpheus-vis--input) 'portaudio)))
    (let ((system-type 'windows-nt))
      (should-not (orpheus-vis--input))
      (should-not (string-match-p
                   "\\[input\\]"
                   (orpheus-vis--config-string
                    (orpheus-vis--input) 16 60)))))
  (let ((orpheus-vis-input 'fifo))
    (should (eq (orpheus-vis--input) 'fifo))))

(ert-deftest orpheus-test/vis-text-graph ()
  "The text graph is as many rows as it is tall, and one column per bar."
  (let ((graph (orpheus-vis--text '(10 50 100 0) 4 3)))
    (should (= 3 (length (split-string graph "\n"))))
    (dolist (row (split-string graph "\n"))
      (should (= 4 (length row))))))

(ert-deftest orpheus-test/vis-svg ()
  "The SVG renderer draws when it can, and returns nil when it cannot."
  (should (equal (car orpheus-vis-colors) (orpheus-vis--color 0)))
  (should (equal (car (last orpheus-vis-colors)) (orpheus-vis--color 100)))
  (when (image-type-available-p 'svg)
    (should (imagep (orpheus-vis--svg-image '(10 50 100) 120 40))))
  ;; An Emacs built without SVG must not signal once per frame.
  (cl-letf (((symbol-function 'image-type-available-p) #'ignore))
    (should-not (orpheus-vis--svg-image '(10 50 100) 120 40))))

(ert-deftest orpheus-test/vis-theme-picks-the-ramp-and-background ()
  "A named visualizer theme overrides the custom colours."
  (unwind-protect
      (progn
        (setq orpheus-vis-theme 'ember)
        (should (equal (orpheus-vis--color 100)
                       (car (last (plist-get
                                   (alist-get 'ember orpheus-vis-theme-presets)
                                   :colors)))))
        (should (equal (orpheus-vis--background-color) "#160d0a")))
    (setq orpheus-vis-theme nil)))

(ert-deftest orpheus-test/vis-render-falls-back-to-text ()
  "A graphical frame that cannot draw SVG gets the block-character graph."
  (let ((orpheus-vis-buffer-name "*orpheus-test-vis*")
        (orpheus-vis--data '(10 50 100))
        (orpheus-vis--image nil)
        (orpheus-vis--image-key nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _frame) t))
                    ((symbol-function 'orpheus-vis--svg-image) #'ignore))
            (with-current-buffer (get-buffer-create orpheus-vis-buffer-name)
              (orpheus-vis--render t)
              (should (string-match-p (string orpheus-vis-bar-char)
                                      (buffer-string))))))
      (kill-buffer orpheus-vis-buffer-name))))


;;; Portable playlists

(defun orpheus-test--with-playlist-directory (function)
  "Call FUNCTION with an empty `orpheus-playlist-directory'."
  (let ((orpheus-playlist-directory (make-temp-file "orpheus-playlists" t)))
    (unwind-protect
        (funcall function)
      (delete-directory orpheus-playlist-directory t))))

(ert-deftest orpheus-test/playlist-roundtrip ()
  "A portable playlist keeps URIs and tags across a write and read."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((file (orpheus-playlist-file "Night Drive"))
            (tracks (list (orpheus-track-create
                           :uri "/music/01.mp3" :title "One" :artist "Band"
                           :duration 125)
                          (orpheus-track-create
                           :uri "https://example.test/radio" :title "Radio")))
            (read (progn (orpheus-playlist-write file tracks)
                         (orpheus-playlist-read file))))
       (should (equal (file-name-nondirectory file) "Night Drive.m3u8"))
       (should (file-readable-p file))
       (should (equal (mapcar #'orpheus-track-uri read)
                      '("/music/01.mp3" "https://example.test/radio")))
       (should (equal (orpheus-track-title (car read)) "One"))
       (should (equal (orpheus-track-artist (car read)) "Band"))
       (should (= (orpheus-track-duration (car read)) 125))
       (with-temp-buffer
         (insert-file-contents file)
         (should (string-prefix-p "#EXTM3U\n" (buffer-string))))))))

(ert-deftest orpheus-test/playlist-name-stays-inside-directory ()
  "A playlist name cannot write outside the playlist directory."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((file (orpheus-playlist-file "../escape")))
       (should (equal (file-name-nondirectory file) "escape.m3u8"))
       (should (string-prefix-p
                (file-name-as-directory
                 (expand-file-name orpheus-playlist-directory))
                file)))
     (should-error (orpheus-playlist-file "")))))

(ert-deftest orpheus-test/windows-relative-paths-ignore-case ()
  "Windows paths compare without case, so ownership checks still match."
  (let* ((root (make-temp-file "orpheus-case" t))
         (directory (file-name-as-directory root))
         (uri (concat (upcase root) "/Album/a.mp3"))
         (local (make-orpheus-local :name 'local)))
    (unwind-protect
        (progn
          (let ((system-type 'windows-nt))
            (should (equal (orpheus-backend--relative-uri uri root)
                           "Album/a.mp3"))
            (should (orpheus-playlist--inside-p
                     (expand-file-name "Mix.m3u8" (upcase directory))
                     directory))
            (should (= 2 (orpheus-local--art-rank
                          directory
                          (concat (upcase directory) "Disc/cover.jpg"))))
            (setf (orpheus-local-instance-watches local)
                  (list (cons (upcase directory) 'watch)))
            (cl-letf (((symbol-function 'file-notify-rm-watch) #'ignore))
              (orpheus-local--watch-remove-tree local directory))
            (should-not (orpheus-local-instance-watches local)))
          (let ((system-type 'gnu/linux))
            (should-not (orpheus-backend--relative-uri uri root))
            (should-not (orpheus-playlist--inside-p
                         (expand-file-name "Mix.m3u8" (upcase directory))
                         directory))))
      (delete-directory root t))))

(ert-deftest orpheus-test/playlist-items ()
  "Portable playlists become rows with a file and a track count."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (orpheus-playlist-write
      (orpheus-playlist-file "Mix")
      (list (orpheus-track-create :uri "/a.mp3")
            (orpheus-track-create :uri "/b.mp3")))
     (with-temp-file (expand-file-name "notes.txt"
                                       orpheus-playlist-directory)
       (insert "not a playlist"))
     (let* ((items (orpheus-playlist-items))
            (item (car items)))
       (should (= (length items) 1))
       (should (equal (orpheus-item-label item) "Mix"))
       (should (eq (orpheus-item-kind item) 'playlist))
       (should (equal (alist-get 'count (orpheus-item-data item)) 2))
       (should (file-readable-p (alist-get 'file (orpheus-item-data item))))))))

(ert-deftest orpheus-test/playlist-plays-in-order ()
  "Playing a portable playlist clears the queue, plays its first track, then
appends the rest."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((file (progn
                    (orpheus-playlist-write
                     (orpheus-playlist-file "Mix")
                     (list (orpheus-track-create :uri "/a.mp3")
                           (orpheus-track-create :uri "/b.mp3")
                           (orpheus-track-create :uri "/c.mp3")))
                    (orpheus-playlist-file "Mix")))
            (orpheus-backend-current 'fake)
            (calls nil))
       (cl-letf (((symbol-function 'orpheus-backend-command)
                  (lambda (_backend action &optional value)
                    (push (list action value) calls)))
                 ((symbol-function 'orpheus-backend-play-uri)
                  (lambda (_backend uri) (push (list 'play uri) calls)))
                 ((symbol-function 'orpheus-backend-add)
                  (lambda (_backend uris) (push (list 'add uris) calls))))
         (should (= 3 (orpheus-playlist-play file))))
       (should (equal (nreverse calls)
                      '((clear nil) (play "/a.mp3")
                        (add ("/b.mp3" "/c.mp3")))))))))

(ert-deftest orpheus-test/playlist-add-queues-all ()
  "Adding a portable playlist appends every URI without clearing the queue."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((file (progn
                   (orpheus-playlist-write
                    (orpheus-playlist-file "Mix")
                    (list (orpheus-track-create :uri "/a.mp3")
                          (orpheus-track-create :uri "/b.mp3")))
                   (orpheus-playlist-file "Mix")))
           (orpheus-backend-current 'fake)
           (added nil))
       (cl-letf (((symbol-function 'orpheus-backend-add)
                  (lambda (_backend uris) (setq added uris))))
         (should (= 2 (orpheus-playlist-add file))))
       (should (equal added '("/a.mp3" "/b.mp3")))))))

(ert-deftest orpheus-test/playlist-save-queue-uses-relative-paths ()
  "Saving a local queue below its root stores a library-relative URI."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((root (make-temp-file "orpheus-music" t))
            (orpheus-local-directory root)
            (orpheus-backend-current (orpheus-local--make))
            (uri (expand-file-name "Album/a.mp3" root))
            (items (list (orpheus-item-create
                          :kind 'track
                          :data (list (cons 'uri uri)
                                      (cons 'track
                                            (orpheus-track-create
                                             :uri uri :title "A")))))))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'orpheus-backend-queue)
                        (lambda (_backend callback)
                          (funcall callback items))))
               (orpheus-playlist-save-queue "Portable" #'ignore))
             (should
              (equal (mapcar #'orpheus-track-uri
                             (orpheus-playlist-read
                              (orpheus-playlist-file "Portable")))
                     '("Album/a.mp3"))))
         (delete-directory root t))))))

(ert-deftest orpheus-test/playlist-uris-resolve-across-backends ()
  "Relative playlist paths resolve for local, MPD and SSH."
  (let ((root (make-temp-file "orpheus-music" t))
        (mpd (make-orpheus-mpd :name 'mpd))
        (ssh (orpheus-ssh--create :name 'ssh :volume 100)))
    (unwind-protect
        (let ((orpheus-local-directory root)
              (orpheus-ssh-directory "Music")
              (local (orpheus-local--make)))
          (should (equal (orpheus-backend-playlist-uri local "Album/a.mp3")
                         (expand-file-name "Album/a.mp3" root)))
          (should (equal (orpheus-backend-playlist-uri
                          mpd (expand-file-name "Album/a.mp3" root))
                         "Album/a.mp3"))
          (should (equal (orpheus-backend-playlist-uri ssh "Album/a.mp3")
                         "Music/Album/a.mp3"))
          (should (equal (orpheus-backend-portable-uri
                          ssh "Music/Album/a.mp3")
                         "Album/a.mp3")))
      (delete-directory root t))))

(ert-deftest orpheus-test/playlist-play-resolves-relative-paths ()
  "Playing a relative playlist asks the backend for the resolved URI."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((root (make-temp-file "orpheus-music" t))
            (file (orpheus-playlist-file "Portable"))
            (orpheus-local-directory root)
            (local (orpheus-local--make))
            (orpheus-backend-current local)
            (played nil))
       (unwind-protect
           (progn
             (orpheus-playlist-write
              file (list (orpheus-track-create :uri "Album/a.mp3")))
             (cl-letf (((symbol-function 'orpheus-backend-command)
                        (lambda (_backend action &optional value)
                          (ignore action value)))
                       ((symbol-function 'orpheus-backend-play-uri)
                        (lambda (_backend uri) (setq played uri)))
                       ((symbol-function 'orpheus-backend-add)
                        (lambda (_backend uris) (ignore uris))))
               (should (= 1 (orpheus-playlist-play file))))
             (should (equal played (expand-file-name "Album/a.mp3" root))))
         (delete-directory root t))))))

(ert-deftest orpheus-test/playlist-editor-reorders-and-keeps-tags ()
  "The editor moves a track and saves it without losing its tags."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((file (orpheus-playlist-file "Edit"))
            (first (orpheus-track-create
                    :uri "/a.mp3" :title "One" :artist "Band"
                    :duration 125))
            (second (orpheus-track-create
                     :uri "/b.mp3" :title "Two" :artist "Other")))
       (orpheus-playlist-write file (list first second))
       (let ((buffer (save-window-excursion (orpheus-playlist-edit file))))
         (unwind-protect
             (with-current-buffer buffer
               (goto-char (point-min))
               (orpheus-playlist-editor-move-down)
               (orpheus-playlist-editor-save))
           (kill-buffer buffer)))
       (let ((read (orpheus-playlist-read file)))
         (should (equal (mapcar #'orpheus-track-uri read)
                        '("/b.mp3" "/a.mp3")))
         (should (equal (orpheus-track-title (car read)) "Two"))
         (should (equal (orpheus-track-artist (car read)) "Other")))))))

(ert-deftest orpheus-test/playlist-editor-refuses-pls ()
  "The editor does not overwrite PLS with M3U8 content."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((file (expand-file-name "Imported.pls"
                                   orpheus-playlist-directory)))
       (with-temp-file file
         (insert "[playlist]\nFile1=/a.mp3\nTitle1=A\nNumberOfEntries=1\n"))
       (should-error (orpheus-playlist-edit file) :type 'user-error)))))

(ert-deftest orpheus-test/playlist-editor-removes ()
  "The editor removes the row at point and saves the rest."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((file (orpheus-playlist-file "Edit")))
       (orpheus-playlist-write
        file (list (orpheus-track-create :uri "/a.mp3")
                   (orpheus-track-create :uri "/b.mp3")))
       (let ((buffer (save-window-excursion (orpheus-playlist-edit file))))
         (unwind-protect
             (with-current-buffer buffer
               (goto-char (point-min))
               (orpheus-playlist-editor-delete)
               (orpheus-playlist-editor-save))
           (kill-buffer buffer)))
       (should (equal (mapcar #'orpheus-track-uri
                              (orpheus-playlist-read file))
                      '("/b.mp3")))))))

(ert-deftest orpheus-test/playlist-editor-inserts-from-player ()
  "The editor inserts the player's current track at point."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((file (orpheus-playlist-file "Insert"))
            (player-name " *orpheus-test-player*")
            (player (get-buffer-create player-name))
            (orpheus-ui-buffer-name player-name)
            (editor nil))
       (unwind-protect
           (progn
             (orpheus-playlist-write
              file (list (orpheus-track-create :uri "/a.mp3")))
             (with-current-buffer player
               (orpheus-ui-mode)
               (let* ((track (orpheus-track-create
                              :uri "/new.mp3" :title "New"))
                      (item (orpheus-item-create
                             :kind 'track :label "New"
                             :data (list (cons 'uri "/new.mp3")
                                         (cons 'track track))))
                      (tab (orpheus-ui-tab-create
                            :kind 'queue :title "Queue" :view 'queue
                            :items (list item))))
                 (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
                 (orpheus-ui--render)
                 (goto-char (point-min))
                 (orpheus-ui--place-point)))
             (setq editor (save-window-excursion (orpheus-playlist-edit file)))
             (with-current-buffer editor
               (goto-char (point-min))
               (orpheus-playlist-editor-insert)
               (orpheus-playlist-editor-save))
             (let ((read (orpheus-playlist-read file)))
               (should (equal (mapcar #'orpheus-track-uri read)
                              '("/new.mp3" "/a.mp3")))
               (should (equal (orpheus-track-title (car read)) "New"))))
         (when (buffer-live-p editor) (kill-buffer editor))
         (kill-buffer player))))))

(ert-deftest orpheus-test/mouse-menus-and-editor-map ()
  "Player rows, tabs and playlist rows expose their mouse actions."
  (should (eq (lookup-key orpheus-ui-item-map [mouse-3])
              'orpheus-ui-mouse-menu))
  (should (eq (lookup-key orpheus-ui-tab-map [mouse-3])
              'orpheus-ui-mouse-tab-menu))
  (should (eq (lookup-key orpheus-playlist-editor-item-map [mouse-1])
              'orpheus-playlist-editor--select))
  (should (eq (lookup-key orpheus-playlist-editor-item-map [mouse-3])
              'orpheus-playlist-editor-mouse-menu))
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((file (orpheus-playlist-file "Mouse")))
       (orpheus-playlist-write
        file (list (orpheus-track-create :uri "/a.mp3")))
       (let ((buffer (save-window-excursion (orpheus-playlist-edit file))))
         (unwind-protect
             (with-current-buffer buffer
               (goto-char (point-min))
               (should (eq (get-text-property (point) 'keymap)
                           orpheus-playlist-editor-item-map))
               (should (eq (get-text-property (point) 'mouse-face)
                           'highlight)))
           (kill-buffer buffer)))))))

;;; The interface, without a frame

(ert-deftest orpheus-test/ui-edits-portable-playlist ()
  "`e' opens the portable playlist file at point in the editor."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (orpheus-playlist-write
      (orpheus-playlist-file "Mix")
      (list (orpheus-track-create :uri "/a.mp3")))
     (let* ((file (orpheus-playlist-file "Mix"))
            (item (car (orpheus-playlist-items)))
            (tab (orpheus-ui-tab-create
                  :kind 'playlists :title "Playlists" :view 'playlists
                  :items (list item)))
            (opened nil))
       (with-temp-buffer
         (orpheus-ui-mode)
         (should (eq (key-binding (kbd "e")) 'orpheus-ui-edit-playlist))
         (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
         (orpheus-ui--render)
         (goto-char (point-min))
         (orpheus-ui--place-point)
         (cl-letf (((symbol-function 'orpheus-playlist-edit)
                    (lambda (path) (setq opened path) nil)))
           (orpheus-ui-edit-playlist)
           (should (equal opened file))))))))

(ert-deftest orpheus-test/ui-tabs ()
  "The tabs follow what the backend says it can do, plus portable playlists."
  (should (equal (mapcar #'car
                         (orpheus-ui--tabs-for-capabilities
                          '(queue directories artists albumartists albums
                            playlists search)))
                 '(queue directories artists albumartists albums playlists
                   search)))
  (should (equal (mapcar #'car
                         (orpheus-ui--tabs-for-capabilities
                          '(queue downloads directories search)))
                 '(queue downloads directories playlists search)))
  (should (equal (orpheus-ui-kind-title 'downloads) "Downloads"))
  (should (equal (mapcar #'car
                         (orpheus-ui--tabs-for-capabilities
                          '(queue directories)))
                 '(queue directories playlists)))
  (should (equal (mapcar #'car (orpheus-ui--tabs-for-capabilities nil))
                 '(queue))))

(ert-deftest orpheus-test/ui-plays-portable-playlist ()
  "RET on a portable playlist row loads that playlist."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (orpheus-playlist-write
      (orpheus-playlist-file "Mix")
      (list (orpheus-track-create :uri "/a.mp3")))
     (let* ((file (orpheus-playlist-file "Mix"))
            (item (car (orpheus-playlist-items)))
            (tab (orpheus-ui-tab-create
                  :kind 'playlists :title "Playlists" :view 'playlists
                  :items (list item)))
            (loaded nil))
       (with-temp-buffer
         (orpheus-ui-mode)
         (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
         (orpheus-ui--render)
         (goto-char (point-min))
         (orpheus-ui--place-point)
         (cl-letf (((symbol-function 'orpheus-playlist-play)
                    (lambda (path) (setq loaded path) 1)))
           (orpheus-ui-confirm)
           (should (equal loaded file))))))))

(ert-deftest orpheus-test/ui-adds-portable-playlist ()
  "`a' on a portable playlist queues every track it names."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (orpheus-playlist-write
      (orpheus-playlist-file "Mix")
      (list (orpheus-track-create :uri "/a.mp3")
            (orpheus-track-create :uri "/b.mp3")))
     (let* ((item (car (orpheus-playlist-items)))
            (tab (orpheus-ui-tab-create
                  :kind 'playlists :title "Playlists" :view 'playlists
                  :items (list item)))
            (orpheus-backend-current 'fake)
            (added nil))
       (with-temp-buffer
         (orpheus-ui-mode)
         (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
         (orpheus-ui--render)
         (goto-char (point-min))
         (orpheus-ui--place-point)
         (cl-letf (((symbol-function 'orpheus-backend-add)
                    (lambda (_backend uris) (setq added uris))))
           (orpheus-ui-add)
           (should (equal added '("/a.mp3" "/b.mp3")))))))))

(ert-deftest orpheus-test/ui-saves-queue-as-portable-playlist ()
  "The queue is written as a portable playlist, whichever backend owns it."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let* ((items (list (orpheus-item-create
                          :kind 'track
                          :data (list (cons 'uri "/a.mp3")
                                      (cons 'track
                                            (orpheus-track-create
                                             :uri "/a.mp3" :title "A"
                                             :artist "Band"))))
                         (orpheus-item-create
                          :kind 'track :data '((uri . "/b.mp3")))))
            (orpheus-backend-current 'fake)
            (message-text nil))
       (cl-letf (((symbol-function 'orpheus-backend-queue)
                  (lambda (_backend callback) (funcall callback items)))
                 ((symbol-function 'message)
                  (lambda (format &rest args)
                    (setq message-text (apply #'format format args)))))
         (orpheus-ui-save-playlist "Queue"))
       (let ((file (orpheus-playlist-file "Queue")))
         (should (file-readable-p file))
         (should (equal (mapcar #'orpheus-track-uri
                                (orpheus-playlist-read file))
                        '("/a.mp3" "/b.mp3"))))
       (should (string-match-p "Queue" message-text))))))

(ert-deftest orpheus-test/ui-playlists-merge-server-rows ()
  "Portable playlists and a backend's own playlists share the tab."
  (orpheus-test--with-playlist-directory
   (lambda ()
     (let ((portable (orpheus-item-create
                      :label "Local" :kind 'playlist
                      :data '((file . "/tmp/Local.m3u8"))))
           (server (orpheus-item-create :label "Server" :kind 'playlist
                                        :data '((browse . (playlists "Server")))))
           (orpheus-backend-current 'fake)
           (asked nil)
           (rows nil))
       (cl-letf (((symbol-function 'orpheus-playlist-items)
                  (lambda () (list portable)))
                 ((symbol-function 'orpheus-backend-capabilities)
                  (lambda (_backend) '(queue playlists)))
                 ((symbol-function 'orpheus-backend-browse)
                  (lambda (_backend kind arg callback)
                    (setq asked (list kind arg))
                    (funcall callback (list server)))))
         (orpheus-ui--playlist-items
          (lambda (items) (setq rows items))))
       (should (equal asked '(playlists nil)))
       (should (equal rows (list portable server)))))))

(ert-deftest orpheus-test/ui-elide ()
  "A long line is cut to the width, with an ellipsis, never widened."
  (should (equal (orpheus-ui--elide "short" 10) "short"))
  (should (= (string-width (orpheus-ui--elide "a very long line" 8)) 8))
  (should (string-suffix-p "…" (orpheus-ui--elide "a very long line" 8)))
  (should (equal (orpheus-ui--elide "wide字" 3) "wi…")))

(ert-deftest orpheus-test/ui-columns ()
  "The right column ends at the width, and the left one gets the rest."
  (should (equal (orpheus-ui--columns "ab" "cd" 6) "ab  cd"))
  (should (= (string-width (orpheus-ui--columns "abcdefgh" "cd" 6)) 6))
  (should (equal (orpheus-ui--columns "" "cd" 4) "  cd")))

(ert-deftest orpheus-test/ui-status-string ()
  "The status line names the song, the time, the volume and the flags."
  (let* ((orpheus-ui-plain-ascii t)
         (status (orpheus-status-create :state 'play :elapsed 65 :total 200
                                        :volume 80 :repeat t :random t))
         (track (orpheus-track-create :title "Song" :artist "Band"
                                      :album "Record"))
         (line (orpheus-ui--status-string status track)))
    (should (string-match-p ">" line))
    (should (string-match-p "Band – Song" line))
    (should (string-match-p "1:05 / 3:20" line))
    (should (string-match-p "vol 80" line))
    (should (string-match-p "R" line))
    (should (string-match-p "Z" line))))

(ert-deftest orpheus-test/ui-status-string-fits-a-width ()
  "A narrow header elides the track instead of clipping the controls."
  (let* ((status (orpheus-status-create :state 'play :elapsed 65 :total 200
                                       :volume 80 :repeat t :random t))
         (track (orpheus-track-create :title (make-string 60 ?x)
                                      :artist "Band"))
         (line (orpheus-ui--status-string status track 72)))
    (should (<= (string-width line) 72))
    (should (string-match-p "1:05 / 3:20" line))
    (should (string-match-p "vol 80" line))
    (should (string-match-p "R" line))
    (should (string-match-p "Z" line))))

(ert-deftest orpheus-test/ui-background-summary ()
  "The header reports a backend's background work."
  (let ((orpheus-backend-current (make-orpheus-local :name 'local))
        (orpheus-local--cue-index-running t)
        (orpheus-local--cue-index-total 5)
        (orpheus-local--cue-index-done 2))
    (should (equal "  indexing cues 2/5"
                   (orpheus-ui--background-summary-string)))
    (let ((line (orpheus-ui--status-string
                 (orpheus-status-create :state 'play :elapsed 1 :total 2
                                        :volume 50)
                 (orpheus-track-create :title "Song"))))
      (should (string-match-p "indexing cues 2/5" line)))))

(ert-deftest orpheus-test/ui-download-summary ()
  "The header counts active and queued transfers and their aggregate speed."
  (let ((orpheus-ui-plain-ascii t)
        (orpheus-ui--downloads
         (list (orpheus-item-create
                :kind 'download
                :data '((state . downloading) (speed . 1024)))
               (orpheus-item-create
                :kind 'download
                :data '((state . queued))))))
    (let ((summary (orpheus-ui--download-summary-string)))
      (should (string-match-p "v 2" summary))
      (should (string-match-p "1KiB/s" summary))))
  (let ((orpheus-ui--downloads
         (list (orpheus-item-create
                :kind 'download
                :data '((state . done) (bytes . 2048))))))
    (should-not (orpheus-ui--download-summary-string))))

(ert-deftest orpheus-test/ui-now-playing-card ()
  "The now-playing card names the track, its artist and the progress."
  (let ((orpheus-ui-plain-ascii t)
        (orpheus-track-current (orpheus-track-create :title "Song"
                                                     :artist "Band"))
        (orpheus-status-current (orpheus-status-create
                                :state 'play :elapsed 30 :total 60)))
    (with-temp-buffer
      (orpheus-ui--insert-now-playing 40)
      (let ((text (buffer-string)))
        (should (string-match-p "Song" text))
        (should (string-match-p "Band" text))
        (should (string-match-p "0:30 / 1:00" text))))))

(ert-deftest orpheus-test/ui-row-indexed ()
  "Each row carries its index, so a click or RET knows the item."
  (let ((item (orpheus-item-create :label "Song" :detail "3:20"
                                   :kind 'track :data '((uri . "a.mp3")))))
    (with-temp-buffer
      (orpheus-ui--insert-item item 4 40)
      (goto-char (point-min))
      (should (= (get-text-property (point) 'orpheus-item) 4))
      (should (equal (get-text-property (point) 'orpheus-item-kind) 'track))
      (should (string-match-p "Song" (buffer-string)))
      (should (string-match-p "3:20" (buffer-string))))))

(ert-deftest orpheus-test/ui-item-playable ()
  "Directories are entered, tracks are played, and both can be added."
  (should (orpheus-item-browse (orpheus-item-create
                                :kind 'directory
                                :data '((browse . (directories . "Rock"))))))
  (should (orpheus-item-playable
           (orpheus-item-create :kind 'track :data '((uri . "a.mp3")))))
  (should-not (orpheus-item-playable
               (orpheus-item-create :kind 'directory
                                    :data '((browse . (directories . "x")))))))


;;; The interface with a buffer

(ert-deftest orpheus-test/ui-mode ()
  "The mode is a special mode with its own keymap, and the buffer is read-only."
  (with-temp-buffer
    (orpheus-ui-mode)
    (should (derived-mode-p 'special-mode))
    (should (eq major-mode 'orpheus-ui-mode))
    (should (keymapp orpheus-ui-mode-map))
    (dolist (key '("p" "s" "n" "a" "d" "?" "/" "g"))
      (should (commandp (lookup-key orpheus-ui-mode-map (kbd key)))))
    (should (eq (lookup-key orpheus-ui-mode-map (kbd "p"))
                #'orpheus-ui-previous-line))
    (should (eq (lookup-key orpheus-ui-mode-map (kbd "SPC"))
                #'orpheus-play-pause))
    (should (commandp (lookup-key orpheus-ui-mode-map (kbd "RET"))))))

(ert-deftest orpheus-test/ui-render-queue ()
  "Rendering a queue shows the tracks and marks the playing one."
  (let ((orpheus-status-current (orpheus-status-create :state 'play :songid 8
                                                       :elapsed 1 :total 10))
        (orpheus-track-current (orpheus-track-create :title "Two"
                                                     :artist "Band")))
    (with-temp-buffer
      (orpheus-ui-mode)
      (setq orpheus-ui--tabs
            (vector (orpheus-ui-tab-create
                     :kind 'queue :title "Queue"
                     :items (list (orpheus-item-create
                                   :label "One" :detail "3:20"
                                   :kind 'track :data '((id . 7)))
                                  (orpheus-item-create
                                   :label "Two" :detail "0:10"
                                   :kind 'track :data '((id . 8)))))))
      (setq orpheus-ui--tab 0)
      (orpheus-ui--render)
      (goto-char (point-min))
      (should (string-match-p "Queue" (buffer-string)))
      (should (string-match-p "One" (buffer-string)))
      (should (string-match-p "Two" (buffer-string)))
      (should (text-property-not-all (point-min) (point-max) 'face nil)))))

(ert-deftest orpheus-test/ui-queue-is-not-a-source ()
  "The queue tab cannot be queued into, and every other tab still can.
Adding a queue row to the queue appends the whole queue to itself, so one
keystroke doubles it and a few of them fill it with copies."
  (let ((added nil))
    (cl-letf (((symbol-function 'orpheus-backend-add)
               (lambda (_backend uris) (push uris added))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'queue :title "Queue"
                       :items (list (orpheus-item-create
                                     :label "One" :kind 'track
                                     :data '((uri . "a.mp3") (pos . 0)))
                                    (orpheus-item-create
                                     :label "Two" :kind 'track
                                     :data '((uri . "b.mp3") (pos . 1)))))
                      (orpheus-ui-tab-create
                       :kind 'directories :title "Directories"
                       :items (list (orpheus-item-create
                                     :label "Solo" :kind 'track
                                     :data '((uri . "c.mp3")))))))
        (setq orpheus-ui--tab 0)
        (orpheus-ui--render)
        (orpheus-ui--place-point)
        (orpheus-ui-add)
        (should-not added)
        (orpheus-ui-add-all)
        (should-not added)
        (setq orpheus-ui--tab 1)
        (orpheus-ui--render)
        (orpheus-ui--place-point)
        (orpheus-ui-add)
        (orpheus-ui-add-all)
        ;; A row arrives as one URI, the whole tab as a list of them.
        (should (equal added '(("c.mp3") "c.mp3")))))))

(ert-deftest orpheus-test/ui-add-asks-before-a-large-queue ()
  "Queueing more than the threshold asks first; no leaves the queue alone."
  (let* ((orpheus-ui-add-confirm-threshold 2)
         (tab (orpheus-ui-tab-create
               :kind 'directories :title "Directories"
               :items (list (orpheus-item-create :label "One" :kind 'track
                                                 :data '((uri . "a.mp3")))
                            (orpheus-item-create :label "Two" :kind 'track
                                                 :data '((uri . "b.mp3")))
                            (orpheus-item-create :label "Three" :kind 'track
                                                 :data '((uri . "c.mp3"))))))
         (added nil)
         (asked nil))
    (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
    (cl-letf (((symbol-function 'orpheus-backend-add)
               (lambda (_backend uris) (setq added uris)))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt) (setq asked prompt) nil)))
      (orpheus-ui-add-all)
      (should asked)
      (should-not added))
    (cl-letf (((symbol-function 'orpheus-backend-add)
               (lambda (_backend uris) (setq added uris)))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t)))
      (orpheus-ui-add-all)
      (should (equal added '("a.mp3" "b.mp3" "c.mp3"))))))

(ert-deftest orpheus-test/ui-warns-about-a-huge-queue ()
  "A queue of thousands says how to clear it, and says it once."
  (let ((orpheus-ui-queue-warning-size 3)
        (orpheus-ui--queue-warned nil)
        (messages nil)
        (tab (orpheus-ui-tab-create :kind 'queue :view 'queue :title "Queue")))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (orpheus-ui--set-items tab (make-list 4 nil))
      (should (string-match-p "D clears it" (car messages)))
      (orpheus-ui--set-items tab (make-list 4 nil))
      (should (= (length messages) 1)))))


;;; Helpers shared by the UI and the backends

(ert-deftest orpheus-test/browse-kinds-are-known ()
  "Every browse kind has a tab title and a key."
  (dolist (kind '(queue downloads directories artists albumartists albums
                  playlists search))
    (should (orpheus-ui-kind-title kind))
    ;; The kind is documented in the backend protocol, so it must be reachable
    ;; from a method that handles `orpheus-backend-browse'.
    (should (memq kind orpheus-backend-browse-kinds))))

(ert-deftest orpheus-test/readme-options-exist ()
  "Every option the configuration tables name is really a user option."
  (let* ((root (file-name-directory orpheus-test--source))
         (file (expand-file-name "docs/configuration.org" root))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (position 0)
         (named nil))
    ;; Only the rows of the tables, whose first column is an option name.
    (while (string-match "^| \\(?:~\\|=\\)\\(orpheus-[a-z0-9-]+\\)\\(?:~\\|=\\)" text position)
      (push (intern (match-string 1 text)) named)
      (setq position (match-end 0)))
    (should (> (length named) 20))
    (dolist (symbol named)
      (should (custom-variable-p symbol)))))

(ert-deftest orpheus-test/readme-symbols-exist ()
  "Every public `orpheus-…' symbol the documentation names is defined.
The double dash marks helpers and the hypothetical function in the extension
example.  The value types are named as types, so they are allowed by name."
  (let ((checked 0)
        (types '(orpheus-track orpheus-status orpheus-item orpheus-lyrics)))
    (dolist (file (orpheus-test--documentation-files))
      (let ((text (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
            (position 0))
        (while (string-match "\\(?:~\\|=\\)\\(orpheus-[a-z0-9-]*[a-z0-9]\\)\\(?:~\\|=\\)" text position)
          (let ((symbol (intern (match-string 1 text))))
            (setq position (match-end 0))
            (unless (string-prefix-p "orpheus--" (symbol-name symbol))
              (setq checked (1+ checked))
              (should (or (boundp symbol) (fboundp symbol)
                          (memq symbol types))))))))
    (should (> checked 30))))

(ert-deftest orpheus-test/docs-use-org-code-markup ()
  "The Org documentation uses Org code markup, not Markdown backticks."
  (let ((root (file-name-directory orpheus-test--source)))
    (dolist (file (append (orpheus-test--documentation-files)
                          (list (expand-file-name "NEWS.org" root))))
      (let ((text (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
        (should-not (string-match-p "`" text))))))

(ert-deftest orpheus-test/documentation-file-links-resolve ()
  "Every local file: link in the documentation points at a file."
  (let ((checked 0))
    (dolist (file (orpheus-test--documentation-files))
      (let ((text (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
            (directory (file-name-directory file))
            (position 0))
        (while (string-match "\\[\\[file:\\([^]]+\\)\\]" text position)
          ;; Save the match end before `expand-file-name' can reuse the match
          ;; data, or the loop restarts at the same link forever.
          (let* ((target (match-string 1 text))
                 (end (match-end 0))
                 (path (car (split-string target "#")))
                 (resolved (expand-file-name path directory)))
            (setq position end)
            (setq checked (1+ checked))
            (should (file-exists-p resolved))))))
    (should (> checked 20))))

(ert-deftest orpheus-test/logo-file-is-packaged ()
  "The player logo ships beside the source."
  (should (file-readable-p orpheus-ui--logo-file)))

(ert-deftest orpheus-test/logo-is-not-an-image-in-a-terminal ()
  "A terminal player keeps the logo out of the text."
  (let ((orpheus-ui--logo-image-cache nil))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) nil)))
      (should-not (orpheus-ui--logo-string)))))

;;; Marks, icons and the parent row

(ert-deftest orpheus-test/ui-icons ()
  "Icons follow the mode: Unicode, ASCII, or none at all."
  (let ((orpheus-ui-icons t)
        (orpheus-ui-plain-ascii nil))
    (should (equal (orpheus-ui--icon 'directory) "📁"))
    (should (equal (orpheus-ui--icon 'track) "🎵"))
    (should-not (orpheus-ui--icon 'unknown-kind)))
  (let ((orpheus-ui-icons t)
        (orpheus-ui-plain-ascii t))
    (should (equal (orpheus-ui--icon 'directory) "d"))
    (should (equal (orpheus-ui--icon 'track) "m")))
  (let ((orpheus-ui-icons nil)
        (orpheus-ui-plain-ascii nil))
    (should-not (orpheus-ui--icon 'track))))

(ert-deftest orpheus-test/ui-row-has-an-icon-and-a-mark-column ()
  "A row starts with the mark column and the icon, then the label."
  (let ((orpheus-ui-icons t)
        (orpheus-ui-plain-ascii t))
    (with-temp-buffer
      (orpheus-ui--insert-item
       (orpheus-item-create :label "Song" :detail "1:00" :kind 'track
                            :data '((uri . "a.mp3")))
       0 30)
      (should (string-prefix-p " m Song" (buffer-string))))))

(ert-deftest orpheus-test/ui-marks-drive-the-actions ()
  "Marked rows are what `a' queues, not the row under point."
  (let ((added nil))
    (cl-letf (((symbol-function 'orpheus-backend-add)
               (lambda (_backend uris) (setq added uris))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'albums :title "Albums"
                       :items (list (orpheus-item-create
                                     :label "One" :kind 'track
                                     :data '((uri . "a.mp3")))
                                    (orpheus-item-create
                                     :label "Two" :kind 'track
                                     :data '((uri . "b.mp3")))
                                    (orpheus-item-create
                                     :label "Three" :kind 'track
                                     :data '((uri . "c.mp3")))))))
        (setq orpheus-ui--tab 0)
        (orpheus-ui--render)
        (orpheus-ui--place-point)
        (orpheus-ui-mark)
        (orpheus-ui-mark)
        (orpheus-ui-add)
        (should (equal added '("a.mp3" "b.mp3")))
        (should (equal (sort (copy-sequence
                              (orpheus-ui-tab-marks (orpheus-ui--current-tab)))
                             #'string<)
                       '("a.mp3" "b.mp3")))
        (orpheus-ui-unmark-all)
        (should-not (orpheus-ui-tab-marks (orpheus-ui--current-tab)))))))

(ert-deftest orpheus-test/ui-toggle-marks ()
  "`t' inverts the marks, as it does in Dired."
  (with-temp-buffer
    (orpheus-ui-mode)
    (setq orpheus-ui--tabs
          (vector (orpheus-ui-tab-create
                   :kind 'albums :title "Albums"
                   :items (list (orpheus-item-create
                                 :label "One" :kind 'track
                                 :data '((uri . "a.mp3")))
                                (orpheus-item-create
                                 :label "Two" :kind 'track
                                 :data '((uri . "b.mp3"))))
                   :marks '("a.mp3"))))
    (setq orpheus-ui--tab 0)
    (orpheus-ui-toggle-marks)
    (should (equal (orpheus-ui-tab-marks (orpheus-ui--current-tab))
                   '("b.mp3")))))

(ert-deftest orpheus-test/ui-parent-row ()
  "A directory list can leave the directory, as Dired's `..' does."
  (let ((item (orpheus-ui--parent-item
               (orpheus-ui-tab-create :kind 'directories :view 'directories
                                      :arg (list "/home/me/Music")))))
    (should (equal (orpheus-item-label item) "../"))
    (should (equal (orpheus-item-browse item) '(directories "/home/me/"))))
  (should (equal (orpheus-item-browse
                  (orpheus-ui--parent-item
                   (orpheus-ui-tab-create :kind 'directories :view 'directories
                                          :arg (list "Rock/Live"))))
                 '(directories "Rock/")))
  ;; A top-level directory's parent is the root, which is the empty path.
  (should (equal (orpheus-item-browse
                  (orpheus-ui--parent-item
                   (orpheus-ui-tab-create :kind 'directories :view 'directories
                                          :arg (list "Rock/"))))
                 '(directories "")))
  (should-not (orpheus-ui--parent-item
               (orpheus-ui-tab-create :kind 'directories :view 'directories)))
  (should-not (orpheus-ui--parent-item
               (orpheus-ui-tab-create :kind 'queue :view 'queue))))

(ert-deftest orpheus-test/ui-parent-row-confirms ()
  "RET on the `../' row moves the tab to the parent directory."
  (let* ((tab (orpheus-ui-tab-create :kind 'directories :view 'directories
                                     :title "Directories"
                                     :arg '("Rock/Live/")))
         (browsed nil)
         (orpheus-ui-buffer-name
          (generate-new-buffer-name "*orpheus-test-parent*"))
         (orpheus-backend-current 'fake))
    (with-temp-buffer
      (rename-buffer orpheus-ui-buffer-name t)
      (orpheus-ui-mode)
      (setq orpheus-ui--tabs (vector tab)
            orpheus-ui--tab 0)
      (orpheus-ui--set-items tab
                             (list (orpheus-item-create
                                    :label "Song" :kind 'track
                                    :data '((uri . "x.mp3")))))
      (orpheus-ui--render)
      (goto-char (point-min))
      (orpheus-ui--place-point)
      (cl-letf (((symbol-function 'orpheus-backend-connected-p)
                 (lambda (&rest _) t))
                ((symbol-function 'orpheus-backend-browse)
                 (lambda (_backend view arg done)
                   (setq browsed (list view arg))
                   (funcall done nil))))
        (orpheus-ui-confirm)
        (should (equal (orpheus-ui-tab-arg tab) '("Rock/")))
        (should (equal browsed '(directories ("Rock/"))))))))


;;; Reading only when something changed

(ert-deftest orpheus-test/ui-tick-does-not-read-a-clean-tab ()
  "The one-second timer must not ask the backend for the queue again."
  (let ((orpheus-ui-buffer-name "*orpheus-test-tick*")
        (refreshes 0)
        (redraws 0)
        (orpheus-ui--needs-refresh nil))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode))
          (cl-letf (((symbol-function 'orpheus-ui-refresh)
                     (lambda (&optional _force)
                       (setq refreshes (1+ refreshes))))
                    ((symbol-function 'orpheus-ui--redraw)
                     (lambda () (setq redraws (1+ redraws)))))
            (dotimes (_ 5) (orpheus-ui-tick))
            (should (= refreshes 0))
            (should (= redraws 5))
            (orpheus-ui-invalidate)
            (orpheus-ui-tick)
            (should (= refreshes 1))))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-render-keeps-the-window-line ()
  "A redraw keeps the same text line at the top of the window.
A line added above the list — the art box, the now-playing card or a path —
must not scroll the list out from under the reader."
  (let ((orpheus-ui-buffer-name "*orpheus-test-scroll*")
        (orpheus-ui-show-art nil)
        (orpheus-ui-show-now-playing nil)
        (orpheus-ui--art nil))
    (unwind-protect
        (let ((buffer (get-buffer-create orpheus-ui-buffer-name)))
          (with-current-buffer buffer
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs
                  (vector (orpheus-ui-tab-create
                           :kind 'queue :view 'queue :title "Queue"
                           :items (mapcar
                                   (lambda (n)
                                     (orpheus-item-create
                                      :label (format "row %d" n)
                                      :kind 'track
                                      :data (list (cons 'uri
                                                        (format "%d.mp3" n)))))
                                   (number-sequence 1 40)))))
            (setq orpheus-ui--tab 0)
            (orpheus-ui--render))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (goto-char (point-min))
            (forward-line 10)
            (set-window-start (selected-window) (point))
            (let ((line (count-lines (point-min) (window-start))))
              (setf (orpheus-ui-tab-arg (orpheus-ui--current-tab)) '("Rock/"))
              (orpheus-ui--render)
              (should (= line (count-lines (point-min) (window-start)))))))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-render-forces-the-top-line ()
  "Redisplay may not scroll to point behind a redraw.
`set-window-start' is called without NOFORCE, so a row far down the list
that holds point cannot drag the cover off the top of the window."
  (let ((orpheus-ui-buffer-name "*orpheus-test-scroll-top*")
        (orpheus-ui-show-art nil)
        (orpheus-ui-show-now-playing nil)
        (calls nil))
    (unwind-protect
        (let ((buffer (get-buffer-create orpheus-ui-buffer-name)))
          (with-current-buffer buffer
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs
                  (vector (orpheus-ui-tab-create
                           :kind 'queue :view 'queue :title "Queue"
                           :items (list (orpheus-item-create
                                         :label "row" :kind 'track
                                         :data '((uri . "1.mp3"))))))
                  orpheus-ui--tab 0)
            (orpheus-ui--render))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'set-window-start)
                       (lambda (_window position &optional noforce)
                         (push (list position noforce) calls))))
              (orpheus-ui--render))
            (should (equal '(1 nil) (car calls)))))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-open-grows-a-window-too-short-for-the-list ()
  "A window the cover leaves no room in is grown by the rows it lacks.
The cover block is tall in a graphical frame, and a split window can be left
showing almost none of the list."
  (with-temp-buffer
    (insert (make-string 12 ?\n))
    (insert (propertize "row" 'orpheus-item 0) "\n")
    (let ((grown nil))
      (cl-letf (((symbol-function 'window-body-height)
                 (lambda (&optional _window _pixels) 14))
                ((symbol-function 'enlarge-window)
                 (lambda (n &optional _horizontal) (setq grown n))))
        (orpheus-ui--fit-window (selected-window)))
      (should (= grown 6)))
    (let ((grown nil))
      (cl-letf (((symbol-function 'window-body-height)
                 (lambda (&optional _window _pixels) 30))
                ((symbol-function 'enlarge-window)
                 (lambda (n &optional _horizontal) (setq grown n))))
        (orpheus-ui--fit-window (selected-window)))
      (should-not grown))))

(ert-deftest orpheus-test/ui-button-answers-the-mouse ()
  "A control under the cover is a button: a click runs it, a tooltip names it."
  (let ((pressed 0)
        (button (orpheus-ui--button "▶" "Play or pause" 'orpheus-play-pause)))
    (should (string-match-p "▶" button))
    (should (equal "Play or pause" (get-text-property 0 'help-echo button)))
    (should (eq 'hand (get-text-property 0 'pointer button)))
    (cl-letf (((symbol-function 'orpheus-play-pause)
               (lambda () (interactive) (setq pressed (1+ pressed)))))
      ;; The binding is handed the click event and ignores it.
      (funcall (lookup-key (get-text-property 0 'keymap button) [mouse-1]) nil)
      (should (= 1 pressed)))))

(ert-deftest orpheus-test/ui-drops-a-listing-that-arrived-late ()
  "A slow reply from an older request cannot replace a newer list."
  (let ((orpheus-ui-buffer-name "*orpheus-test-stale*")
        (orpheus-backend-current 'fake)
        (callbacks nil))
    (unwind-protect
        (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
          (orpheus-ui-mode)
          (setq orpheus-ui--tabs
                (vector (orpheus-ui-tab-create
                         :kind 'albums :title "Albums" :view 'albums)))
          (setq orpheus-ui--tab 0)
          (cl-letf (((symbol-function 'orpheus-backend-connected-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'orpheus-backend-browse)
                     (lambda (_backend _view _arg callback)
                       (push callback callbacks))))
            (orpheus-ui--load-tab (aref orpheus-ui--tabs 0) #'ignore)
            (orpheus-ui--load-tab (aref orpheus-ui--tabs 0) #'ignore)
            (let ((newest (nth 0 callbacks))
                  (oldest (nth 1 callbacks)))
              (funcall newest (list (orpheus-item-create
                                     :kind 'track :label "new")))
              (funcall oldest (list (orpheus-item-create
                                     :kind 'track :label "old")))
              (should (equal (mapcar #'orpheus-item-label
                                     (orpheus-ui-tab-items
                                      (aref orpheus-ui--tabs 0)))
                             '("new"))))))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-layout-presets-arrange-the-sections ()
  "The layout list decides which sections are drawn and in what order."
  (let ((orpheus-ui-buffer-name "*orpheus-test-layout*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
          (orpheus-ui-mode)
          (setq orpheus-ui--tabs
                (vector (orpheus-ui-tab-create
                         :kind 'queue :title "Queue" :view 'queue
                         :items (list (orpheus-item-create
                                       :kind 'track :label "Row")))))
          (setq orpheus-ui--tab 0)
          (let ((orpheus-ui-layout '(list footer)))
            (orpheus-ui--render)
            (should (< (string-match-p "Row" (buffer-string))
                       (string-match-p "RET play" (buffer-string)))))
          (let ((orpheus-ui-layout '(footer list)))
            (orpheus-ui--render)
            (should (< (string-match-p "RET play" (buffer-string))
                       (string-match-p "Row" (buffer-string))))
            (should-not (string-match-p "Queue" (buffer-string)))))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-layout-cycle-walks-the-presets ()
  "Cycling the layout applies the next named arrangement."
  (let ((orpheus-ui-layout (alist-get 'classic orpheus-ui-layout-presets)))
    (orpheus-ui-cycle-layout)
    (should (equal orpheus-ui-layout
                   (alist-get 'cover orpheus-ui-layout-presets)))))

(ert-deftest orpheus-test/ui-transport-is-a-row-of-buttons ()
  "The transport row is previous, play, next, the toggles and stop."
  (with-temp-buffer
    ;; The newline keeps the row off buffer position 1, where a property on the
    ;; first character is not stored.
    (insert "\n" (orpheus-ui--transport nil))
    (goto-char (point-min))
    ;; In the order of the row, since the search only moves forward.
    (dolist (help '("Previous track" "Play" "Stop" "Next track"
                    "Open in another program"
                    "Repeat: off, the queue, or this track"
                    "Shuffle" "Single track" "Consume"
                    "Search the library" "The orpheus menu"))
      (should (text-property-search-forward 'help-echo help)))))

(ert-deftest orpheus-test/ui-transport-is-spaced-and-traditional ()
  "The transport is a spaced row, with stop before the modes."
  (let ((orpheus-ui-plain-ascii nil))
    (let* ((status (orpheus-status-create :state 'play :repeat t))
           (row (orpheus-ui--transport status))
           stop repeat)
      (with-temp-buffer
        (insert "\n" row)
        (goto-char (point-min))
        (text-property-search-forward 'help-echo "Stop")
        (setq stop (point))
        (goto-char (point-min))
        (text-property-search-forward 'help-echo "Repeat: off, the queue, or this track")
        (setq repeat (point)))
      (should (string-match-p " ◁◁ " row))
      (should (string-match-p " ⏸ " row))
      (should (string-match-p " ▷▷ " row))
      (should (string-match-p " ■ " row))
      (should (string-match-p " ➦ " row))
      (should (and stop repeat (< stop repeat)))))
  (let ((orpheus-ui-plain-ascii t))
    (let ((row (orpheus-ui--transport nil)))
      (should (string-match-p " << " row))
      (should (string-match-p " X " row))
      (should-not (string-match-p "\\[\\]" row)))))

(ert-deftest orpheus-test/download-progress-does-not-redraw-another-tab ()
  "A transfer update redraws the Downloads tab, not the queue behind it."
  (let ((orpheus-ui-buffer-name "*orpheus-test-download-redraw*")
        (rendered 0)
        (downloads-a (list (orpheus-item-create
                            :kind 'download
                            :data '((state . downloading) (speed . 1024)))))
        (downloads-b (list (orpheus-item-create
                            :kind 'download
                            :data '((state . downloading) (speed . 2048))))))
    (unwind-protect
        (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
          (orpheus-ui-mode)
          (setq orpheus-ui--tabs
                (vector (orpheus-ui-tab-create
                         :kind 'queue :title "Queue" :view 'queue
                         :items (list (orpheus-item-create
                                       :label "One" :kind 'track
                                       :data '((id . 1))))))
                orpheus-ui--tab 0)
          (orpheus-ui--render)
          (cl-letf (((symbol-function 'orpheus-ui--render)
                     (lambda () (setq rendered (1+ rendered)))))
            (orpheus-ui--set-downloads downloads-a)
            (should (= rendered 0)))
          (setq orpheus-ui--tabs
                (vector (orpheus-ui-tab-create :kind 'downloads
                                               :title "Downloads"
                                               :view 'downloads))
                orpheus-ui--tab 0)
          (cl-letf (((symbol-function 'orpheus-ui--render)
                     (lambda () (setq rendered (1+ rendered)))))
            (orpheus-ui--set-downloads downloads-b)
            (should (= rendered 1))))
      (when (get-buffer orpheus-ui-buffer-name)
        (kill-buffer orpheus-ui-buffer-name)))))

(ert-deftest orpheus-test/ui-seek-seconds ()
  "A click on the progress bar seeks to the column it landed on."
  (should (= -20 (orpheus-ui--seek-seconds 100 100 11 100 20)))
  (should (= 0 (orpheus-ui--seek-seconds 105 100 11 100 50)))
  (should (= 80 (orpheus-ui--seek-seconds 110 100 11 100 20))))

(ert-deftest orpheus-test/ui-now-playing-carries-the-transport ()
  "The now-playing card has the transport buttons and a bar that seeks."
  (let ((orpheus-ui-plain-ascii nil)
        (orpheus-ui--art nil)
        (orpheus-track-current nil)
        (orpheus-status-current nil))
    (with-temp-buffer
      (orpheus-ui--insert-now-playing 80)
      (goto-char (point-min))
      (should (text-property-search-forward 'help-echo "Previous track"))
      (should (text-property-search-forward 'help-echo "Next track")))))

(ert-deftest orpheus-test/ui-refresh-clears-the-invalidation ()
  "A read that happens clears the flag, so the next tick is quiet again."
  (let ((orpheus-ui-buffer-name "*orpheus-test-dirty*")
        (orpheus-ui--needs-refresh t))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs
                  (vector (orpheus-ui-tab-create :kind 'queue
                                                 :title "Queue")))
            (setq orpheus-ui--tab 0))
          (cl-letf (((symbol-function 'orpheus-ui--load-tab)
                     (lambda (_tab callback) (funcall callback))))
            (orpheus-ui-refresh))
          (should-not orpheus-ui--needs-refresh))
      (kill-buffer orpheus-ui-buffer-name))))


;;; Cover art

(ert-deftest orpheus-test/ui-art-is-remembered ()
  "A cover already fetched is shown again without asking the backend."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (calls 0)
        (orpheus-backend-current (orpheus-make-backend 'local)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-album-art)
               (lambda (_backend _track callback)
                 (setq calls (1+ calls))
                 (funcall callback "/tmp/cover.png"))))
      (let ((track (orpheus-track-create :uri "a/b.mp3")))
        (orpheus-ui--update-art track)
        (should (= calls 1))
        (should (equal (plist-get orpheus-ui--art :file) "/tmp/cover.png"))
        (setq orpheus-ui--art nil)
        (orpheus-ui--update-art track)
        (should (= calls 1))
        (should (equal (plist-get orpheus-ui--art :file) "/tmp/cover.png"))))))

(ert-deftest orpheus-test/ui-art-keeps-its-place-while-loading ()
  "While the cover is on its way the area is reserved, so the list stays put."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-backend-current (orpheus-make-backend 'local)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-album-art)
               (lambda (_backend _track _callback) nil)))
      (orpheus-ui--update-art (orpheus-track-create :uri "a/c.mp3"))
      (should (plist-get orpheus-ui--art :loading))
      (with-temp-buffer
        (orpheus-ui--insert-art 80)
        (should (string-match-p "\n" (buffer-string)))))))

(ert-deftest orpheus-test/ui-art-box-keeps-its-size ()
  "The cover area is the same size while loading and with no cover at all.
Otherwise the list below it moves up and down every time a cover arrives."
  (let ((orpheus-ui--art (list :key "k" :loading t))
        (orpheus-ui-art-size 100))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t)))
      (with-temp-buffer
        (orpheus-ui--insert-art 80)
        (let ((loading (buffer-string)))
          ;; A bare number would be a hundred line heights, not pixels, and
          ;; the list would start far below the window.
          (should (equal (get-text-property 0 'display loading)
                         '(space :width 1 :height (100))))
          (setq orpheus-ui--art (list :key "k" :file nil))
          (erase-buffer)
          (orpheus-ui--insert-art 80)
          (should (equal (buffer-string) loading)))))))

(ert-deftest orpheus-test/local-cover-detection ()
  "Cover files are found by name regardless of case and extension."
  (let* ((dir (make-temp-file "orpheus-cover" t))
         (cover (expand-file-name "Cover.WEBP" dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "01 Song.mp3" dir))
          (with-temp-file cover)
          (should (equal (orpheus-local--cover-file dir) cover))
          (let ((found 'waiting))
            (orpheus-backend-folder-art
             (orpheus-make-backend 'local) dir
             (lambda (file) (setq found file)))
            (should (orpheus-test--wait
                     (lambda () (not (eq found 'waiting))) 20))
            (should (equal found cover))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/ui-folder-art ()
  "A directory tab shows its folder cover when nothing is playing."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-track-current nil)
        (orpheus-backend-current (orpheus-make-backend 'local))
        (folder "/tmp/orpheus-album"))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-folder-art)
               (lambda (_backend dir callback)
                 (should (equal dir folder))
                 (funcall callback "/tmp/orpheus-album/cover.jpg"))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'directories :title "Directories"
                       :view 'directories :arg (list folder))))
        (orpheus-ui--update-context-art)
        (should (equal (plist-get orpheus-ui--art :file)
                       "/tmp/orpheus-album/cover.jpg"))))))

(ert-deftest orpheus-test/ui-art-falls-back-to-folder ()
  "A track without cover art falls back to the folder being browsed."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-track-current
         (orpheus-track-create :uri "/music/Album/01.mp3"))
        (orpheus-backend-current (orpheus-make-backend 'local))
        (folder "/music/Album"))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-album-art)
               (lambda (_backend _track callback) (funcall callback nil)))
              ((symbol-function 'orpheus-backend-folder-art)
               (lambda (_backend _dir callback)
                 (funcall callback "/music/Album/folder.jpg"))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'directories :title "Directories"
                       :view 'directories :arg (list folder))))
        (orpheus-ui--update-context-art)
        (should (equal (plist-get orpheus-ui--art :file)
                       "/music/Album/folder.jpg"))))))

(ert-deftest orpheus-test/ui-art-for-album-view ()
  "An MPD album folder uses its first song's embedded cover."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-track-current nil)
        (orpheus-backend-current (orpheus-make-backend 'mpd))
        (asked nil))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-album-art)
               (lambda (_backend track callback)
                 (setq asked (orpheus-track-uri track))
                 (funcall callback "/tmp/orpheus-album/cover.jpg"))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'albums :title "Albums" :view 'albums
                       :arg '("Record")
                       :items (list (orpheus-item-create
                                     :label "01.mp3" :kind 'track
                                     :data '((uri . "Music/Record/01.mp3")))))))
        (orpheus-ui--update-context-art)
        (should (equal asked "Music/Record/01.mp3"))
        (should (equal (plist-get orpheus-ui--art :file)
                       "/tmp/orpheus-album/cover.jpg"))))))

(ert-deftest orpheus-test/ui-art-is-not-fetched-in-terminal ()
  "A terminal has nowhere to draw art, so no backend work is wasted."
  (let ((orpheus-ui-show-art t)
        (orpheus-ui--art nil)
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-track-current nil)
        (orpheus-backend-current (orpheus-make-backend 'local))
        (calls 0))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) nil))
              ((symbol-function 'orpheus-ui--redraw) (lambda () nil))
              ((symbol-function 'orpheus-backend-folder-art)
               (lambda (&rest _args) (setq calls (1+ calls)))))
      (with-temp-buffer
        (orpheus-ui-mode)
        (setq orpheus-ui--tabs
              (vector (orpheus-ui-tab-create
                       :kind 'directories :title "Directories"
                       :view 'directories :arg '("/music/Album"))))
        (orpheus-ui--update-context-art)
        (should-not orpheus-ui--art)
        (should (= calls 0))))))

(ert-deftest orpheus-test/ui-art-image-rejects-non-image ()
  "A file Emacs cannot draw is skipped instead of breaking a redraw."
  (let ((file (make-temp-file "orpheus-not-an-image"))
        (orpheus-ui--art nil))
    (unwind-protect
        (progn
          (write-region "not an image" nil file nil 'silent)
          (setq orpheus-ui--art (list :file file))
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _frame) t)))
            (should-not (orpheus-ui--art-image))))
      (delete-file file))))


;;; The mode line

(ert-deftest orpheus-test/search-matches-a-cue-title ()
  "A row's track title is searchable even when the label only shows a number."
  (let ((item (orpheus-item-create
               :label "02  Second"
               :data (list (cons 'uri "/m/disc.flac#start=10.000")
                           (cons 'track (orpheus-track-create
                                         :title "Second" :artist "Band"
                                         :album "Record"))))))
    (should (orpheus-item-search-match-p item "second"))
    (should (orpheus-item-search-match-p item "band"))
    (should (orpheus-item-search-match-p item "record"))
    (should-not (orpheus-item-search-match-p item "third"))))

(ert-deftest orpheus-test/mode-line-controls ()
  "The track in the mode line answers clicks and survives silence."
  (should (lookup-key orpheus-mode-line-menu [detach]))
  (should (eq (lookup-key orpheus-mode-line-map [mode-line mouse-1]) 'orpheus))
  (should (eq (lookup-key orpheus-mode-line-map [mode-line mouse-2])
              'orpheus-play-pause))
  (should (commandp (lookup-key orpheus-mode-line-map [mode-line mouse-3])))
  (let ((orpheus-status-current
         (orpheus-status-create :state 'play :volume 50))
        (orpheus-track-current
         (orpheus-track-create :title "Song" :artist "Band"))
        (orpheus-ui-plain-ascii t))
    (let* ((string (orpheus-mode-line-string))
           (label (string-match "Band – Song" string)))
      (should label)
      (should (eq (get-text-property label 'local-map string)
                  orpheus-mode-line-map))
      (should (get-text-property label 'help-echo string))
      (should (eq (lookup-key (get-text-property 1 'local-map string)
                              [mode-line mouse-1])
                  'orpheus-previous))))
  ;; With nothing playing there is still something to click.
  (let ((orpheus-status-current nil)
        (orpheus-track-current nil)
        (orpheus-ui-plain-ascii t))
    (should (stringp (orpheus-mode-line-string)))))


(ert-deftest orpheus-test/ui-detach-button-is-clickable ()
  "The tab row offers a detach button."
  (should (eq (lookup-key orpheus-ui-detach-map [mouse-1]) 'orpheus-detach))
  (let ((orpheus-ui-plain-ascii t))
    (should (string-match-p "detach" (orpheus-ui--tab-string)))))

(ert-deftest orpheus-test/detach-opens-a-new-frame ()
  "Detaching selects a new frame and shows the player buffer there."
  (let* ((name (generate-new-buffer-name "*orpheus-detach-test*"))
         (orpheus-ui-buffer-name name)
         (frame nil)
         (selected nil)
         (buffer (get-buffer-create name)))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _frame) t))
                    ((symbol-function 'orpheus--ensure-backend) #'ignore)
                    ((symbol-function 'orpheus-start) #'ignore)
                    ((symbol-function 'orpheus-connect) #'ignore)
                    ((symbol-function 'orpheus-ui-open) #'ignore)
                    ((symbol-function 'make-frame)
                     (lambda (&optional _parameters)
                       (setq frame 'new-frame) 'new-frame))
                    ((symbol-function 'select-frame)
                     (lambda (new &optional _force) (setq selected new))))
            (orpheus-detach))
          (should (eq frame 'new-frame))
          (should (eq selected 'new-frame))
          (should (eq (window-buffer (selected-window)) buffer)))
      (when (get-buffer name) (kill-buffer name)))))

;;; The visualizer redraws a frame once

(ert-deftest orpheus-test/vis-image-is-reused ()
  "A frame that looks the same reuses the image of the frame before it."
  (let ((orpheus-vis--image nil)
        (orpheus-vis--image-key nil))
    (cl-letf (((symbol-function 'orpheus-vis--svg-image)
               (lambda (_levels _width _height) (list 'image))))
      (let ((first (orpheus-vis--image '(10 20 30 40) 120 40))
            (same (orpheus-vis--image '(11 21 30 41) 120 40))
            (other (orpheus-vis--image '(90 20 30 40) 120 40)))
        (should (eq first same))
        (should-not (eq first other))))))

(ert-deftest orpheus-test/vis-graphical-image-fills-the-window ()
  "The bars are sized in pixels, so a large font leaves no empty frame."
  (let ((buffer (get-buffer-create orpheus-vis-buffer-name))
        (captured nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer (setq orpheus-vis--data '(10 20 30)))
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _frame) t))
                    ((symbol-function 'get-buffer-window)
                     (lambda (&optional _buffer _frame) 'fake-window))
                    ((symbol-function 'window-body-width)
                     (lambda (&optional _window pixels) (if pixels 1000 100)))
                    ((symbol-function 'window-body-height)
                     (lambda (&optional _window pixels) (if pixels 700 30)))
                    ((symbol-function 'window-frame)
                     (lambda (_window) nil))
                    ((symbol-function 'frame-char-height)
                     (lambda (&optional _frame) 20))
                    ((symbol-function 'orpheus-vis--image)
                     (lambda (_levels width height)
                       (setq captured (list width height)) 'image))
                    ((symbol-function 'insert-image) #'ignore))
            (with-current-buffer buffer (orpheus-vis--render t)))
          (should (equal captured '(1000 680))))
      (kill-buffer buffer))))

(ert-deftest orpheus-test/vis-filter-marks-a-new-frame ()
  "Each complete frame marks the buffer for a redraw."
  (let ((orpheus-vis--accum "")
        (orpheus-vis--data nil)
        (orpheus-vis--dirty nil))
    (orpheus-vis--filter nil "1;2;3\n")
    (should (equal orpheus-vis--data '(1 2 3)))
    (should orpheus-vis--dirty)))


;;; One command list for a whole album

(ert-deftest orpheus-test/mpd-add-sends-one-command-list ()
  "Adding many files is one round trip, not one per file."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "command_list_end" line)
            "OK\n"
          nil))
    (let ((mpd (orpheus-test--mpd-client)))
      (unwind-protect
          (progn
            (orpheus-backend-add mpd '("a.mp3" "b.mp3"))
            (should (orpheus-test--wait
                     (lambda ()
                       (member "command_list_end"
                               orpheus-test--mpd-requests))))
            (should (equal (nreverse (cl-copy-list orpheus-test--mpd-requests))
                           '("command_list_begin" "add \"a.mp3\""
                             "add \"b.mp3\"" "command_list_end"))))
        (orpheus-backend-disconnect mpd)))))

(ert-deftest orpheus-test/mpd-add-skips-directories ()
  "MPD's `add' expands a directory, so a directory URI is never sent."
  (orpheus-test--with-fake-mpd
      (lambda (line)
        (if (string-prefix-p "command_list_end" line)
            "OK\n"
          nil))
    (let ((mpd (orpheus-test--mpd-client)))
      (unwind-protect
          (progn
            (orpheus-backend-add mpd '("Music/" "a.mp3"))
            (should (orpheus-test--wait
                     (lambda ()
                       (member "command_list_end"
                               orpheus-test--mpd-requests))))
            (should (equal (nreverse (cl-copy-list orpheus-test--mpd-requests))
                           '("command_list_begin" "add \"a.mp3\""
                             "command_list_end"))))
        (orpheus-backend-disconnect mpd)))))


;;; The SSH backend

(defun orpheus-test--with-fake-ssh (function &optional mode)
  "Call FUNCTION with ssh and sftp replaced by the suite's fake programs.
MODE selects a failure the fake ssh should simulate: `empty', `missing',
`broken', `nosize', `nocat' or `minimal'.  The cache is a fresh temporary
directory, so a test never writes to the user's own cache."
  (let ((orpheus-ssh-program
         (expand-file-name invocation-name invocation-directory))
        (orpheus-ssh-extra-args
         (list "-Q" "--batch" "-l"
               (expand-file-name
                "test/orpheus-fake-ssh.el"
                (file-name-directory orpheus-test--source))
               "--"))
        (orpheus-ssh-sftp-program
         (expand-file-name invocation-name invocation-directory))
        (orpheus-ssh-sftp-extra-args
         (list "-Q" "--batch" "-l"
               (expand-file-name
                "test/orpheus-fake-sftp.el"
                (file-name-directory orpheus-test--source))
               "--"))
        (orpheus-ssh-host "fake.invalid")
        (orpheus-cache-directory (make-temp-file "orpheus-test-cache" t))
        (process-environment
         (if mode
             (cons (concat "ORPHEUS_TEST_FAKE_SSH=" mode)
                   process-environment)
           process-environment)))
    (unwind-protect
        (funcall function)
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-backend-is-local ()
  "The SSH backend plays remote files with mpv, not with MPD."
  (should (orpheus-backend-exists-p 'ssh))
  (let ((ssh (orpheus-make-backend 'ssh)))
    (should (orpheus-local-p ssh))
    (should-not (orpheus-mpd-p ssh))
    (should (equal (orpheus-backend-capabilities ssh)
                   '(queue downloads directories search)))))

(ert-deftest orpheus-test/ssh-directory-command ()
  "A remote path is one quoted shell word, joined without doubled slashes."
  (should (equal (orpheus-ssh--join "" "Song.mp3") "Song.mp3"))
  (should (equal (orpheus-ssh--join "Music/" "Album/")
                 "Music/Album/"))
  (should (equal (orpheus-ssh--directory-command "Music/")
                 "ls -1p -- 'Music/'"))
  (should (equal (orpheus-ssh--directory-command "")
                 "ls -1p"))
  (should (equal (orpheus-ssh--shell-quote "It's/")
                 "'It'\\''s/'")))

(ert-deftest orpheus-test/ssh-cover-file ()
  "A remote cover is the best image, whatever its name or case."
  (should (equal (orpheus-ssh--cover-file
                  "Music/Album" "Cover.JPG\nSong.mp3\nnotes.txt\n")
                 "Music/Album/Cover.JPG"))
  (should (equal (orpheus-ssh--cover-file
                  "Music/Album" "band.png\nSong.mp3\n")
                 "Music/Album/band.png"))
  (should (equal (orpheus-ssh--cover-file
                  "Music/Album" "band.png\nCover.JPG\n")
                 "Music/Album/Cover.JPG"))
  (should-not (orpheus-ssh--cover-file
               "Music/Album" "Song.mp3\nnotes.txt\n")))

(ert-deftest orpheus-test/ssh-browses-remote-directories ()
  "The files tab lists the remote directory and skips non-music files."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (items nil))
       (unwind-protect
           (progn
             (orpheus-backend-browse ssh 'directories '("Music/")
                                     (lambda (result) (setq items result)))
             (should (orpheus-test--wait (lambda () items) 20))
             (should (equal (mapcar #'orpheus-item-label items)
                            '("Album/" "Song.mp3")))
             (should (equal (orpheus-item-browse (car items))
                            '(directories "Music/Album/")))
             (should (equal (alist-get 'uri
                                       (orpheus-item-data (cadr items)))
                            "Music/Song.mp3")))
         (orpheus-backend-disconnect ssh))))))

(ert-deftest orpheus-test/ssh-command-that-hangs-is-dropped ()
  "A silent remote command times out instead of loading forever."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-command-timeout 0.2)
           (result 'waiting)
           (start (float-time)))
       (orpheus-ssh--run (orpheus-make-backend 'ssh) "sleep 5"
                         (lambda (output) (setq result output)))
       (should (orpheus-test--wait (lambda () (not (eq result 'waiting))) 5))
       (should (null result))
       (should (< (- (float-time) start) 3))))))

(ert-deftest orpheus-test/ssh-fetches-folder-cover ()
  "Browsing an album fetches its cover file over ssh."
  (let ((orpheus-cache-directory (make-temp-file "orpheus-cover-cache" t)))
    (unwind-protect
        (orpheus-test--with-fake-ssh
         (lambda ()
           (let ((ssh (orpheus-make-backend 'ssh))
                 (file 'waiting))
             (unwind-protect
                 (progn
                   (orpheus-backend-folder-art
                    ssh "Music/Album"
                    (lambda (result) (setq file result)))
                   (should (orpheus-test--wait
                            (lambda () (not (eq file 'waiting))) 20))
                   (should (file-readable-p file))
                   (should (string-match-p "Cover" file)))
               (orpheus-backend-disconnect ssh)))))
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-cover-search-is-quiet ()
  "A failed cover find does not fill the echo area; the listing still answers."
  (let ((orpheus-cache-directory (make-temp-file "orpheus-cover-cache" t)))
    (unwind-protect
        (orpheus-test--with-fake-ssh
         (lambda ()
           (let ((ssh (orpheus-make-backend 'ssh))
                 (file 'waiting)
                 (messages nil))
             (unwind-protect
                 (cl-letf (((symbol-function 'message)
                            (lambda (format-string &rest args)
                              (push (apply #'format format-string args)
                                    messages))))
                   (orpheus-backend-folder-art
                    ssh "Music/Album"
                    (lambda (result) (setq file result)))
                   (should (orpheus-test--wait
                            (lambda () (not (eq file 'waiting))) 20))
                   (should (file-readable-p file))
                   (should-not
                    (cl-some (lambda (entry)
                               (string-match-p "could not run" entry))
                             messages)))
               (orpheus-backend-disconnect ssh))))
         "missing")
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-fetches-folder-cover-over-sftp ()
  "A remote cover uses sftp when the shell cannot run cat."
  (let ((orpheus-cache-directory (make-temp-file "orpheus-cover-cache" t)))
    (unwind-protect
        (orpheus-test--with-fake-ssh
         (lambda ()
           (let ((ssh (orpheus-make-backend 'ssh))
                 (file 'waiting))
             (unwind-protect
                 (progn
                   (orpheus-backend-folder-art
                    ssh "Music/Album"
                    (lambda (result) (setq file result)))
                   (should (orpheus-test--wait
                            (lambda () (not (eq file 'waiting))) 20))
                   (should (file-readable-p file))
                   (should (orpheus-ssh-instance-cat-failed ssh)))
               (orpheus-backend-disconnect ssh))))
         "nocat")
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-searches-remote-library ()
  "The search tab finds tracks below the remote start directory."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-directory "Music")
           (ssh (orpheus-make-backend 'ssh))
           (items nil))
       (unwind-protect
           (progn
             (orpheus-backend-browse ssh 'search '("song")
                                     (lambda (result) (setq items result)))
             (should (orpheus-test--wait (lambda () items) 20))
             (should (equal (mapcar #'orpheus-item-label items)
                            '("Song.mp3")))
             (should (equal (alist-get 'uri
                                       (orpheus-item-data (car items)))
                            "Music/Song.mp3"))
             (should (orpheus-item-playable (car items))))
         (orpheus-backend-disconnect ssh))))))

(ert-deftest orpheus-test/ssh-search-falls-back-to-a-recursive-listing ()
  "When find is missing, search filters a recursive ls instead."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-directory "Music")
           (ssh (orpheus-make-backend 'ssh))
           (items 'waiting)
           (messages nil))
       (unwind-protect
           (progn
             (should (string-match-p "2>/dev/null"
                                     (orpheus-ssh--search-command)))
             (cl-letf (((symbol-function 'message)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) messages))))
               (orpheus-backend-browse ssh 'search '("song")
                                       (lambda (result) (setq items result)))
               (should (orpheus-test--wait
                        (lambda () (not (eq items 'waiting))) 20)))
             (should (equal (mapcar #'orpheus-item-label items)
                            '("Song.mp3")))
             (should (equal (alist-get 'uri
                                       (orpheus-item-data (car items)))
                            "Music/Song.mp3"))
             (should (orpheus-ssh-instance-search-fallback ssh))
             (should (cl-some (lambda (entry)
                                (string-match-p "recursive" entry))
                              messages)))
         (orpheus-backend-disconnect ssh))))
   "missing"))

(ert-deftest orpheus-test/ssh-search-with-no-matches-is-not-a-failure ()
  "A successful find with no matches returns nothing without an error."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-directory "Music")
           (ssh (orpheus-make-backend 'ssh))
           (items 'waiting)
           (messages nil))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'message)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) messages))))
               (orpheus-backend-browse ssh 'search '("song")
                                       (lambda (result) (setq items result)))
               (should (orpheus-test--wait
                        (lambda () (not (eq items 'waiting))) 20)))
             (should-not items)
             (should-not (orpheus-ssh-instance-search-fallback ssh))
             (should-not (cl-some (lambda (entry)
                                    (string-match-p "could not\\|failed" entry))
                                  messages)))
         (orpheus-backend-disconnect ssh))))
   "empty"))

(ert-deftest orpheus-test/ssh-search-reports-one-failure ()
  "When find and ls both fail, repeated searches report it once."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-directory "Music")
           (ssh (orpheus-make-backend 'ssh))
           (items 'waiting)
           (messages nil))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'message)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) messages))))
               (dotimes (_ 2)
                 (setq items 'waiting)
                 (orpheus-backend-browse ssh 'search '("song")
                                         (lambda (result) (setq items result)))
                 (should (orpheus-test--wait
                          (lambda () (not (eq items 'waiting))) 20))))
             (should-not items)
             (should (orpheus-ssh-instance-search-fallback ssh))
             (should (= 1
                        (cl-count-if
                         (lambda (entry)
                           (string-match-p "search failed" entry))
                         messages))))
         (orpheus-backend-disconnect ssh))))
   "broken"))

(ert-deftest orpheus-test/ssh-download-command-is-plain-cat ()
  "A whole file is one simple cat, so a minimal remote shell can run it."
  (let ((command (orpheus-ssh--download-command
                  (orpheus-make-backend 'ssh) "Music/It's.mp3")))
    (should (equal command "cat -- 'Music/It'\\''s.mp3'"))
    (should-not (string-match-p "stat\\|ORPHEUS-SIZE" command))))

(ert-deftest orpheus-test/ssh-cuts-a-disc-track-with-ffmpeg ()
  "With ffmpeg on the host, only the track's minutes are sent."
  (let ((ssh (orpheus-make-backend 'ssh))
        (uri "Music/Disc/disc.flac#start=10.000&end=20.000"))
    (unwind-protect
        (progn
          (should (equal (orpheus-ssh--download-command ssh uri)
                         "cat -- 'Music/Disc/disc.flac'"))
          (setf (orpheus-ssh-instance-capabilities ssh) '((ffmpeg . t)))
          (let ((command (orpheus-ssh--download-command ssh uri)))
            (should (string-prefix-p "ffmpeg " command))
            (should (string-match-p
                     "-ss 10.000 -t 10.000 -i 'Music/Disc/disc.flac'" command))
            (should (string-suffix-p "-f matroska -" command)))
          ;; The copy is the track already, so nothing is left to seek.
          (should (equal (orpheus-ssh--played-path ssh uri "/cache/track.mka")
                         "/cache/track.mka"))
          ;; Without ffmpeg on the host the image is played from the range,
          ;; unless a track can be cut here, which is then the whole file.
          (setf (orpheus-ssh-instance-capabilities ssh) nil)
          (let ((orpheus-ssh-split-images nil))
            (should (equal (orpheus-ssh--played-path
                            ssh uri "/cache/disc.flac")
                           "/cache/disc.flac#start=10.000&end=20.000"))))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/ssh-whole-image-is-cached-once ()
  "Every track of a disc reuses the one copy taken without ffmpeg.
A track cut on the host or here is cached separately, because it is the track."
  (let ((orpheus-cache-directory (make-temp-file "orpheus-cache" t))
        (orpheus-ssh-split-images nil)
        (ssh (orpheus-make-backend 'ssh))
        (first "d/disc.flac#start=0.000&end=10.000")
        (second "d/disc.flac#start=10.000&end=20.000"))
    (unwind-protect
        (progn
          (should (equal (orpheus-ssh--cache-file ssh first)
                         (orpheus-ssh--cache-file ssh second)))
          (setf (orpheus-ssh-instance-capabilities ssh) '((ffmpeg . t)))
          (should-not (equal (orpheus-ssh--cache-file ssh first)
                             (orpheus-ssh--cache-file ssh second))))
      (orpheus-backend-disconnect ssh)
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-cue-sheets-turn-a-disc-into-rows ()
  "A folder's cue sheet gives its disc one row per track."
  (let* ((sheets (list (cons "Music/Disc/disc.cue"
                             (concat "TRACK 01 AUDIO\n  TITLE \"First\"\n"
                                     "  INDEX 01 00:00:00\n"
                                     "TRACK 02 AUDIO\n  TITLE \"Second\"\n"
                                     "  INDEX 01 00:10:00\n"))))
         (listing "disc.cue\ndisc.flac\nnotes.txt\n")
         (items (orpheus-ssh--directory-items "Music/Disc" listing sheets)))
    (should (equal (orpheus-ssh--cue-names listing) '("disc.cue")))
    (should (= 2 (length items)))
    (should (equal (orpheus-item-label (nth 1 items)) "02  Second"))
    (should (equal (alist-get 'uri (orpheus-item-data (nth 1 items)))
                   "Music/Disc/disc.flac#start=10.000"))
    (should (equal (orpheus-item-detail (nth 0 items)) "0:10"))
    (should (orpheus-local--known-track
             (alist-get 'uri (orpheus-item-data (nth 1 items)))))))

(ert-deftest orpheus-test/ssh-reads-each-cue-sheet-with-one-cat ()
  "A shell without loops still lists a disc's tracks, one cat per sheet."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (items 'waiting))
       (unwind-protect
           (progn
             (orpheus-backend-browse ssh 'directories '("Music/Album")
                                     (lambda (result) (setq items result)))
             (should (orpheus-test--wait
                      (lambda () (not (eq items 'waiting))) 20))
             (should (equal (mapcar #'orpheus-item-label items)
                            '("01  First" "02  Second")))
             (should (orpheus-local--known-track
                      (alist-get 'uri (orpheus-item-data (car items))))))
         (orpheus-backend-disconnect ssh))))
   "cue"))

(ert-deftest orpheus-test/ssh-search-finds-a-cue-track-title ()
  "A song title in a cue sheet is found without downloading the image."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (items 'waiting))
       (unwind-protect
           (progn
             (orpheus-ssh--search ssh "Second"
                                  (lambda (rows) (setq items rows)))
             (should (orpheus-test--wait
                      (lambda () (not (eq items 'waiting))) 20))
             (should (equal (mapcar #'orpheus-item-label items)
                            '("02  Second")))
             (should (equal (alist-get 'uri (orpheus-item-data (car items)))
                            "Music/Disc/disc.flac#start=10.000")))
         (orpheus-backend-disconnect ssh))))
   "cue-search"))

(ert-deftest orpheus-test/ssh-split-track-is-played-as-cut ()
  "A track cut here from a range is played as the file it now is."
  (skip-unless (executable-find "ffmpeg"))
  (let ((orpheus-cache-directory (make-temp-file "orpheus-cache" t))
        (ssh (orpheus-make-backend 'ssh))
        (uri "Music/Disc/disc.flac#start=10.000&end=20.000"))
    (unwind-protect
        (progn
          (should (orpheus-ssh--split-p ssh uri))
          (should (equal (orpheus-ssh--played-path
                          ssh uri (orpheus-ssh--cache-file ssh uri))
                         (orpheus-ssh--cache-file ssh uri)))
          (should (equal (orpheus-ssh--played-path
                          ssh uri (orpheus-ssh--cache-file ssh uri t))
                         (concat (orpheus-ssh--cache-file ssh uri t)
                                 "#start=10.000&end=20.000"))))
      (orpheus-backend-disconnect ssh)
      (delete-directory orpheus-cache-directory t))))

(ert-deftest orpheus-test/ssh-size-probe-reads-ls ()
  "The optional size probe is a simple ls, and its size field is read."
  (should (equal (orpheus-ssh--size-command "Music/It's.mp3")
                 "ls -lL -- 'Music/It'\\''s.mp3' 2>/dev/null"))
  (should (= (orpheus-ssh--parse-size
              "-rw-r--r-- 1 user group 1234 Nov  8 12:00 Song.mp3\n")
             1234))
  (should-not (orpheus-ssh--parse-size "ls: no such file\n")))

(ert-deftest orpheus-test/ssh-size-probe-fills-total ()
  "A successful size probe fills the transfer total."
  (let ((ssh (orpheus-make-backend 'ssh))
        (transfer (orpheus-ssh-transfer-create
                   :uri "Music/Song.mp3" :state 'downloading)))
    (cl-letf (((symbol-function 'orpheus-ssh--run)
               (lambda (_ssh _command callback &optional _quiet)
                 (funcall callback
                          "-rw-r--r-- 1 user group 2048 Nov  8 12:00 Song.mp3\n"))))
      (orpheus-ssh--request-size ssh transfer))
    (should (= (orpheus-ssh-transfer-total transfer) 2048))))

(ert-deftest orpheus-test/ssh-size-probe-failure-is-cached ()
  "A host that cannot report a size is asked only once."
  (let ((ssh (orpheus-make-backend 'ssh))
        (transfer (orpheus-ssh-transfer-create
                   :uri "Music/Song.mp3" :state 'downloading))
        (calls 0))
    (cl-letf (((symbol-function 'orpheus-ssh--run)
               (lambda (_ssh _command callback &optional _quiet)
                 (cl-incf calls)
                 (funcall callback nil))))
      (orpheus-ssh--request-size ssh transfer)
      (orpheus-ssh--request-size ssh transfer))
    (should (= calls 1))
    (should (orpheus-ssh-instance-size-failed ssh))))

(ert-deftest orpheus-test/ssh-flac-info-reads-the-seek-table ()
  "A FLAC header's rate, length and seek points are read without ffmpeg."
  (let* ((bytes (orpheus-test--flac-info-bytes
                 44100 4410000 '((0 . 0) (44100 . 5000) (88200 . 9000))))
         (info (orpheus-ssh--flac-info bytes)))
    (should (= 44100 (plist-get info :rate)))
    (should (= 4410000 (plist-get info :samples)))
    (should (> (plist-get info :meta-end) 0))
    (should (equal (plist-get info :seeks)
                   '((0 . 0) (44100 . 5000) (88200 . 9000))))
    (should (= 42 (length (orpheus-ssh--flac-header
                           (plist-get info :streaminfo)))))
    (should-not (orpheus-ssh--flac-info "not a flac"))))

(ert-deftest orpheus-test/ssh-split-plan-uses-the-nearest-seek-point ()
  "A track's range starts at the seek point before it and ends after it."
  (let* ((info (plist-put
                (orpheus-ssh--flac-info
                 (orpheus-test--flac-info-bytes
                  44100 4410000 '((0 . 0) (44100 . 5000) (88200 . 9000))))
                :size 100000))
         (plan (orpheus-ssh--split-plan info '(1.5 . 2.0))))
    (should plan)
    (should (plist-get plan :exact))
    (should (= (plist-get plan :start) (+ (plist-get info :meta-end) 5000)))
    (should (= (plist-get plan :length) 4000))
    (should (< (abs (- (plist-get plan :offset) 0.5)) 0.001))
    (should (= (plist-get plan :duration) 0.5))
    (should (equal (plist-get plan :streaminfo)
                   (plist-get info :streaminfo)))))

(ert-deftest orpheus-test/ssh-split-plan-estimates-without-a-seek-table ()
  "Without a seek table the range keeps a margin and is corrected later."
  (let* ((info (list :rate 44100 :samples 4410000 :meta-end 1000
                     :seeks nil :size 1000000
                     :streaminfo (make-string 34 0)))
         (plan (orpheus-ssh--split-plan info '(10.0 . 20.0))))
    (should plan)
    (should-not (plist-get plan :exact))
    (should-not (plist-get plan :offset))
    (should (> (plist-get plan :length) 44000))
    (should (= (plist-get plan :duration) 10.0))))

(ert-deftest orpheus-test/ssh-split-plan-runs-to-the-end ()
  "A last track with no end runs to the end of the image."
  (let* ((info (plist-put
                (orpheus-ssh--flac-info
                 (orpheus-test--flac-info-bytes
                  44100 4410000 '((0 . 0) (44100 . 5000) (88200 . 9000))))
                :size 100000))
         (plan (orpheus-ssh--split-plan info '(90.0))))
    (should plan)
    (should (= (plist-get plan :duration) 10.0))
    (should (= (plist-get plan :cue-start) 90.0))))

(ert-deftest orpheus-test/ssh-range-command-reads-only-the-track ()
  "The remote dd skips to the byte and stops at the length."
  (should (equal (orpheus-ssh--range-command "Music/Disc/disc.flac" 100 200)
                 (concat "dd if='Music/Disc/disc.flac' "
                         "iflag=skip_bytes,count_bytes bs=1M "
                         "skip=100 count=200"))))

(ert-deftest orpheus-test/ssh-cut-command-names-the-format ()
  "ffmpeg needs the format because the part file has no .flac suffix."
  (let ((command (orpheus-ssh--region-cut-command
                  "region.flac" "track.flac.part" 4.054422 477.5
                  (orpheus-track-create :title "Song" :artist "Band"
                                        :album "Record" :trackno 3))))
    (should (equal (last command 3) '("-f" "flac" "track.flac.part")))
    (should (member "title=Song" command))
    (should (member "track=3" command))))

(ert-deftest orpheus-test/ssh-download-rows-show-their-state ()
  "A transfer becomes a row with its state, size and URI."
  (let* ((ssh (orpheus-make-backend 'ssh))
         (transfer (orpheus-ssh-transfer-create
                    :uri "Music/Song.mp3" :state 'downloading :bytes 2048)))
    (setf (orpheus-ssh-instance-transfers ssh) (list transfer))
    (unwind-protect
        (let ((row (car (orpheus-ssh--download-items ssh))))
          (should (equal (orpheus-item-label row) "Song.mp3"))
          (should (string-prefix-p "downloading" (orpheus-item-detail row)))
          (should (equal (alist-get 'state (orpheus-item-data row))
                         'downloading))
          (should (equal (alist-get 'uri (orpheus-item-data row))
                         "Music/Song.mp3")))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/ssh-transfer-speed-uses-recent-samples ()
  "The speed reflects the last few seconds, not the whole transfer."
  (let* ((now (float-time))
         (transfer (orpheus-ssh-transfer-create
                    :state 'downloading :bytes 2048
                    :started (- now 10)
                    :samples (list (cons now 2048)
                                   (cons (- now 2) 1024)))))
    (should (= (round (orpheus-ssh--transfer-speed transfer)) 512))))

(ert-deftest orpheus-test/ssh-transfer-records-speed-samples ()
  "Each received chunk records a byte sample for the rolling speed."
  (let* ((ssh (orpheus-make-backend 'ssh))
         (path (make-temp-file "orpheus-sample-"))
         (transfer (orpheus-ssh-transfer-create
                    :uri "Music/Song.mp3" :path path
                    :state 'downloading)))
    (unwind-protect
        (progn
          (orpheus-ssh--transfer-filter ssh transfer "abc")
          (should (= (cdr (car (orpheus-ssh-transfer-samples transfer))) 3))
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (orpheus-ssh--part-file path))
                           (buffer-string))
                         "abc")))
      (dolist (file (list path (orpheus-ssh--part-file path)))
        (when (file-exists-p file) (delete-file file)))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/ssh-download-rows-show-progress-and-eta ()
  "A downloading row reports its completion, speed and ETA."
  (let* ((ssh (orpheus-make-backend 'ssh))
         (now (float-time))
         (transfer (orpheus-ssh-transfer-create
                    :uri "Music/Song.mp3" :state 'downloading
                    :bytes 1024 :total 3072
                    :started (- now 10)
                    :samples (list (cons now 1024)
                                   (cons (- now 2) 0)))))
    (setf (orpheus-ssh-instance-transfers ssh) (list transfer))
    (unwind-protect
        (let* ((row (car (orpheus-ssh--download-items ssh)))
               (data (orpheus-item-data row))
               (detail (orpheus-item-detail row)))
          (should (string-match-p "33%" detail))
          (should (string-match-p "512B/s" detail))
          (should (string-match-p "ETA 0:04" detail))
          (should (= (alist-get 'total data) 3072))
          (should (= (alist-get 'percent data) 33))
          (should (= (alist-get 'speed data) 512))
          (should (= (alist-get 'eta data) 4)))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/ssh-download-without-size-probe-still-works ()
  "A failed ls size probe does not stop the cat download."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (path nil))
       (unwind-protect
           (progn
             (orpheus-ssh--download
              ssh "Music/Song.mp3"
              (lambda (result) (setq path result)))
             (should (orpheus-test--wait (lambda () path) 20))
             (should (file-readable-p path))
             (should (equal (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string))
                            "fake audio\n"))
             (let ((transfer (orpheus-ssh--transfer-for-uri
                              ssh "Music/Song.mp3")))
               (should (eq (orpheus-ssh-transfer-state transfer) 'done))
               (should (= (orpheus-ssh-transfer-bytes transfer) 11))))
         (orpheus-backend-disconnect ssh))))
   "nosize"))

(ert-deftest orpheus-test/ssh-cancel-removes-a-partial-copy ()
  "Cancelling a transfer deletes the bytes it had already written."
  (let* ((ssh (orpheus-make-backend 'ssh))
         (partial (make-temp-file "orpheus-partial-"))
         (transfer (orpheus-ssh-transfer-create
                    :uri "Music/Song.mp3" :path partial :state 'downloading)))
    (setf (orpheus-ssh-instance-transfers ssh) (list transfer))
    (unwind-protect
        (progn
          (orpheus-ssh--cancel ssh "Music/Song.mp3")
          (should-not (file-exists-p partial))
          (should-not (orpheus-ssh--transfer-for-uri ssh "Music/Song.mp3")))
      (when (file-exists-p partial) (delete-file partial))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/local-searches-files-asynchronously ()
  "The local backend finds tracks with find, without blocking."
  (let* ((root (file-name-as-directory
                (make-temp-file "orpheus-search-" t)))
         (song (expand-file-name "Song.mp3" root))
         (notes (expand-file-name "notes.txt" root))
         (orpheus-local-directory root)
         (items 'waiting))
    (unwind-protect
        (progn
          (write-region "audio" nil song)
          (write-region "notes" nil notes)
          (orpheus-local--search "song"
                                 (lambda (result) (setq items result)))
          (should (orpheus-test--wait
                   (lambda () (not (eq items 'waiting))) 20))
          (should (equal (mapcar #'orpheus-item-label items)
                         '("Song.mp3")))
          (should (equal (alist-get 'uri (orpheus-item-data (car items)))
                         song)))
      (delete-directory root t))))

(ert-deftest orpheus-test/ssh-plays-a-downloaded-copy ()
  "Playing a remote file downloads it, loads the copy, and cleans up."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (sent nil)
           (loaded nil))
       (setf (orpheus-local-instance-queue ssh) (list "Music/Song.mp3"))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'orpheus-local--send)
                        (lambda (_local command &optional _callback)
                          (push command sent))))
               (orpheus-local--play-index ssh 0)
               (should (orpheus-test--wait
                        (lambda ()
                          (setq loaded (cl-find "loadfile" sent
                                                :key #'car :test #'equal)))
                        20))
               (should (file-readable-p (cadr loaded)))
               (should (equal (with-temp-buffer
                                (insert-file-contents (cadr loaded))
                                (buffer-string))
                              "fake audio\n"))
               (let ((transfer (orpheus-ssh--transfer-for-uri
                                ssh "Music/Song.mp3")))
                 (should transfer)
                 (should (= (orpheus-ssh-transfer-total transfer) 11))
                 (should (= (orpheus-ssh-transfer-bytes transfer) 11)))
               (should (eq (orpheus-status-state
                            (orpheus-local-instance-status ssh))
                           'play))))
         (orpheus-backend-disconnect ssh))
       ;; The cache survives a disconnect, so replaying the track is free.
       (should (file-exists-p (cadr loaded)))))))

(ert-deftest orpheus-test/ssh-does-not-claim-playback-while-downloading ()
  "A pending copy leaves the player stopped until mpv can open it."
  (let ((ssh (orpheus-make-backend 'ssh)))
    (setf (orpheus-local-instance-queue ssh) (list "Music/Song.mp3"))
    (cl-letf (((symbol-function 'orpheus-local--resolve-uri)
               (lambda (_backend _uri _callback) nil)))
      (orpheus-local--play-index ssh 0)
      (should (equal (orpheus-track-uri
                      (orpheus-local-instance-track ssh))
                     "Music/Song.mp3"))
      (should-not (eq (orpheus-status-state
                       (orpheus-local-instance-status ssh))
                      'play)))))

(ert-deftest orpheus-test/ssh-without-a-host-says-so ()
  "Without `orpheus-ssh-host' no mpv is started and the message says why."
  (let ((orpheus-ssh-host nil)
        (reached 'none)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (orpheus-backend-connect (orpheus-make-backend 'ssh)
                               (lambda (backend) (setq reached backend)))
      (should-not reached)
      (should (cl-some (lambda (text)
                         (string-match-p "orpheus-ssh-host" text))
                       messages)))))

(ert-deftest orpheus-test/ssh-capabilities-are-probed-once ()
  "The capability check runs once and later calls use its answer."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (first 'waiting)
           (second 'waiting))
       (unwind-protect
           (progn
             (orpheus-ssh--capabilities
              ssh (lambda (result) (setq first result)))
             (should (orpheus-test--wait
                      (lambda () (not (eq first 'waiting))) 20))
             (let ((calls 0))
               (cl-letf (((symbol-function 'orpheus-ssh--run)
                          (lambda (&rest _args) (cl-incf calls))))
                 (orpheus-ssh--capabilities
                  ssh (lambda (result) (setq second result))))
               (should (= calls 0)))
             (should (equal first second))
             (should (alist-get 'cat first)))
         (orpheus-backend-disconnect ssh))))))

(ert-deftest orpheus-test/ssh-capabilities-report-missing-shell-features ()
  "The probe records every missing command and shell feature."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (capabilities 'waiting))
       (unwind-protect
           (progn
             (orpheus-ssh--capabilities
              ssh (lambda (result) (setq capabilities result)))
             (should (orpheus-test--wait
                      (lambda () (not (eq capabilities 'waiting))) 20))
             (dolist (key '(cat find size substitution compound))
               (should-not (alist-get key capabilities)))
             (should (orpheus-ssh-instance-cat-failed ssh))
             (should (orpheus-ssh-instance-size-failed ssh))
             (should (orpheus-ssh-instance-search-fallback ssh))
             (should (string-match-p
                      "downloads use sftp"
                      (orpheus-ssh--capability-summary capabilities))))
         (orpheus-backend-disconnect ssh))))
   "minimal"))

(ert-deftest orpheus-test/ssh-connect-checks-capabilities-once ()
  "Connecting asks for the check and reports its answer only once."
  (let ((orpheus-ssh-host "fake.invalid")
        (ssh (orpheus-make-backend 'ssh))
        (probes 0)
        (messages nil)
        (connected 0)
        (capabilities '((cat . t) (find . t) (size . t)
                        (substitution . t) (compound . t))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'orpheus-local--ensure)
                     (lambda (_backend callback) (funcall callback ssh)))
                    ((symbol-function 'orpheus-ssh--capabilities)
                     (lambda (_backend callback)
                       (cl-incf probes)
                       (funcall callback capabilities)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (dotimes (_ 2)
              (orpheus-backend-connect
               ssh (lambda (backend) (when backend (cl-incf connected))))))
          (should (= probes 2))
          (should (= connected 2))
          (should (= (length messages) 1))
          (should (string-match-p "cat yes" (car messages))))
      (orpheus-backend-disconnect ssh))))

(ert-deftest orpheus-test/ssh-diagnose-shows-capabilities ()
  "The diagnostic buffer lists the remote commands and shell features."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-backend-current nil)
           (buffer nil))
       (cl-letf (((symbol-function 'display-buffer)
                  (lambda (candidate &rest _args)
                    (setq buffer candidate)
                    candidate)))
         (orpheus-ssh-diagnose)
         (should (orpheus-test--wait
                  (lambda ()
                    (and buffer
                         (with-current-buffer buffer
                           (not (string-match-p "Checking" (buffer-string))))))
                  20))
         (with-current-buffer buffer
           (should (string-match-p "cat" (buffer-string)))
           (should (string-match-p "command substitution" (buffer-string))))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest orpheus-test/ssh-sftp-quotes-batch-paths ()
  "An sftp batch path is quoted and special glob characters are escaped."
  (should (equal (orpheus-ssh--sftp-quote "Music/It's \"x\"?.mp3")
                 "\"Music/It's \\\"x\\\"\\?.mp3\""))
  (should (equal (orpheus-ssh--sftp-batch "Music/Song.mp3" "/tmp/Song.mp3")
                 "get \"Music/Song.mp3\" \"/tmp/Song.mp3\"\n"))
  (should-error (orpheus-ssh--sftp-batch "bad\nname" "/tmp/Song.mp3")))

(ert-deftest orpheus-test/ssh-sftp-fallback-when-cat-is-rejected ()
  "A shell without cat gets the track over sftp instead."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (path nil))
       (unwind-protect
           (progn
             (orpheus-ssh--download
              ssh "Music/Song.mp3"
              (lambda (result) (setq path result)))
             (should (orpheus-test--wait (lambda () path) 20))
             (should (equal (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string))
                            "sftp bytes\n"))
             (should (orpheus-ssh-instance-cat-failed ssh))
             (let ((transfer (orpheus-ssh--transfer-for-uri
                              ssh "Music/Song.mp3")))
               (should (orpheus-ssh-transfer-retried transfer))
               (should (= (orpheus-ssh-transfer-bytes transfer) 11))))
         (orpheus-backend-disconnect ssh))))
   "nocat"))

(ert-deftest orpheus-test/ssh-sftp-used-when-cat-is-known-missing ()
  "A cached cat failure chooses sftp before starting a process."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((ssh (orpheus-make-backend 'ssh))
           (path nil))
       (setf (orpheus-ssh-instance-cat-failed ssh) t)
       (unwind-protect
           (progn
             (orpheus-ssh--download
              ssh "Music/Song.mp3"
              (lambda (result) (setq path result)))
             (should (orpheus-test--wait (lambda () path) 20))
             (should (equal (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string))
                            "sftp bytes\n")))
         (orpheus-backend-disconnect ssh))))))

;;; New playlists, the menu and the cached interface

(ert-deftest orpheus-test/playlist-create-chooses-the-directory ()
  "A new playlist is written where the player was told to keep it."
  (let* ((storage (make-temp-file "orpheus-playlists" t))
         (music (make-temp-file "orpheus-music" t))
         (orpheus-playlist-directory storage)
         (orpheus-playlist-new-directory music))
    (unwind-protect
        (progn
          (let ((file (orpheus-playlist-create "Night Drive" music)))
            (should (file-readable-p file))
            (should (equal (file-name-directory file)
                           (file-name-as-directory
                            (expand-file-name music))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "#EXTM3U\n"))))
          (orpheus-playlist-write (orpheus-playlist-file "Other") nil)
          (should (equal (mapcar #'file-name-base (orpheus-playlist-files))
                         '("Night Drive" "Other")))
          (should (equal (file-name-base (orpheus-playlist-find "Night Drive"))
                         "Night Drive")))
      (delete-directory storage t)
      (delete-directory music t))))

(ert-deftest orpheus-test/playlist-create-refuses-an-existing-name ()
  "Creating a playlist never overwrites one that is already there."
  (let ((directory (make-temp-file "orpheus-playlists" t)))
    (unwind-protect
        (progn
          (orpheus-playlist-create "Mix" directory)
          (should-error (orpheus-playlist-create "Mix" directory)))
      (delete-directory directory t))))

(ert-deftest orpheus-test/ui-new-playlist-opens-the-editor ()
  "The N command creates the file and opens it, so an empty playlist is usable."
  (let ((directory (make-temp-file "orpheus-playlists" t))
        (edited nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'orpheus-playlist-edit)
                     (lambda (file) (setq edited file))))
            (orpheus-ui-new-playlist "Mix" directory))
          (should edited)
          (should (file-readable-p edited)))
      (delete-directory directory t))))

(ert-deftest orpheus-test/ui-save-playlist-can-choose-the-directory ()
  "Saving a new queue playlist writes it where the player was told."
  (let ((directory (make-temp-file "orpheus-music" t))
        (orpheus-backend-current 'fake)
        (items (list (orpheus-item-create
                      :kind 'track :data '((uri . "/a.mp3"))))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'orpheus-backend-queue)
                     (lambda (_backend callback) (funcall callback items))))
            (orpheus-ui-save-playlist "Mix" directory))
          (should (equal (file-name-directory (orpheus-playlist-file "Mix" directory))
                         (file-name-as-directory (expand-file-name directory))))
          (should (file-readable-p (orpheus-playlist-file "Mix" directory))))
      (delete-directory directory t))))

(ert-deftest orpheus-test/ui-menu-keys ()
  "The menu is on ?, the whole table on H, and N makes a playlist."
  (should (eq (lookup-key orpheus-ui-mode-map (kbd "?")) 'orpheus-menu))
  (should (eq (lookup-key orpheus-ui-mode-map (kbd "H")) 'orpheus-help))
  (should (eq (lookup-key orpheus-ui-mode-map (kbd "E")) 'orpheus-equalizer-menu))
  (should (eq (lookup-key orpheus-ui-mode-map (kbd "N"))
              'orpheus-ui-new-playlist))
  (should (eq (lookup-key orpheus-ui-mode-map (kbd "B"))
              'orpheus-switch-backend)))

(ert-deftest orpheus-test/ui-bindings-are-customizable ()
  "A (KEY . COMMAND) setting rebinds the player without a hook."
  (let ((orpheus-ui-mode-bindings '(("C-c o" . orpheus-ui-first-line))))
    (unwind-protect
        (progn
          (orpheus-ui--apply-bindings)
          (should (eq (lookup-key orpheus-ui-mode-map (kbd "C-c o"))
                      'orpheus-ui-first-line)))
      (define-key orpheus-ui-mode-map (kbd "C-c o") nil))))

(ert-deftest orpheus-test/ui-open-reuses-the-selected-window ()
  "Opening the player passes the same-window action to `pop-to-buffer'."
  (let ((orpheus-ui-buffer-name
         (generate-new-buffer-name "*orpheus-open-test*"))
        (orpheus-ui-display-action '(display-buffer-same-window))
        (action 'unset))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &optional action-argument)
                       (setq action action-argument)
                       (set-window-buffer (selected-window) buffer)))
                    ((symbol-function 'orpheus-ui--fit-window) #'ignore)
                    ((symbol-function 'orpheus-ui--update-context-art) #'ignore)
                    ((symbol-function 'orpheus-ui-refresh) #'ignore))
            (orpheus-ui-open))
          (should (equal action '(display-buffer-same-window))))
      (when (get-buffer orpheus-ui-buffer-name)
        (kill-buffer orpheus-ui-buffer-name)))))

(ert-deftest orpheus-test/ui-tab-is-reused-within-the-cache-window ()
  "A tab loaded a moment ago is shown again without asking the backend."
  (let ((orpheus-ui-buffer-name
         (generate-new-buffer-name "*orpheus-cache-test*"))
        (orpheus-ui-cache-seconds 30)
        (orpheus-ui--needs-refresh nil)
        (loads 0)
        (tab (orpheus-ui-tab-create :kind 'queue :title "Queue"
                                    :loaded t :loaded-at (float-time))))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs (vector tab)
                  orpheus-ui--tab 0))
          (cl-letf (((symbol-function 'orpheus-ui--load-tab)
                     (lambda (_tab callback)
                       (setq loads (1+ loads))
                       (funcall callback))))
            (orpheus-ui-refresh)
            (should (= loads 0))
            (setf (orpheus-ui-tab-loaded-at tab) 0)
            (orpheus-ui-refresh)
            (should (= loads 1))))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-library-change-invalidates-every-tab ()
  "A library update forgets cached rows in every tab, not just the visible one."
  (let ((orpheus-ui-buffer-name
         (generate-new-buffer-name "*orpheus-cache-invalidation*"))
        (first (orpheus-ui-tab-create :kind 'queue :title "Queue"
                                      :loaded t :loaded-at (float-time)))
        (second (orpheus-ui-tab-create :kind 'directories :title "Directories"
                                       :loaded t :loaded-at (float-time)))
        (orpheus-ui--art-cache (make-hash-table :test #'equal))
        (orpheus-ui--image-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash "song" "cover.jpg" orpheus-ui--art-cache)
          (puthash "cover" 'image orpheus-ui--image-cache)
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs (vector first second)
                  orpheus-ui--tab 0
                  orpheus-ui--needs-refresh nil)
            (orpheus-ui-invalidate-cache)
            (should-not (orpheus-ui-tab-loaded first))
            (should-not (orpheus-ui-tab-loaded second))
            (should (= 0 (orpheus-ui-tab-loaded-at first)))
            (should (= 0 (orpheus-ui-tab-loaded-at second)))
            (should (= 0 (hash-table-count orpheus-ui--art-cache)))
            (should (= 0 (hash-table-count orpheus-ui--image-cache)))
            (should orpheus-ui--needs-refresh)))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-library-changed-hook-clears-tabs ()
  "The player listens for backend library changes."
  (let ((orpheus-ui-buffer-name
         (generate-new-buffer-name "*orpheus-library-hook*"))
        (tab (orpheus-ui-tab-create :kind 'directories :title "Directories"
                                    :loaded t :loaded-at (float-time))))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode)
            (setq orpheus-ui--tabs (vector tab)
                  orpheus-ui--tab 0
                  orpheus-ui--needs-refresh nil)
            (run-hooks 'orpheus-backend-library-changed-hook)
            (should-not (orpheus-ui-tab-loaded tab))
            (should orpheus-ui--needs-refresh)))
      (kill-buffer orpheus-ui-buffer-name))))

(ert-deftest orpheus-test/ui-database-commands-invalidate-the-tab-cache ()
  "Updating or rescanning the library drops rows cached in every tab."
  (let ((actions nil)
        (invalidated 0))
    (cl-letf (((symbol-function 'orpheus--command)
               (lambda (action &optional _value) (push action actions)))
              ((symbol-function 'orpheus-ui-invalidate-cache)
               (lambda () (setq invalidated (1+ invalidated)))))
      (orpheus-update-database)
      (orpheus-rescan-database))
    (should (equal (nreverse actions) '(update rescan)))
    (should (= invalidated 2))))

(ert-deftest orpheus-test/ui-card-height-is-stable ()
  "The card keeps its line count when the first track starts."
  (let ((orpheus-ui-show-art nil)
        (orpheus-ui-show-now-playing t)
        (orpheus-ui--art nil)
        (orpheus-track-current nil)
        (orpheus-status-current nil))
    (with-temp-buffer
      (orpheus-ui-mode)
      (setq orpheus-ui--tabs
            (vector (orpheus-ui-tab-create
                     :kind 'queue :title "Queue"
                     :items (list (orpheus-item-create
                                   :label "row" :kind 'track
                                   :data '((uri . "a.mp3"))))))
            orpheus-ui--tab 0)
      (orpheus-ui--render)
      (let ((before (count-lines (point-min) (point-max))))
        (setq orpheus-track-current
              (orpheus-track-create :title "Song" :artist "Band")
              orpheus-status-current
              (orpheus-status-create :state 'play :elapsed 1 :total 10))
        (orpheus-ui--render)
        (should (= before (count-lines (point-min) (point-max))))))))

;;; The cache, its size limit and the artwork search

(defun orpheus-test--cached-file (directory name age)
  "Write NAME into DIRECTORY with AGE seconds of age and return its path."
  (let ((path (expand-file-name name directory)))
    (with-temp-file path (insert "0123456789"))
    (set-file-times path
                    (time-subtract (current-time) (seconds-to-time age)))
    path))

(ert-deftest orpheus-test/cache-prune-removes-the-oldest-files ()
  "The cache is trimmed to `orpheus-cache-max-bytes', oldest file first."
  (let* ((directory (make-temp-file "orpheus-cache" t))
         (orpheus-cache-directory directory)
         (orpheus-cache-max-bytes 15))
    (unwind-protect
        (let ((old (orpheus-test--cached-file directory "old" 3600))
              (new (orpheus-test--cached-file directory "new" 60)))
          (should (= (orpheus-cache-size) 20))
          (should (= (orpheus-cache-prune) 10))
          (should-not (file-exists-p old))
          (should (file-exists-p new))
          (should (= (orpheus-cache-size) 10)))
      (delete-directory directory t))))

(ert-deftest orpheus-test/cache-prune-keeps-the-file-in-use ()
  "A file named in KEEP survives even when it is the oldest one."
  (let* ((directory (make-temp-file "orpheus-cache" t))
         (orpheus-cache-directory directory)
         (orpheus-cache-max-bytes 5))
    (unwind-protect
        (let ((playing (orpheus-test--cached-file directory "playing" 3600))
              (other (orpheus-test--cached-file directory "other" 60)))
          (orpheus-cache-prune (list playing))
          (should (file-exists-p playing))
          (should-not (file-exists-p other)))
      (delete-directory directory t))))

(ert-deftest orpheus-test/cache-prune-skips-partial-downloads ()
  "A file still being written is never counted or deleted."
  (let* ((directory (make-temp-file "orpheus-cache" t))
         (orpheus-cache-directory directory)
         (orpheus-cache-max-bytes 0)
         (part (expand-file-name "track.mp3.part" directory)))
    (unwind-protect
        (progn
          (with-temp-file part (insert "half a track"))
          (should (= (orpheus-cache-size) 0))
          (should-not (orpheus-cache-prune))
          (should (file-exists-p part)))
      (delete-directory directory t))))

(ert-deftest orpheus-test/art-best-prefers-the-track-folder ()
  "A cover next to the track wins over the parent and a child folder."
  (let ((dir "/music/Album/Disc 1"))
    (should (equal (orpheus-local--art-best
                    dir '("/music/Album/cover.jpg"
                          "/music/Album/Disc 1/front.jpg"))
                   "/music/Album/Disc 1/front.jpg"))
    (should (equal (orpheus-local--art-best
                    dir '("/music/Album/cover.jpg"))
                   "/music/Album/cover.jpg"))
    (should (equal (orpheus-local--art-best
                    dir '("/music/Album/Disc 1/Scans/front.jpg"))
                   "/music/Album/Disc 1/Scans/front.jpg"))
    ;; A sibling album's cover is not this album's cover.
    (should-not (orpheus-local--art-best dir '("/music/Other/cover.jpg")))
    ;; The order of `orpheus-local-cover-names' decides within a folder.
    (should (equal (orpheus-local--art-best
                    "/music/Album"
                    '("/music/Album/folder.jpg" "/music/Album/cover.jpg"))
                   "/music/Album/cover.jpg"))
    ;; Any image is a cover, but a named one still wins over it.
    (should (equal (orpheus-local--art-best
                    "/music/Album" '("/music/Album/scan.jpg"))
                   "/music/Album/scan.jpg"))
    (should (equal (orpheus-local--art-best
                    "/music/Album"
                    '("/music/Album/scan.jpg" "/music/Album/cover.jpg"))
                   "/music/Album/cover.jpg"))
    (should-not (orpheus-local--art-best
                 "/music/Album" '("/music/Album/notes.txt")))))

(ert-deftest orpheus-test/local-art-scan-finds-parent-and-child-covers ()
  "The asynchronous scan looks in the album root and in child folders."
  (let* ((root (make-temp-file "orpheus-art" t))
         (album (expand-file-name "Album" root))
         (disc (expand-file-name "Disc 1" album))
         (scans (expand-file-name "Scans" disc))
         (parent-cover (expand-file-name "cover.jpg" album))
         (child-cover (expand-file-name "front.jpg" scans)))
    (make-directory scans t)
    (unwind-protect
        (progn
          (with-temp-file parent-cover (insert "jpg"))
          (let ((found 'waiting))
            (orpheus-local--scan-art disc (lambda (path) (setq found path)))
            (should (orpheus-test--wait
                     (lambda () (not (eq found 'waiting))) 20))
            (should (equal found parent-cover)))
          (delete-file parent-cover)
          (with-temp-file child-cover (insert "jpg"))
          (let ((found 'waiting))
            (orpheus-local--scan-art disc (lambda (path) (setq found path)))
            (should (orpheus-test--wait
                     (lambda () (not (eq found 'waiting))) 20))
            (should (equal found child-cover))))
      (delete-directory root t))))

(ert-deftest orpheus-test/local-art-scan-skips-the-windows-find ()
  "Windows ships find.exe, not GNU find; art uses the Emacs fallback."
  (let* ((dir (make-temp-file "orpheus-art-win" t))
         (cover (expand-file-name "cover.jpg" dir))
         (found 'waiting)
         (started nil))
    (unwind-protect
        (progn
          (with-temp-file cover (insert "jpg"))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "C:/Windows/System32/find.exe"))
                    ((symbol-function 'make-process)
                     (lambda (&rest _) (setq started t) nil)))
            (let ((system-type 'windows-nt))
              (orpheus-local--scan-art
               dir (lambda (path) (setq found path)))))
          (should-not started)
          (should (equal found cover)))
      (dolist (buffer (buffer-list))
        (when (equal (buffer-name buffer) " *orpheus-art*")
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest orpheus-test/local-file-answers-for-open-externally ()
  "A local track is its own file; a stream has none."
  (let ((file (make-temp-file "orpheus-track-"))
        (local (orpheus-make-backend 'local)))
    (unwind-protect
        (progn
          (should (equal (orpheus-backend-local-file
                          local (orpheus-track-create :uri file))
                         file))
          (should-not (orpheus-backend-local-file
                       local (orpheus-track-create
                              :uri "https://example.org/a.mp3"))))
      (delete-file file))))

(ert-deftest orpheus-test/cycle-repeat-goes-through-three-states ()
  "`r' moves from off to the queue to this track and back."
  (let ((orpheus-backend-current (orpheus-make-backend 'local))
        (commands nil)
        (messages nil))
    (cl-letf (((symbol-function 'orpheus-backend-connected-p)
               (lambda (_backend) t))
              ((symbol-function 'orpheus-backend-command)
               (lambda (_backend action &optional value)
                 (push (cons action value) commands)))
              ((symbol-function 'orpheus--tick) (lambda () nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (setq orpheus-status-current (orpheus-status-create :state 'play))
      (orpheus-cycle-repeat)
      (should (equal (assq 'repeat commands) '(repeat . t)))
      (should-not (cdr (assq 'single commands)))
      (setq commands nil
            orpheus-status-current
            (orpheus-status-create :state 'play :repeat t))
      (orpheus-cycle-repeat)
      (should (equal (assq 'single commands) '(single . t)))
      (setq commands nil
            orpheus-status-current
            (orpheus-status-create :state 'play :repeat t :single t))
      (orpheus-cycle-repeat)
      (should (= 2 (length commands)))
      (should-not (cdr (assq 'repeat commands)))
      (should-not (cdr (assq 'single commands)))
      (should (string-match-p "off" (car messages))))))

(ert-deftest orpheus-test/transport-uses-the-media-icons ()
  "The card is one row of plain media glyphs, with an ASCII fallback."
  (let ((orpheus-ui-plain-ascii nil)
        (status (orpheus-status-create :state 'stop)))
    (let ((row (orpheus-ui--transport status)))
      (dolist (glyph '("◁◁" "▶" "■" "▷▷" "➦" "⇆" "⤮" "⌕" "⁈"))
        (should (string-match-p (regexp-quote glyph) row)))))
  (let ((orpheus-ui-plain-ascii t)
        (status (orpheus-status-create :state 'play)))
    (let ((row (orpheus-ui--transport status)))
      (should (string-match-p "<<" row))
      (should (string-match-p "||" row))
      (should-not (string-match-p "◁◁" row)))))

(ert-deftest orpheus-test/ui-clears-the-list-when-entering-a-folder ()
  "The old rows go at once, so a slow link still shows that RET worked."
  (let* ((orpheus-backend-current (orpheus-make-backend 'local))
         (orpheus-ui-show-art nil)
         (orpheus-ui-show-now-playing nil)
         (tab (orpheus-ui-tab-create
               :kind 'directories :title "Directories" :view 'directories
               :arg '("/music")
               :items (list (orpheus-item-create
                             :label "Album/" :kind 'directory
                             :data '((browse . (directories "/music/Album"))
                                     (uri . "/music/Album")))))))
    (with-temp-buffer
      (orpheus-ui-mode)
      (setq orpheus-ui--tabs (vector tab) orpheus-ui--tab 0)
      (orpheus-ui--render)
      (orpheus-ui--place-point)
      (cl-letf (((symbol-function 'orpheus-backend-connected-p)
                 (lambda (_backend) t))
                ((symbol-function 'orpheus-backend-browse)
                 (lambda (&rest _args) nil)))
        (orpheus-ui-confirm))
      (should-not (orpheus-ui-tab-items tab))
      (should (orpheus-ui-tab-loading tab))
      (should (equal (orpheus-ui-tab-arg tab) '("/music/Album"))))))

(ert-deftest orpheus-test/ssh-cover-command-searches-around-the-folder ()
  "One remote find covers the folder, its parent and every image name."
  (let ((command (orpheus-ssh--cover-command "Music/Album/Disc 1")))
    (should (string-match-p "find 'Music/Album/Disc 1/' 'Music/Album/'" command))
    (should (string-match-p "-maxdepth 2" command))
    ;; The parentheses must be shell words, or a remote sh stops at them.
    (should (string-match-p "'('" command))
    (should (string-match-p "'-iname' '\\*\\.jpg'" command))
    (should-not (string-match-p "'-iname' 'cover\\.jpg'" command))))

(ert-deftest orpheus-test/ssh-control-args-share-one-connection ()
  "Control commands share a master connection; downloads do not use it."
  (let ((orpheus-ssh-host "fake.invalid")
        (orpheus-ssh-multiplex t)
        (orpheus-ssh--control-directory nil)
        (orpheus-ssh--control-owned nil))
    (unwind-protect
        (let* ((args (orpheus-ssh--control-args))
               (path (cl-find-if (lambda (arg)
                                   (string-prefix-p "ControlPath=" arg))
                                 args)))
          (should (member "ControlMaster=auto" args))
          (should (member "ControlPersist=600" args))
          (should path)
          (should (<= (orpheus-ssh--control-path-length
                       orpheus-ssh--control-directory)
                      orpheus-ssh--control-path-limit))
          (should (file-directory-p orpheus-ssh--control-directory)))
      (let ((directory orpheus-ssh--control-directory))
        (when directory (ignore-errors (delete-directory directory t))))
      (setq orpheus-ssh--control-directory nil)))
  (let ((orpheus-ssh-host "fake.invalid")
        (orpheus-ssh-multiplex nil))
    (should-not (orpheus-ssh--control-args))))

(ert-deftest orpheus-test/ssh-control-path-fits-a-long-temporary-directory ()
  "A long TMPDIR does not push the control socket past the platform limit."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let ((orpheus-ssh-host "fake.invalid")
        (orpheus-ssh-multiplex t)
        (orpheus-ssh-control-directory nil)
        (orpheus-ssh--control-directory nil)
        (orpheus-ssh--control-owned nil)
        (orpheus-ssh--control-warned nil)
        (temporary-file-directory
         (expand-file-name
          "a-temporary-directory-that-is-far-too-long-for-a-control-socket/"
          temporary-file-directory))
        (process-environment (cons "XDG_RUNTIME_DIR=" process-environment)))
    (unwind-protect
        (let* ((args (orpheus-ssh--control-args))
               (path (cl-find-if (lambda (arg)
                                   (string-prefix-p "ControlPath=" arg))
                                 args)))
          (should path)
          (should (<= (orpheus-ssh--control-path-length
                       orpheus-ssh--control-directory)
                      orpheus-ssh--control-path-limit))
          (should (file-directory-p orpheus-ssh--control-directory)))
      (let ((directory orpheus-ssh--control-directory))
        (when directory (ignore-errors (delete-directory directory t))))
      (setq orpheus-ssh--control-directory nil))))

(ert-deftest orpheus-test/ssh-control-directory-and-lifetime-are-configurable ()
  "A chosen control directory is used, has room, and is left alone on close."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let* ((directory (make-temp-file "orpheus-short-" t))
         (orpheus-ssh-host "fake.invalid")
         (orpheus-ssh-multiplex t)
         (orpheus-ssh-control-directory directory)
         (orpheus-ssh-control-persist 30)
         (orpheus-ssh--control-directory nil)
         (orpheus-ssh--control-owned t))
    (unwind-protect
        (let* ((args (orpheus-ssh--control-args))
               (path (cl-find-if (lambda (arg)
                                   (string-prefix-p "ControlPath=" arg))
                                 args)))
          (should (member "ControlPersist=30" args))
          (should (equal orpheus-ssh--control-directory directory))
          (should (equal (file-name-directory (substring path (length "ControlPath=")))
                         (file-name-as-directory directory)))
          (should-not orpheus-ssh--control-owned))
      (delete-directory directory t))))

(ert-deftest orpheus-test/ssh-control-errors-are-recognized ()
  "A socket path failure is told apart from a login or permission failure."
  (should (orpheus-ssh--control-error-p "ControlPath too long"))
  (should (orpheus-ssh--control-error-p
           "unix_listener: path too long for Unix domain socket"))
  (should-not (orpheus-ssh--control-error-p
               "Permission denied (publickey).")))

(ert-deftest orpheus-test/ssh-control-failure-retries-without-multiplexing ()
  "A host that rejects the control socket still answers the command."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((orpheus-ssh-multiplex t)
           (ssh (orpheus-make-backend 'ssh))
           (result 'waiting)
           (warned nil))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'message)
                        (lambda (format &rest arguments)
                          (when (string-match-p "multiplex" format)
                            (setq warned t))
                          (apply #'format-message format arguments))))
               (orpheus-ssh--run ssh "ls -1p"
                                 (lambda (output) (setq result output)))
               (should (orpheus-test--wait
                        (lambda () (not (eq result 'waiting))) 20)))
             (should (equal result "Album/\nSong.mp3\nnotes.txt\n"))
             (should (orpheus-ssh-instance-control-failed ssh))
             (should warned)
             (should-not (orpheus-ssh--control-args ssh)))
         (orpheus-backend-disconnect ssh))))
   "nomux"))

(ert-deftest orpheus-test/switch-backend-disconnects-and-rebuilds-the-player ()
  "Switching stops the old backend, starts the new one and rebuilds the tabs."
  (let ((orpheus-backend 'local)
        (orpheus-backend-current (orpheus-make-backend 'local))
        (orpheus--timer nil)
        (orpheus--failures 3)
        (orpheus--retry-at 100)
        (orpheus-ui-buffer-name
         (generate-new-buffer-name "*orpheus-switch-test*"))
        (disconnected nil)
        (connected nil)
        (started 0)
        (refreshed nil))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create orpheus-ui-buffer-name)
            (orpheus-ui-mode))
          (cl-letf (((symbol-function 'orpheus-backend-disconnect)
                     (lambda (backend)
                       (push (orpheus-backend-name backend) disconnected)))
                    ((symbol-function 'orpheus-backend-connect)
                     (lambda (backend callback)
                       (setq connected (orpheus-backend-name backend))
                       (funcall callback backend)))
                    ((symbol-function 'orpheus-start)
                     (lambda () (cl-incf started)))
                    ((symbol-function 'orpheus--tick) #'ignore)
                    ((symbol-function 'orpheus-ui-refresh)
                     (lambda (&optional force) (setq refreshed force)))
                    ((symbol-function 'orpheus-ui--render) #'ignore))
            (should (eq (orpheus-switch-backend 'mpd) 'mpd)))
          (should (eq orpheus-backend 'mpd))
          (should (eq (orpheus-backend-name orpheus-backend-current) 'mpd))
          (should (memq 'local disconnected))
          (should (eq connected 'mpd))
          (should (= started 1))
          (should refreshed)
          (should (= orpheus--failures 0))
          (with-current-buffer orpheus-ui-buffer-name
            (should (eq orpheus-ui--backend orpheus-backend-current))
            (should (memq 'artists
                          (mapcar #'orpheus-ui-tab-kind
                                  (append orpheus-ui--tabs nil))))))
      (when (get-buffer orpheus-ui-buffer-name)
        (kill-buffer orpheus-ui-buffer-name)))))

(ert-deftest orpheus-test/switch-backend-refuses-an-unknown-name ()
  "A typo or a backend that is not registered changes nothing."
  (let ((orpheus-backend 'local)
        (orpheus-backend-current (orpheus-make-backend 'local)))
    (should-error (orpheus-switch-backend 'nonsense))
    (should (eq orpheus-backend 'local))))

(ert-deftest orpheus-test/ssh-cache-survives-a-restart ()
  "A track fetched in one session is played from the cache in the next."
  (orpheus-test--with-fake-ssh
   (lambda ()
     (let ((first (orpheus-make-backend 'ssh))
           (path 'waiting)
           (second nil)
           (found 'waiting))
       (unwind-protect
           (progn
             (orpheus-ssh--download first "Music/Song.mp3"
                                    (lambda (result) (setq path result)))
             (should (orpheus-test--wait
                      (lambda () (not (eq path 'waiting))) 20))
             (orpheus-backend-disconnect first)
             (should (file-exists-p path))
             (setq second (orpheus-make-backend 'ssh)
                   orpheus-ssh-program "no-such-ssh-program")
             (orpheus-ssh--download second "Music/Song.mp3"
                                    (lambda (result) (setq found result)))
             (should (equal found path)))
         (when second (orpheus-backend-disconnect second)))))))

;;; The equalizer editor

(ert-deftest orpheus-test/equalizer-filter-uses-the-width ()
  "The band width in octaves reaches the filter string."
  (let ((orpheus-equalizer-bands '(1000))
        (orpheus-equalizer-width 2))
    (should (equal (orpheus-equalizer--filter '(3))
                   "equalizer=frequency=1000:width_type=o:width=2:gain=3"))))

(ert-deftest orpheus-test/equalizer-adjust-keeps-the-other-bands ()
  "Adjusting one band leaves the others alone and respects the limit."
  (let ((orpheus-equalizer-bands '(100 1000))
        (orpheus-equalizer-max-gain 6))
    (should (equal (orpheus-equalizer-adjust '(0 0) 1 3) '(0 3)))
    (should (equal (orpheus-equalizer-adjust '(0 5) 1 4) '(0 6)))))

(ert-deftest orpheus-test/equalizer-invert-and-tilt ()
  "Invert flips the curve and tilt lifts the bass, not the treble."
  (let ((orpheus-equalizer-bands '(100 1000 10000))
        (orpheus-equalizer-max-gain 12))
    (should (equal (orpheus-equalizer-invert-gains '(2 0 -3)) '(-2 0 3)))
    (let ((tilted (orpheus-equalizer-tilt-gains '(0 0 0))))
      (should (> (car tilted) 0))
      (should (< (car (last tilted)) 0)))))

(ert-deftest orpheus-test/equalizer-bar-draws-the-gain ()
  "The bar fills one column per dB, on the side the gain points to."
  (let ((orpheus-equalizer-max-gain 4))
    (should (= 3 (cl-count ?█ (orpheus-equalizer--bar-string 3))))
    (should (= 2 (cl-count ?█ (orpheus-equalizer--bar-string -2))))
    (should (string-match-p "│" (orpheus-equalizer--bar-string 0)))))

(ert-deftest orpheus-test/equalizer-state-roundtrip ()
  "The curve, its preset and the switch survive a restart."
  (let* ((dir (make-temp-file "orpheus-eq-state" t))
         (orpheus-equalizer-state-file (expand-file-name "state.json" dir))
         (orpheus-equalizer-bands '(100 1000))
         (orpheus-equalizer-enabled t)
         (orpheus-equalizer-preset "mine")
         (orpheus-equalizer--gains '(2 -3)))
    (unwind-protect
        (progn
          (orpheus-equalizer--write-state)
          (setq orpheus-equalizer-enabled nil
                orpheus-equalizer-preset "flat"
                orpheus-equalizer--gains nil)
          (orpheus-equalizer--load-state)
          (should orpheus-equalizer-enabled)
          (should (equal orpheus-equalizer-preset "mine"))
          (should (equal orpheus-equalizer--gains '(2 -3))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-restore-keeps-the-curve ()
  "A backend starting later is given the remembered curve."
  (let* ((dir (make-temp-file "orpheus-eq-state" t))
         (orpheus-equalizer-state-file (expand-file-name "state.json" dir))
         (orpheus-equalizer-bands '(100 1000))
         (orpheus-equalizer--gains nil)
         (orpheus-equalizer-enabled nil)
         (orpheus-backend-current (make-orpheus-mpd :name 'mpd)))
    (unwind-protect
        (progn
          (with-temp-file orpheus-equalizer-state-file
            (insert "{\"enabled\":true,\"preset\":\"mine\",\"gains\":[1,-2]}"))
          (orpheus-equalizer-restore)
          (should orpheus-equalizer-enabled)
          (should (equal orpheus-equalizer--gains '(1 -2))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-export-and-import ()
  "A preset written to a file comes back with the same gains."
  (let* ((dir (make-temp-file "orpheus-eq-share" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir))
         (file (expand-file-name "shared.json" dir)))
    (unwind-protect
        (progn
          (orpheus-equalizer-export-preset "bass" file)
          (orpheus-equalizer-import-preset file)
          (should (equal (cdr (assoc "bass" (orpheus-equalizer--read-presets)))
                         (cdr (assoc "bass" orpheus-equalizer-builtin-presets)))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-import-bare-array-names-the-preset ()
  "A bare array of gains takes the file's own name."
  (let* ((dir (make-temp-file "orpheus-eq-share" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir))
         (file (expand-file-name "warm.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "[1,2,3,4,5,6,7,8,9,10]"))
          (orpheus-equalizer-import-preset file)
          (should (equal (cdr (assoc "warm" (orpheus-equalizer--read-presets)))
                         '(1 2 3 4 5 6 7 8 9 10))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-import-refuses-a-broken-file ()
  "A preset file that does not fit the bands is an error, not silence."
  (let* ((dir (make-temp-file "orpheus-eq-share" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir))
         (file (expand-file-name "broken.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"broken\":[1,2]}"))
          (should-error (orpheus-equalizer-import-preset file)
                        :type 'user-error))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-copy-preset ()
  "Copying a preset puts JSON on the kill ring."
  (let ((text (orpheus-equalizer-copy-preset "flat")))
    (should (string-match-p "\"flat\"" text))
    (should (equal text (car kill-ring)))))

(ert-deftest orpheus-test/equalizer-sparkline-draws-one-glyph-per-band ()
  "A preset's preview has a glyph for each band, and silence has its own."
  (let ((orpheus-equalizer-max-gain 12))
    (should (= 10 (length (orpheus-equalizer--sparkline
                           '(0 0 0 0 0 0 0 0 0 0)))))
    (should (string-match-p "·" (orpheus-equalizer--sparkline
                                 '(0 0 0 0 0 0 0 0 0 0))))
    (should-not (string-match-p "·" (orpheus-equalizer--sparkline
                                     '(12 12 12 12 12 12 12 12 12 12))))))

(ert-deftest orpheus-test/equalizer-rename-preset ()
  "Renaming keeps the gains and follows the current preset's name."
  (let* ((dir (make-temp-file "orpheus-eq" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir))
         (orpheus-equalizer-state-file (expand-file-name "state.json" dir))
         (orpheus-equalizer-preset "old"))
    (unwind-protect
        (progn
          (orpheus-equalizer--write-presets '(("old" . (1 2 3 4 5 6 7 8 9 10))))
          (orpheus-equalizer-rename-preset "old" "new")
          (should (equal (cdr (assoc "new" (orpheus-equalizer--read-presets)))
                         '(1 2 3 4 5 6 7 8 9 10)))
          (should-not (assoc "old" (orpheus-equalizer--read-presets)))
          (should (equal orpheus-equalizer-preset "new")))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-rename-refuses-a-collision ()
  "Renaming onto another saved preset is refused instead of replacing it."
  (let* ((dir (make-temp-file "orpheus-eq" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir)))
    (unwind-protect
        (progn
          (orpheus-equalizer--write-presets
           '(("one" . (1 1 1 1 1 1 1 1 1 1))
             ("two" . (2 2 2 2 2 2 2 2 2 2))))
          (should-error (orpheus-equalizer-rename-preset "one" "two")
                        :type 'user-error)
          (should (equal (cdr (assoc "two" (orpheus-equalizer--read-presets)))
                         '(2 2 2 2 2 2 2 2 2 2))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/equalizer-preset-manager-lists-the-saved-ones ()
  "The manager shows your presets and their preview, not the built-ins."
  (let* ((dir (make-temp-file "orpheus-eq" t))
         (orpheus-equalizer-preset-file (expand-file-name "saved.json" dir))
         (orpheus-equalizer-state-file (expand-file-name "state.json" dir)))
    (unwind-protect
        (progn
          (orpheus-equalizer--write-presets
           '(("warm" . (1 2 3 4 5 6 7 8 9 10))))
          (orpheus-equalizer-preset-manager)
          (with-current-buffer orpheus-equalizer-presets-buffer-name
            (should (string-match-p "warm" (buffer-string)))
            (should-not (string-match-p "loudness" (buffer-string)))
            (should (derived-mode-p 'orpheus-equalizer-presets-mode))))
      (when (get-buffer orpheus-equalizer-presets-buffer-name)
        (kill-buffer orpheus-equalizer-presets-buffer-name))
      (delete-directory dir t))))

;;; Lyrics

(ert-deftest orpheus-test/lyrics-parse-lrc ()
  "LRC times become seconds, and one line may carry several times."
  (should (equal (orpheus-lyrics--parse
                  "[ar:Band]\n[00:01.50]Hello\n[00:12.00][00:20.00]Chorus\n")
                 '((1.5 . "Hello") (12.0 . "Chorus") (20.0 . "Chorus")))))

(ert-deftest orpheus-test/lyrics-parse-offset-and-plain ()
  "An LRC offset shifts every time; text without times stays plain."
  (should (equal (orpheus-lyrics--parse "[offset:+500]\n[00:02.00]Late")
                 '((1.5 . "Late"))))
  (should (equal (orpheus-lyrics--parse "one\n\ntwo\n")
                 '((nil . "one") (nil . "") (nil . "two")))))

(ert-deftest orpheus-test/lyrics-index-at ()
  "The current line is the last one whose time has come."
  (let ((lines '((1.0 . "a") (5.0 . "b") (9.0 . "c"))))
    (should (= 1 (orpheus-lyrics--index-at lines 6)))
    (should (= 0 (orpheus-lyrics--index-at lines 2)))
    (should-not (orpheus-lyrics--index-at lines 0.5))))

(ert-deftest orpheus-test/lyrics-sidecar-prefers-lrc ()
  "A sidecar is found beside the track, and .lrc beats .txt."
  (let* ((dir (make-temp-file "orpheus-lyrics" t))
         (song (expand-file-name "Song.mp3" dir)))
    (unwind-protect
        (progn
          (with-temp-file song (insert "not really audio"))
          (with-temp-file (expand-file-name "Song.txt" dir) (insert "plain"))
          (with-temp-file (expand-file-name "Song.LRC" dir)
            (insert "[00:01.00]hi"))
          (should (equal (orpheus-lyrics--sidecar song)
                         (expand-file-name "Song.LRC" dir))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/lyrics-from-file ()
  "A track reads the lyrics in the file beside it."
  (let* ((dir (make-temp-file "orpheus-lyrics" t))
         (song (expand-file-name "a.flac" dir)))
    (unwind-protect
        (progn
          (with-temp-file song (insert ""))
          (with-temp-file (expand-file-name "a.lrc" dir)
            (insert "[00:01.00]hi"))
          (let ((lyrics (orpheus-lyrics--from-file song)))
            (should (orpheus-lyrics-p lyrics))
            (should (orpheus-lyrics-synced lyrics))
            (should (equal (orpheus-lyrics-lines lyrics) '((1.0 . "hi"))))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/lyrics-from-tags ()
  "A daemon's plain lyrics tag is read, with no packing rules invented."
  (let ((lyrics (orpheus-lyrics--from-tags
                 '((file . "a.mp3") (unsyncedlyrics . "one\ntwo")))))
    (should (orpheus-lyrics-p lyrics))
    (should-not (orpheus-lyrics-synced lyrics))
    (should (equal (orpheus-lyrics-lines lyrics)
                   '((nil . "one") (nil . "two"))))))

(ert-deftest orpheus-test/lyrics-come-from-the-local-backend ()
  "The local backend answers with the sidecar next to the file."
  (let* ((dir (make-temp-file "orpheus-lyrics" t))
         (song (expand-file-name "Song.mp3" dir))
         (answer 'waiting))
    (unwind-protect
        (progn
          (with-temp-file song (insert ""))
          (with-temp-file (expand-file-name "Song.lrc" dir)
            (insert "[00:01.00]hi"))
          (orpheus-backend-lyrics
           (make-orpheus-local :name 'local)
           (orpheus-track-create :uri song)
           (lambda (lyrics) (setq answer lyrics)))
          (should (orpheus-lyrics-p answer))
          (should (equal (orpheus-lyrics-lines answer) '((1.0 . "hi")))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/lyrics-come-from-the-mpd-library-root ()
  "MPD resolves a library path under its configured local root."
  (let* ((dir (make-temp-file "orpheus-lyrics" t))
         (orpheus-mpd-music-directory dir)
         (answer 'waiting))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Song.mp3" dir) (insert ""))
          (with-temp-file (expand-file-name "Song.lrc" dir)
            (insert "[00:01.00]hi"))
          (orpheus-backend-lyrics
           (make-orpheus-mpd :name 'mpd)
           (orpheus-track-create :uri "Song.mp3")
           (lambda (lyrics) (setq answer lyrics)))
          (should (orpheus-lyrics-p answer))
          (should (equal (orpheus-lyrics-lines answer) '((1.0 . "hi")))))
      (delete-directory dir t))))

(ert-deftest orpheus-test/ssh-lyrics-command-quotes-the-paths ()
  "The remote sidecar command never builds a word out of raw data."
  (let ((command (orpheus-ssh--lyrics-command "/Music/a b.mp3")))
    (should (string-match-p "'/Music/a b.lrc'" command))
    (should (string-match-p "'/Music/a b.txt'" command))))

;;; orpheus-test.el ends here
