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
                  orpheus-directory-face orpheus-error-face))
    (should (facep face))))

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
  "MPD can browse everything; local files at least the filesystem."
  (let ((mpd (orpheus-make-backend 'mpd))
        (local (orpheus-make-backend 'local)))
    (dolist (kind '(queue directories artists albumartists albums playlists
                    search))
      (should (memq kind (orpheus-backend-capabilities mpd))))
    (should (memq 'queue (orpheus-backend-capabilities local)))
    (should (memq 'directories (orpheus-backend-capabilities local)))))


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
    (should (not (string-match-p "pipewire" mac)))))

(ert-deftest orpheus-test/vis-input-for-platform ()
  "Linux captures pipewire; macOS falls back to portaudio."
  (let ((orpheus-vis-input nil))
    (let ((system-type 'gnu/linux)) (should (eq (orpheus-vis--input) 'pipewire)))
    (let ((system-type 'darwin)) (should (eq (orpheus-vis--input) 'portaudio)))
    (let ((system-type 'windows-nt)) (should (eq (orpheus-vis--input) 'wasapi))))
  (let ((orpheus-vis-input 'fifo))
    (should (eq (orpheus-vis--input) 'fifo))))

(ert-deftest orpheus-test/vis-text-graph ()
  "The text graph is as many rows as it is tall, and one column per bar."
  (let ((graph (orpheus-vis--text '(10 50 100 0) 4 3)))
    (should (= 3 (length (split-string graph "\n"))))
    (dolist (row (split-string graph "\n"))
      (should (= 4 (length row))))))

(ert-deftest orpheus-test/vis-svg ()
  "The SVG renderer makes an image, and colours follow the level."
  (should (imagep (orpheus-vis--svg-image '(10 50 100) 120 40)))
  (should (equal (car orpheus-vis-colors) (orpheus-vis--color 0)))
  (should (equal (car (last orpheus-vis-colors)) (orpheus-vis--color 100))))


;;; The interface, without a frame

(ert-deftest orpheus-test/ui-tabs ()
  "The tabs follow what the backend says it can do, in rmpc's order."
  (should (equal (mapcar #'car
                         (orpheus-ui--tabs-for-capabilities
                          '(queue directories artists albumartists albums
                            playlists search)))
                 '(queue directories artists albumartists albums playlists
                   search)))
  (should (equal (mapcar #'car
                         (orpheus-ui--tabs-for-capabilities
                          '(queue directories)))
                 '(queue directories)))
  (should (equal (mapcar #'car (orpheus-ui--tabs-for-capabilities nil))
                 '(queue))))

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


;;; Helpers shared by the UI and the backends

(ert-deftest orpheus-test/browse-kinds-are-known ()
  "Every browse kind has a tab title and a key."
  (dolist (kind '(queue directories artists albumartists albums playlists
                  search))
    (should (orpheus-ui-kind-title kind))
    ;; The kind is documented in the backend protocol, so it must be reachable
    ;; from a method that handles `orpheus-backend-browse'.
    (should (memq kind orpheus-backend-browse-kinds))))

(ert-deftest orpheus-test/readme-options-exist ()
  "Every option the configuration tables name is really a user option."
  (let* ((readme (expand-file-name
                  "README.org"
                  (file-name-directory orpheus-test--source)))
         (text (with-temp-buffer
                 (insert-file-contents readme)
                 (buffer-string)))
         (start (string-match "^\\* Configuration" text))
         (end (string-match "^\\* Backends" text))
         (named nil))
    (should start)
    (should end)
    (let ((section (substring text start end))
          (position 0))
      ;; Only the rows of the tables, whose first column is an option name.
      (while (string-match "^| `\\(orpheus-[a-z0-9-]+\\)`" section position)
        (push (intern (match-string 1 section)) named)
        (setq position (match-end 0))))
    (should (> (length named) 20))
    (dolist (symbol named)
      (should (custom-variable-p symbol)))))

(ert-deftest orpheus-test/readme-symbols-exist ()
  "Every `orpheus-…' symbol the README names in backticks is defined.
Only the public ones: the double dash marks helpers and the one hypothetical
function in the extension example.  The four value types are named as types,
so they are allowed by name."
  (let* ((readme (expand-file-name
                  "README.org"
                  (file-name-directory orpheus-test--source)))
         (text (with-temp-buffer
                 (insert-file-contents readme)
                 (buffer-string)))
         (position 0)
         (checked 0)
         (types '(orpheus-track orpheus-status orpheus-item)))
    (while (string-match "`\\(orpheus-[a-z0-9-]*[a-z0-9]\\)`" text position)
      (let ((symbol (intern (match-string 1 text))))
        (setq position (match-end 0))
        (unless (string-prefix-p "orpheus--" (symbol-name symbol))
          (setq checked (1+ checked))
          (should (or (boundp symbol) (fboundp symbol)
                      (memq symbol types))))))
    (should (> checked 30))))

;;; orpheus-test.el ends here
