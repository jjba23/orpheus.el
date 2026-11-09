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
         (orpheus-equalizer-preset-file file)
         (orpheus-equalizer-enabled nil)
         (orpheus-equalizer--gains nil)
         (applied nil))
    (unwind-protect
        (progn
          (delete-file file)
          (orpheus-equalizer--write-presets
           '(("mine" . (1 2 3 4 5 6 7 8 9 10))))
          (cl-letf (((symbol-function 'orpheus-equalizer--apply)
                     (lambda () (setq applied t))))
            (orpheus-equalizer-load-preset "mine"))
          (should applied)
          (should (equal orpheus-equalizer--gains
                         '(1 2 3 4 5 6 7 8 9 10)))
          (should (equal orpheus-equalizer-preset "mine"))
          (should orpheus-equalizer-enabled))
      (ignore-errors (delete-file file)))))

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
  "The SVG renderer draws when it can, and returns nil when it cannot."
  (should (equal (car orpheus-vis-colors) (orpheus-vis--color 0)))
  (should (equal (car (last orpheus-vis-colors)) (orpheus-vis--color 100)))
  (when (image-type-available-p 'svg)
    (should (imagep (orpheus-vis--svg-image '(10 50 100) 120 40))))
  ;; An Emacs built without SVG must not signal once per frame.
  (cl-letf (((symbol-function 'image-type-available-p) #'ignore))
    (should-not (orpheus-vis--svg-image '(10 50 100) 120 40))))

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
        (types '(orpheus-track orpheus-status orpheus-item)))
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

(ert-deftest orpheus-test/ui-transport-is-a-row-of-buttons ()
  "The transport row is previous, play, next, the toggles and stop."
  (with-temp-buffer
    ;; The newline keeps the row off buffer position 1, where a property on the
    ;; first character is not stored.
    (insert "\n" (orpheus-ui--transport nil))
    (goto-char (point-min))
    (dolist (help '("Previous track" "Play" "Next track" "Repeat"
                    "Shuffle" "Single track" "Consume" "Stop"))
      (should (text-property-search-forward 'help-echo help)))))

(ert-deftest orpheus-test/ui-transport-is-spaced-and-traditional ()
  "The transport is a spaced row with stop beside the other transport keys."
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
        (text-property-search-forward 'help-echo "Repeat")
        (setq repeat (point)))
      (should (string-match-p "  ⏮  " row))
      (should (string-match-p "  ⏸  " row))
      (should (string-match-p "  ⏭  " row))
      (should (string-match-p "  ⏹  " row))
      (should (and stop repeat (< stop repeat)))))
  (let ((orpheus-ui-plain-ascii t))
    (let ((row (orpheus-ui--transport nil)))
      (should (string-match-p "  <<  " row))
      (should (string-match-p "  X  " row))
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

(ert-deftest orpheus-test/mode-line-controls ()
  "The track in the mode line answers clicks and survives silence."
  (should (eq (lookup-key orpheus-mode-line-map [mode-line mouse-1]) 'orpheus))
  (should (eq (lookup-key orpheus-mode-line-map [mode-line mouse-2])
              'orpheus-play-pause))
  (should (commandp (lookup-key orpheus-mode-line-map [mode-line mouse-3])))
  (let ((orpheus-status-current
         (orpheus-status-create :state 'play :volume 50))
        (orpheus-track-current
         (orpheus-track-create :title "Song" :artist "Band"))
        (orpheus-ui-plain-ascii t))
    (let ((string (orpheus-mode-line-string)))
      (should (string-match-p "Band – Song" string))
      (should (eq (get-text-property 0 'local-map string)
                  orpheus-mode-line-map))
      (should (get-text-property 0 'help-echo string))))
  ;; With nothing playing there is still something to click.
  (let ((orpheus-status-current nil)
        (orpheus-track-current nil)
        (orpheus-ui-plain-ascii t))
    (should (stringp (orpheus-mode-line-string)))))


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
`broken', `nosize', `nocat' or `minimal'."
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
        (process-environment
         (if mode
             (cons (concat "ORPHEUS_TEST_FAKE_SSH=" mode)
                   process-environment)
           process-environment)))
    (funcall function)))

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
  "A remote cover is found regardless of its case."
  (should (equal (orpheus-ssh--cover-file
                  "Music/Album" "Cover.JPG\nSong.mp3\nnotes.txt\n")
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
  "The download is a simple cat, so a minimal remote shell can run it."
  (let ((command (orpheus-ssh--download-command "Music/It's.mp3")))
    (should (equal command "cat -- 'Music/It'\\''s.mp3'"))
    (should-not (string-match-p "stat\\|ORPHEUS-SIZE" command))))

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
                           (insert-file-contents path)
                           (buffer-string))
                         "abc")))
      (when (file-exists-p path) (delete-file path))
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
       (should-not (file-exists-p (cadr loaded)))))))

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
              'orpheus-ui-new-playlist)))

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

;;; orpheus-test.el ends here
