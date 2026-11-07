;;; orpheus-integration-test.el --- orpheus against real players -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The unit suite in orpheus-test.el needs nothing but Emacs.  This file is
;; the other half: it starts a real MPD and a real mpv, in a temporary
;; directory, on a socket of its own, and plays two generated tracks through
;; them.  Everything it finds is skipped when the programs are missing, so a
;; machine without mpd or mpv still runs it and still passes.
;;
;; Run it with:
;;
;;   emacs -Q --batch -L . -L test -l test/orpheus-integration-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; or with `maak integration'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'orpheus)

(defconst orpheus-integration--dir
  (file-name-as-directory
   (make-temp-file "orpheus-integration" t))
  "Directory the fixtures and both players live in.")

(defvar orpheus-integration--mpd nil
  "The MPD process the MPD test started.")

(defvar orpheus-integration--cleanup nil
  "Functions to run once the fixture is no longer needed.")

(defun orpheus-integration--wait (predicate &optional timeout)
  "Wait until PREDICATE holds, or TIMEOUT seconds pass, and return its value."
  (let ((deadline (+ (float-time) (or timeout 15))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun orpheus-integration--make-track (file title artist album track)
  "Write a short tone to FILE tagged with TITLE, ARTIST, ALBUM and TRACK."
  (when (zerop (call-process
                "ffmpeg" nil nil nil
                "-v" "error" "-y"
                "-f" "lavfi" "-i" "sine=frequency=440:duration=3"
                "-metadata" (concat "title=" title)
                "-metadata" (concat "artist=" artist)
                "-metadata" (concat "album=" album)
                "-metadata" (concat "track=" track)
                "-metadata" "date=2026"
                "-id3v2_version" "3"
                file))
    file))

(defun orpheus-integration--fixture ()
  "Build a music directory with two tagged tracks and a cover, once."
  (let ((music (expand-file-name "music/" orpheus-integration--dir))
        (cover (expand-file-name "music/cover.png" orpheus-integration--dir)))
    (unless (file-exists-p cover)
      (make-directory music t)
      (orpheus-integration--make-track (expand-file-name "one.mp3" music)
                                       "Integration One" "Orpheus"
                                       "Integration Album" "1")
      (orpheus-integration--make-track (expand-file-name "two.mp3" music)
                                       "Integration Two" "Orpheus"
                                       "Integration Album" "2")
      (call-process "ffmpeg" nil nil nil
                    "-v" "error" "-y"
                    "-f" "lavfi" "-i" "color=c=red:s=32x32:d=1"
                    "-frames:v" "1" cover))))

(defun orpheus-integration--programs-p (&rest programs)
  "Return non-nil when every one of PROGRAMS can be found."
  (cl-every #'executable-find programs))


;;; MPD

(defun orpheus-integration--start-mpd ()
  "Start MPD on a socket inside the fixture directory."
  (orpheus-integration--fixture)
  (let ((socket (expand-file-name "socket" orpheus-integration--dir))
        (conf (expand-file-name "mpd.conf" orpheus-integration--dir)))
    (with-temp-file conf
      (insert (format "music_directory \"%smusic\"
playlist_directory \"%splaylists\"
db_file \"%stag_cache\"
log_file \"%smpd.log\"
pid_file \"%smpd.pid\"
state_file \"%sstate\"
sticker_file \"%ssticker.sql\"
auto_update \"yes\"
bind_to_address \"%s\"
audio_output {
    type \"null\"
    name \"null\"
}
"
                      orpheus-integration--dir orpheus-integration--dir
                      orpheus-integration--dir orpheus-integration--dir
                      orpheus-integration--dir orpheus-integration--dir
                      orpheus-integration--dir socket)))
    (make-directory (expand-file-name "playlists" orpheus-integration--dir) t)
    ;; `--no-daemon' keeps MPD a child of this Emacs, so killing the process
    ;; really stops the daemon instead of leaving it behind.
    (setq orpheus-integration--mpd
          (make-process :name "orpheus-integration-mpd"
                        :command (list (executable-find "mpd") "--no-daemon"
                                       conf)
                        :buffer nil :noquery t
                        :stderr (get-buffer-create "*orpheus integration mpd*")))
    (unless (orpheus-integration--wait (lambda () (file-exists-p socket)) 10)
      (error "MPD did not open its socket: %s"
             (with-current-buffer "*orpheus integration mpd*" (buffer-string))))
    socket))

(defun orpheus-integration--stop-mpd ()
  "Stop the MPD this file started, however it was started."
  (let ((pid-file (expand-file-name "mpd.pid" orpheus-integration--dir)))
    (when (file-readable-p pid-file)
      (let ((pid (string-to-number
                  (string-trim (with-temp-buffer
                                 (insert-file-contents pid-file)
                                 (buffer-string))))))
        (when (> pid 0)
          (ignore-errors (signal-process pid 'TERM)))))))

(ert-deftest orpheus-test/integration-mpd ()
  "A real MPD serves status, tags, browsing, queue, art and playlists."
  (skip-unless (orpheus-integration--programs-p "mpd" "ffmpeg"))
  (let* ((socket (orpheus-integration--start-mpd))
         (orpheus-cache-directory (expand-file-name "cache/"
                                                    orpheus-integration--dir))
         (mpd (make-orpheus-mpd :name 'mpd :socket socket))
         (orpheus-backend-current mpd)
         (connected nil)
         (status nil)
         (track nil)
         (queue nil))
    (unwind-protect
        (progn
          (orpheus-backend-connect mpd (lambda (backend)
                                         (setq connected (and backend t))))
          (should (orpheus-integration--wait (lambda () connected)))
          (should (orpheus-backend-connected-p mpd))

          ;; Empty queue, then two tracks.
          (orpheus-backend-command mpd 'clear)
          (orpheus-backend-command mpd 'stop)
          (orpheus-backend-add mpd '("one.mp3" "two.mp3"))
          (orpheus-backend-command mpd 'play)
          (orpheus-backend-queue mpd (lambda (items) (setq queue items)))
          (should (orpheus-integration--wait (lambda () queue)))
          (should (= 2 (length queue)))
          (should (string-match-p "Orpheus – Integration One"
                                  (orpheus-item-label (car queue))))

          (sleep-for 0.3)
          (orpheus-backend-status mpd (lambda (s) (setq status s)))
          (should (orpheus-integration--wait
                   (lambda () (and status
                                   (eq 'play (orpheus-status-state status))))))
          (should (> (orpheus-status-total status) 0))

          (orpheus-backend-track mpd (lambda (result) (setq track result)))
          (should (orpheus-integration--wait (lambda () track)))
          (should (equal "Integration One" (orpheus-track-title track)))
          (should (equal "Orpheus" (orpheus-track-artist track)))
          (should (equal "Integration Album" (orpheus-track-album track)))

          (let ((found 'pending))
            (orpheus-backend-browse mpd 'directories nil
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (= 2 (length found))))
          (let ((found 'pending))
            (orpheus-backend-browse mpd 'artists nil
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (equal '("Orpheus")
                           (mapcar #'orpheus-item-label found))))
          (let ((found 'pending))
            (orpheus-backend-browse mpd 'albums nil
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (equal '("Integration Album")
                           (mapcar #'orpheus-item-label found))))
          (let ((found 'pending))
            (orpheus-backend-browse mpd 'search '("Integration")
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (= 2 (length found))))

          ;; Playlists: save the queue, read it back.
          (orpheus-backend-command mpd 'save "integration list")
          (let ((found 'pending))
            (orpheus-backend-browse mpd 'playlists nil
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (equal '("integration list")
                           (mapcar #'orpheus-item-label found))))
          (let ((found 'pending))
            (orpheus-backend-browse mpd 'playlists '("integration list")
                                    (lambda (items) (setq found items)))
            (should (orpheus-integration--wait (lambda () (listp found))))
            (should (= 2 (length found))))

          ;; Cover art next to the tracks.
          (let ((art 'pending))
            (orpheus-backend-album-art
             mpd (orpheus-track-create :uri "one.mp3")
             (lambda (file) (setq art file)))
            (should (orpheus-integration--wait
                     (lambda () (not (eq art (quote pending))))))
            (should (file-readable-p art))
            (should (> (file-attribute-size (file-attributes art)) 0)))

          ;; Pause, volume, stop.
          (orpheus-backend-command mpd 'pause)
          (sleep-for 0.3)
          (orpheus-backend-status mpd (lambda (s) (setq status s)))
          (should (orpheus-integration--wait
                   (lambda () (and status
                                   (eq 'pause (orpheus-status-state status))))))
          (orpheus-backend-command mpd 'stop))
      (ignore-errors (orpheus-backend-disconnect mpd))
      (when (process-live-p orpheus-integration--mpd)
        (ignore-errors (delete-process orpheus-integration--mpd)))
      (orpheus-integration--stop-mpd))))

;;; mpv

(ert-deftest orpheus-test/integration-local ()
  "A real mpv plays a queue Emacs keeps, and the queue moves on by itself."
  (skip-unless (orpheus-integration--programs-p "mpv" "ffmpeg"))
  (orpheus-integration--fixture)
  (let* ((music (expand-file-name "music/" orpheus-integration--dir))
         (orpheus-local-socket (expand-file-name "mpv.sock"
                                                 orpheus-integration--dir))
         (orpheus-local-audio-driver "null")
         (orpheus-cache-directory (expand-file-name "cache/"
                                                    orpheus-integration--dir))
         (orpheus-notifications nil)
         (local (make-orpheus-local :name 'local))
         (orpheus-backend-current local)
         (connected nil)
         (track nil))
    (unwind-protect
        (progn
          (orpheus-backend-connect local (lambda (backend)
                                           (setq connected backend)))
          (should (orpheus-integration--wait (lambda () connected)))
          (should (orpheus-backend-connected-p local))

          (orpheus-backend-add
           local (list (expand-file-name "one.mp3" music)
                       (expand-file-name "two.mp3" music)))
          (orpheus-backend-command local 'playpos 0)

          ;; The null output is untimed, so the first track runs out at once
          ;; and the queue shows it moved on.
          (should (orpheus-integration--wait
                   (lambda () (= 1 (orpheus-local-instance-index local))) 15))
          (should (orpheus-integration--wait
                   (lambda ()
                     (setq track (orpheus-local-instance-track local))
                     (and track
                          (equal "Integration Two"
                                 (orpheus-track-title track))))))
          (should (equal "Orpheus" (orpheus-track-artist track)))

          (let ((items 'pending))
            (orpheus-backend-browse local 'directories (list music)
                                    (lambda (result) (setq items result)))
            (should (= 2 (length (cl-remove-if-not
                                  (lambda (item)
                                    (equal 'track (orpheus-item-kind item)))
                                  items)))))
          (let ((art 'pending))
            (orpheus-backend-album-art
             local (orpheus-track-create :uri (expand-file-name "one.mp3" music))
             (lambda (file) (setq art file)))
            (should (orpheus-integration--wait
                     (lambda () (not (eq art (quote pending))))))
            (should (file-readable-p art)))
          (orpheus-backend-command local 'volume 33)
          (should (orpheus-integration--wait
                   (lambda ()
                     (orpheus-local--poll local)
                     (= 33 (orpheus-status-volume
                            (orpheus-local-instance-status local)))))))
      (orpheus-backend-disconnect local))))

;;; orpheus-integration-test.el ends here
