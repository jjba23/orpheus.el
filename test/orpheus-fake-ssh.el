;;; orpheus-fake-ssh.el --- A stand-in for ssh in the test suite -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The SSH backend runs `ssh HOST COMMAND' to list a remote directory, search
;; it, and copy one file into a temporary directory.  This program is what the
;; suite puts in place of ssh: it answers those commands with a fixed listing
;; and a fixed file, so the tests need neither sshd nor a network.

;;; Code:

(let ((command (car (last command-line-args-left)))
      (mode (getenv "ORPHEUS_TEST_FAKE_SSH")))
  (when (and (equal mode "nomux")
             (catch 'multiplexed
               (dolist (argument command-line-args-left)
                 (when (or (string-prefix-p "ControlPath=" argument)
                           (equal argument "ControlMaster=auto"))
                   (throw 'multiplexed t)))
               nil))
    (princ "ControlPath too long\n" 'external-debugging-output)
    (kill-emacs 255))
  (cond
   ((and command (string-prefix-p "sleep " command))
    (sleep-for 10)
    (princ "awake"))
   ((and command (string-prefix-p "ls -lL " command))
    (if (member mode '("nosize" "minimal"))
        (kill-emacs 1)
      (princ "-rw-r--r-- 1 user group 11 Nov  8 12:00 Song.mp3\n")))
   ((and command (string-prefix-p "ls -1pR " command))
    (if (equal mode "broken")
        (kill-emacs 1)
      (princ "Music:\nSong.mp3\nAlbum/\n\nMusic/Album:\nOther.mp3\nnotes.txt\n")))
   ((and (equal mode "cue") command (string-prefix-p "ls " command))
    (princ "disc.cue\ndisc.flac\n"))
   ((and command (string-prefix-p "ls " command))
    (if (string-match-p "Album" command)
        (princ "Cover.JPG\nSong.mp3\nnotes.txt\n")
      (princ "Album/\nSong.mp3\nnotes.txt\n")))
   ((and command (string-prefix-p "ffmpeg " command))
    (if (equal mode "minimal")
        (kill-emacs 127)
      (princ "ok")))
   ((and command (string-prefix-p "printf " command))
    (if (equal mode "minimal")
        (kill-emacs 1)
      (princ "ok")))
   ((and (member mode '("cue" "cue-search")) command
         (string-prefix-p "cat " command)
         (string-match-p "disc\\.cue" command))
    (princ (concat "TRACK 01 AUDIO\n  TITLE \"First\"\n"
                   "  INDEX 01 00:00:00\n"
                   "TRACK 02 AUDIO\n  TITLE \"Second\"\n"
                   "  INDEX 01 00:10:00\n")))
   ((and command (string-prefix-p "cat " command))
    (if (member mode '("nocat" "minimal"))
        (kill-emacs 127)
      (princ "fake audio\n")))
   ((and command (string-prefix-p "find " command))
    (cond
     ((equal mode "empty") nil)
     ((equal mode "cue-search")
      (princ "Music/Disc/disc.cue\nMusic/Disc/disc.flac\n"))
     ((member mode '("missing" "broken" "minimal")) (kill-emacs 127))
     (t (princ "Music/Song.mp3\nMusic/Album/Other.mp3\nMusic/notes.txt\n"))))
   (t (kill-emacs 2))))

;;; orpheus-fake-ssh.el ends here
