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
  (cond
   ((and command (string-prefix-p "ls -lL " command))
    (if (equal mode "nosize")
        (kill-emacs 1)
      (princ "-rw-r--r-- 1 user group 11 Nov  8 12:00 Song.mp3\n")))
   ((and command (string-prefix-p "ls -1pR " command))
    (if (equal mode "broken")
        (kill-emacs 1)
      (princ "Music:\nSong.mp3\nAlbum/\n\nMusic/Album:\nOther.mp3\nnotes.txt\n")))
   ((and command (string-prefix-p "ls " command))
    (princ "Album/\nSong.mp3\nnotes.txt\n"))
   ((and command (string-prefix-p "cat " command))
    (princ "fake audio\n"))
   ((and command (string-prefix-p "find " command))
    (cond
     ((equal mode "empty") nil)
     ((member mode '("missing" "broken")) (kill-emacs 127))
     (t (princ "Music/Song.mp3\nMusic/Album/Other.mp3\nMusic/notes.txt\n"))))
   (t (kill-emacs 2))))

;;; orpheus-fake-ssh.el ends here
