;;; orpheus-fake-ssh.el --- A stand-in for ssh in the test suite -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The SSH backend runs `ssh HOST COMMAND' to list a remote directory, search
;; it, and copy one file into a temporary directory.  This program is what the
;; suite puts in place of ssh: it answers those commands with a fixed listing
;; and a fixed file, so the tests need neither sshd nor a network.

;;; Code:

(let ((command (car (last command-line-args-left))))
  (cond
   ((and command (string-prefix-p "ls " command))
    (princ "Album/\nSong.mp3\nnotes.txt\n"))
   ((and command (string-match-p "ORPHEUS-SIZE" command))
    (princ "ORPHEUS-SIZE 11\nfake audio\n"))
   ((and command (string-prefix-p "cat " command))
    (princ "fake audio\n"))
   ((and command (string-prefix-p "find " command))
    (princ "Music/Song.mp3\nMusic/Album/Other.mp3\nMusic/notes.txt\n"))
   (t (kill-emacs 2))))

;;; orpheus-fake-ssh.el ends here
