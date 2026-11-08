;;; orpheus-fake-ssh.el --- A stand-in for ssh in the test suite -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The SSH backend runs `ssh HOST COMMAND' to list a remote directory and to
;; copy one file into a temporary directory.  This program is what the suite
;; puts in place of ssh: it answers those two commands with a fixed listing and
;; a fixed file, so the tests need neither sshd nor a network.

;;; Code:

(let ((command (car (last command-line-args-left))))
  (cond
   ((and command (string-prefix-p "ls " command))
    (princ "Album/\nSong.mp3\nnotes.txt\n"))
   ((and command (string-prefix-p "cat " command))
    (princ "fake audio\n"))
   (t (kill-emacs 2))))

;;; orpheus-fake-ssh.el ends here
