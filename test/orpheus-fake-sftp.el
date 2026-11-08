;;; orpheus-fake-sftp.el --- A stand-in for sftp in the test suite -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The SSH backend falls back to `sftp -b BATCH HOST' when the remote shell
;; cannot run `cat'.  This program reads the batch file, takes the local path
;; from its `get' command, and writes a fixed payload there, so the tests need
;; neither sshd nor a network.

;;; Code:

(let* ((args command-line-args-left)
       (batch (cadr (member "-b" args))))
  (if (not batch)
      (kill-emacs 2)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents batch)
          (goto-char (point-min))
          (if (re-search-forward "^get \"\\([^\"]*\\)\" \"\\([^\"]*\\)\"$" nil t)
              (with-temp-file (match-string 2)
                (insert "sftp bytes\n"))
            (kill-emacs 2)))
      (error (kill-emacs 2)))))

;;; orpheus-fake-sftp.el ends here
