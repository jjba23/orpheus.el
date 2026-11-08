;;; orpheus-fake-ssh.el --- A stand-in for ssh in the test suite -*- lexical-binding: t -*-

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The SSH backend starts `ssh -N -L' and then talks to the MPD behind it.
;; This program is what the suite puts in place of ssh: it reads the same -L
;; argument, listens on the local port and copies the bytes to the fake MPD
;; that runs inside the test's own Emacs.  It keeps the suite free of sshd and
;; of any network beyond loopback.

;;; Code:

(defun orpheus-fake-ssh--ports ()
  "Return the local and remote ports from the command line, as a cons."
  (let* ((args command-line-args-left)
         (spec (cadr (member "-L" args)))
         (parts (and spec (split-string spec ":"))))
    (when (and parts (= (length parts) 4))
      (cons (string-to-number (nth 1 parts))
            (string-to-number (nth 3 parts))))))

(defun orpheus-fake-ssh--forward (to)
  "Return a filter that sends everything it receives to TO."
  (lambda (_process chunk)
    (when (process-live-p to)
      (process-send-string to chunk))))

(let ((ports (orpheus-fake-ssh--ports)))
  (if (null ports)
      (kill-emacs 2)
    (let ((server (make-network-process
                   :name "orpheus-fake-ssh"
                   :server t :host "127.0.0.1" :family 'ipv4
                   :service (car ports) :coding 'binary :noquery t
                   :log (lambda (_server client message)
                          (when (string-prefix-p "accept" message)
                            (condition-case nil
                                ;; The greeting can arrive the instant the
                                ;; connection does, so the filter that
                                ;; forwards it to the client is given to
                                ;; `make-network-process' itself.
                                (let ((remote (make-network-process
                                               :name "orpheus-fake-ssh-remote"
                                               :host "127.0.0.1" :family 'ipv4
                                               :service (cdr ports)
                                               :coding 'binary :noquery t
                                               :filter (orpheus-fake-ssh--forward
                                                        client))))
                                  (set-process-filter client
                                                      (orpheus-fake-ssh--forward
                                                       remote)))
                              (error (delete-process client))))))))
      (while (process-live-p server)
        (accept-process-output nil 0.1)))))

;;; orpheus-fake-ssh.el ends here
