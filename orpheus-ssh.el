;;; orpheus-ssh.el --- Music on another machine, played through mpv -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Some collections live on a machine that is not this one.  This backend
;;; lists a remote directory with ssh, copies the track you choose into a
;;; private temporary directory, and hands that copy to mpv.  There is no
;;; daemon on the far side and no tunnel: each listing and each download is one
;;; short-lived ssh command.
;;;
;;; The queue, the transport and the status are the local backend's, unchanged,
;;; because this backend includes it.  Only the path mpv opens is different:
;;; `orpheus-local--resolve-uri' downloads a remote name first.  The temporary
;;; directory is removed when the backend disconnects and when Emacs exits.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)
(require 'orpheus-local)

;;; Settings

(defcustom orpheus-ssh-host nil
  "Machine the music lives on, as ssh takes it.
The value is a host name, or `user@host' when the login name differs from
yours.  A nil host is a backend that is not configured yet, and connecting
says so instead of starting a player that could not work."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'orpheus)

(defcustom orpheus-ssh-directory nil
  "Remote directory the files tab starts in, or nil for the login home.
The value is a path on the far side: relative to the login home, such as
\"Music\", or absolute.  A leading `~' is not expanded, because the path is
quoted for the remote shell."
  :type '(choice (const :tag "The login home" nil) string)
  :group 'orpheus)

(defcustom orpheus-ssh-program "ssh"
  "Program name or path of ssh."
  :type 'string :group 'orpheus)

(defcustom orpheus-ssh-extra-args nil
  "Extra arguments passed to ssh before the host.
A non-standard ssh port, or `-o StrictHostKeyChecking=accept-new' for a
machine whose key is not in `~/.ssh/known_hosts' yet, belongs here."
  :type '(repeat string) :group 'orpheus)

;;; The backend

(cl-defstruct (orpheus-ssh (:include orpheus-local)
                           (:constructor orpheus-ssh--create)
                           (:conc-name orpheus-ssh-instance-))
  "The mpv backend whose files come from another machine over ssh."
  temp-directory
  (downloads (make-hash-table :test #'equal))
  (processes nil)
  current-file)

(defvar orpheus-ssh--live nil
  "SSH backends whose temporary files may still be on disk.")

(defun orpheus-ssh--make ()
  "Return a new SSH backend built from the user options."
  (let ((ssh (orpheus-ssh--create :name 'ssh :volume 100)))
    (push ssh orpheus-ssh--live)
    ssh))

(orpheus-register-backend 'ssh #'orpheus-ssh--make
                          "Music on another machine, copied over ssh and played locally")

;;; The remote host

(defun orpheus-ssh--host ()
  "Return the host to reach, or nil when none is configured."
  (and orpheus-ssh-host
       (not (string-empty-p orpheus-ssh-host))
       orpheus-ssh-host))

(defun orpheus-ssh--program ()
  "Return ssh's program name or path."
  (or (executable-find orpheus-ssh-program) orpheus-ssh-program))

(defun orpheus-ssh--shell-quote (string)
  "Return STRING quoted as one POSIX shell word."
  (concat "'" (string-replace "'" "'\\''" string) "'"))

(defun orpheus-ssh--join (dir name)
  "Return NAME below DIR, where DIR may be empty for the remote home."
  (if (or (null dir) (string-empty-p dir))
      name
    (concat (file-name-as-directory dir) name)))

(defun orpheus-ssh--directory-command (dir)
  "Return the remote command that lists DIR, or the login home when empty."
  (if (or (null dir) (string-empty-p dir))
      "ls -1p"
    (format "ls -1p -- %s" (orpheus-ssh--shell-quote dir))))

(defun orpheus-ssh--media-file-p (name)
  "Return non-nil when NAME looks like something mpv can play."
  (member (downcase (or (file-name-extension name) ""))
          orpheus-local-extensions))

;;; Listing a remote directory

(defun orpheus-ssh--directory-items (dir output)
  "Return rows for DIR from the remote `ls' OUTPUT."
  (delq nil
        (mapcar
         (lambda (line)
           (cond
            ((string-suffix-p "/" line)
             (orpheus-item-create
              :label line :kind 'directory :detail "remote"
              :data (list (cons 'browse
                                (cons 'directories
                                      (list (orpheus-ssh--join dir line)))))))
            ((orpheus-ssh--media-file-p line)
             (orpheus-item-create
              :label line :kind 'track
              :detail (upcase (or (file-name-extension line) "file"))
              :data (list (cons 'uri (orpheus-ssh--join dir line)))))))
         (split-string output "\n" t))))

(defun orpheus-ssh--run (_ssh command callback)
  "Run COMMAND on ssh's host, then call CALLBACK with its output or nil."
  (if-let* ((host (orpheus-ssh--host)))
           (let ((buffer (generate-new-buffer " *orpheus-ssh-command*")))
             (condition-case err
                 (make-process
                  :name "orpheus-ssh"
                  :command (append (list (orpheus-ssh--program))
                                   orpheus-ssh-extra-args
                                   (list host command))
                  :buffer buffer
                  :stderr (get-buffer-create "*orpheus ssh*")
                  :noquery t
                  :sentinel
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (let ((output
                             (and (eq (process-status process) 'exit)
                                  (zerop (process-exit-status process))
                                  (with-current-buffer buffer (buffer-string)))))
                        (when (buffer-live-p buffer) (kill-buffer buffer))
                        (if output
                            (funcall callback output)
                          (message "orpheus: ssh could not list %s; see *orpheus ssh*"
                                   command)
                          (funcall callback nil))))))
               (error
                (when (buffer-live-p buffer) (kill-buffer buffer))
                (message "orpheus: cannot run ssh: %s"
                         (error-message-string err))
                (funcall callback nil))))
    (message "orpheus: set `orpheus-ssh-host' to the machine your music is on")
    (funcall callback nil)))

(defun orpheus-ssh--browse-directories (ssh dir callback)
  "Call CALLBACK with DIR's rows on SSH, or nil when ssh fails."
  (orpheus-ssh--run
   ssh (orpheus-ssh--directory-command dir)
   (lambda (output)
     (funcall callback (and output (orpheus-ssh--directory-items dir output))))))

;;; Downloading one track

(defun orpheus-ssh--temp-directory (ssh)
  "Return SSH's private temporary directory, making it when needed."
  (or (orpheus-ssh-instance-temp-directory ssh)
      (setf (orpheus-ssh-instance-temp-directory ssh)
            (make-temp-file "orpheus-ssh-" t))))

(defun orpheus-ssh--cache-file (ssh uri)
  "Return a path in SSH's temporary directory for URI."
  (let* ((dir (orpheus-ssh--temp-directory ssh))
         (base (or (file-name-nondirectory (directory-file-name uri))
                   "track"))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" base)))
    (expand-file-name
     (format "%s-%s" (substring (secure-hash 'sha1 uri) 0 8) safe)
     dir)))

(defun orpheus-ssh--note-current (ssh path)
  "Remember PATH as SSH's current copy and delete the previous one."
  (let ((old (orpheus-ssh-instance-current-file ssh)))
    (unless (equal old path)
      (when old (ignore-errors (delete-file old)))
      (setf (orpheus-ssh-instance-current-file ssh) path))))

(defun orpheus-ssh--download-sentinel (ssh process path uri callback buffer)
  "Finish PROCESS, one of SSH's downloads of URI into PATH, and call CALLBACK."
  (unless (process-live-p process)
    (setf (orpheus-ssh-instance-processes ssh)
          (delq process (orpheus-ssh-instance-processes ssh)))
    (unwind-protect
        (if (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process))
                 (file-directory-p (file-name-directory path)))
            (progn
              (with-current-buffer buffer
                (let ((coding-system-for-write 'binary))
                  (write-region (point-min) (point-max) path nil 'silent)))
              (puthash uri path (orpheus-ssh-instance-downloads ssh))
              (orpheus-ssh--note-current ssh path)
              (funcall callback path))
          (ignore-errors (delete-file path))
          (message "orpheus: ssh could not download %s; see *orpheus ssh*" uri)
          (funcall callback nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun orpheus-ssh--download (ssh uri callback)
  "Download URI from SSH's host, then call CALLBACK with the local copy."
  (let ((cached (gethash uri (orpheus-ssh-instance-downloads ssh)))
        (host (orpheus-ssh--host)))
    (cond
     ((and cached (file-readable-p cached))
      (orpheus-ssh--note-current ssh cached)
      (funcall callback cached))
     ((null host)
      (message "orpheus: set `orpheus-ssh-host' to the machine your music is on")
      (funcall callback nil))
     (t
      (message "orpheus: downloading %s" (file-name-nondirectory uri))
      (let* ((path (orpheus-ssh--cache-file ssh uri))
             (buffer (generate-new-buffer " *orpheus-ssh-download*"))
             (process
              (condition-case err
                  (progn
                    (with-current-buffer buffer (set-buffer-multibyte nil))
                    (make-process
                     :name "orpheus-ssh-download"
                     :command (append
                               (list (orpheus-ssh--program))
                               orpheus-ssh-extra-args
                               (list host
                                     (format "cat -- %s"
                                             (orpheus-ssh--shell-quote uri))))
                     :buffer buffer
                     :stderr (get-buffer-create "*orpheus ssh*")
                     :noquery t
                     :coding 'binary
                     :sentinel
                     (lambda (process _event)
                       (orpheus-ssh--download-sentinel
                        ssh process path uri callback buffer))))
                (error
                 (when (buffer-live-p buffer) (kill-buffer buffer))
                 (message "orpheus: cannot run ssh: %s"
                          (error-message-string err))
                 nil))))
        (if process
            (push process (orpheus-ssh-instance-processes ssh))
          (funcall callback nil)))))))

(defun orpheus-ssh--cleanup (ssh)
  "Stop SSH's downloads and delete its temporary directory."
  (dolist (process (orpheus-ssh-instance-processes ssh))
    (when (process-live-p process) (ignore-errors (delete-process process))))
  (setf (orpheus-ssh-instance-processes ssh) nil)
  (clrhash (orpheus-ssh-instance-downloads ssh))
  (let ((dir (orpheus-ssh-instance-temp-directory ssh)))
    (when dir
      (ignore-errors (delete-directory dir t))
      (setf (orpheus-ssh-instance-temp-directory ssh) nil)))
  (setf (orpheus-ssh-instance-current-file ssh) nil))

(defun orpheus-ssh--cleanup-all ()
  "Delete the temporary files of every SSH backend still alive."
  (dolist (ssh orpheus-ssh--live)
    (ignore-errors (orpheus-ssh--cleanup ssh))))

(add-hook 'kill-emacs-hook #'orpheus-ssh--cleanup-all)

;;; Backend methods

(cl-defmethod orpheus-local--resolve-uri ((ssh orpheus-ssh) uri callback)
  "Download URI from SSH to a local copy, then call CALLBACK with its path."
  (orpheus-ssh--download ssh uri callback))

(cl-defmethod orpheus-backend-connected-p ((ssh orpheus-ssh))
  "Return non-nil when SSH has a host and its mpv is listening."
  (and (orpheus-ssh--host) (orpheus-local--ready-p ssh)))

(cl-defmethod orpheus-backend-connect ((ssh orpheus-ssh) callback)
  "Start mpv for SSH when a host is set, then call CALLBACK with it or nil."
  (if (orpheus-ssh--host)
      (orpheus-local--ensure ssh callback)
    (message "orpheus: set `orpheus-ssh-host' to the machine your music is on")
    (funcall callback nil)))

(cl-defmethod orpheus-backend-disconnect ((ssh orpheus-ssh))
  "Stop SSH's mpv and delete its downloaded files."
  (orpheus-local--kill ssh)
  (orpheus-ssh--cleanup ssh)
  (setq orpheus-ssh--live (delq ssh orpheus-ssh--live)))

(cl-defmethod orpheus-backend-browse ((ssh orpheus-ssh) kind arg callback)
  "Browse KIND on SSH, with ARG as the path down the remote hierarchy."
  (pcase kind
         ('queue (funcall callback (orpheus-local--queue-items ssh)))
         ('directories
          (orpheus-ssh--browse-directories
           ssh (or (car arg) orpheus-ssh-directory) callback))
         (_ (funcall callback nil))))

(cl-defmethod orpheus-backend-album-art ((ssh orpheus-ssh) track callback)
  "Find cover art for TRACK in SSH's downloaded copy, then call CALLBACK."
  (let ((file (gethash (orpheus-track-uri track)
                       (orpheus-ssh-instance-downloads ssh))))
    (if (and file (file-readable-p file))
        (orpheus-local--album-art-file file callback)
      (funcall callback nil))))

(cl-defmethod orpheus-backend-capabilities ((_ssh orpheus-ssh))
  "Return what files on another machine can browse."
  '(queue directories))

(provide 'orpheus-ssh)

;;; orpheus-ssh.el ends here
