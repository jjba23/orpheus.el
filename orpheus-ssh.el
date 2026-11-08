;;; orpheus-ssh.el --- A Music Player Daemon on another machine, over ssh -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Some collections live on the machine that owns the speakers.  `ssh -N -L'
;;; makes the daemon there look like a daemon here, on a loopback port nothing
;;; else is using, and the rest is orpheus-mpd.el unchanged: the same queue,
;;; the same library, the same cover art, the same transport.
;;;
;;; Emacs never listens on the network itself and the tunnel is one process
;;; that a sentinel watches, so a machine that goes away is reported rather
;;; than waited for.  The audio output belongs to the far side, which is the
;;; point: a laptop can drive the music on a home server.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'orpheus-backend)
(require 'orpheus-mpd)

;;; Settings

(defcustom orpheus-ssh-host nil
  "Machine the remote MPD runs on, as ssh takes it.
The value is a host name, or `user@host' when the login name differs from
yours.  A nil host is a backend that is not configured yet, and connecting
says so instead of starting a tunnel that could not work."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'orpheus)

(defcustom orpheus-ssh-mpd-port 6600
  "Port the MPD on `orpheus-ssh-host' listens on."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ssh-local-port nil
  "Loopback port the tunnel listens on, or nil to pick a free one.
A port is picked by opening one and closing it again, so two sessions do not
fight over the same number."
  :type '(choice (const :tag "Pick a free port" nil) integer)
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

(cl-defstruct (orpheus-ssh (:include orpheus-mpd)
                           (:constructor orpheus-ssh--create)
                           (:conc-name orpheus-ssh-instance-))
  "The MPD backend reached through an ssh tunnel.
Everything but the connection is the MPD backend's, so its methods are used
as they are and the tunnel is only what gets them their connection."
  process
  local-port
  (connecting nil)
  (waiting nil)
  (attempts 0))

(defun orpheus-ssh--make ()
  "Return a new SSH backend built from the user options."
  (orpheus-ssh--create :name 'ssh
                       :host "127.0.0.1"
                       :port (or orpheus-ssh-local-port 0)
                       :socket nil
                       :password nil))

(orpheus-register-backend 'ssh #'orpheus-ssh--make
                          "A Music Player Daemon on another machine, over ssh")

;;; The tunnel

(defun orpheus-ssh--host ()
  "Return the host to reach, or nil when none is configured."
  (and orpheus-ssh-host
       (not (string-empty-p orpheus-ssh-host))
       orpheus-ssh-host))

(defun orpheus-ssh--free-port ()
  "Return a loopback TCP port nothing is listening on right now."
  (let ((server (make-network-process :name "orpheus-ssh-port"
                                      :server t :host "127.0.0.1"
                                      :family 'ipv4 :service t :noquery t)))
    (prog1 (process-contact server :service)
      (delete-process server))))

(defun orpheus-ssh--tunnel-args (host local-port mpd-port)
  "Return the ssh arguments forwarding LOCAL-PORT to MPD-PORT on HOST."
  (list "-N"
        "-o" "ExitOnForwardFailure=yes"
        "-L" (format "127.0.0.1:%d:localhost:%d" local-port mpd-port)
        host))

(defun orpheus-ssh--tunnel-live-p (ssh)
  "Return non-nil when SSH's tunnel process is running."
  (process-live-p (orpheus-ssh-instance-process ssh)))

(defun orpheus-ssh--stop-tunnel (ssh)
  "Stop SSH's tunnel, if one is running."
  (let ((process (orpheus-ssh-instance-process ssh)))
    (setf (orpheus-ssh-instance-process ssh) nil)
    (when (process-live-p process)
      (ignore-errors (delete-process process)))))

(defun orpheus-ssh--sentinel (ssh process event)
  "Forget the tunnel of SSH when PROCESS ends, as EVENT says."
  (when (and (eq process (orpheus-ssh-instance-process ssh))
             (not (process-live-p process)))
    (setf (orpheus-ssh-instance-process ssh) nil)
    (orpheus-mpd--close ssh)
    (run-hook-with-args 'orpheus-backend-disconnected-hook ssh event)
    (unless (orpheus-ssh-instance-connecting ssh)
      (message "orpheus: the ssh tunnel to %s stopped (%s); see *orpheus ssh*"
               (or (orpheus-ssh--host) "the remote machine")
               (string-trim event)))))

(defun orpheus-ssh--call-waiting (ssh backend)
  "Call everyone waiting for SSH's tunnel with BACKEND."
  (let ((waiting (prog1 (orpheus-ssh-instance-waiting ssh)
                   (setf (orpheus-ssh-instance-waiting ssh) nil))))
    (dolist (callback waiting)
      (ignore-errors (funcall callback backend)))))

(defun orpheus-ssh--give-up (ssh reason)
  "Stop waiting for SSH's tunnel, tell its callers, and say REASON."
  (setf (orpheus-ssh-instance-connecting ssh) nil)
  (orpheus-ssh--call-waiting ssh nil)
  (message "orpheus: cannot reach MPD over ssh: %s" reason))

(defun orpheus-ssh--ready (ssh)
  "Hand SSH to everyone waiting for its tunnel."
  (setf (orpheus-ssh-instance-attempts ssh) 0
        (orpheus-ssh-instance-connecting ssh) nil)
  (orpheus-ssh--call-waiting ssh ssh))

(defun orpheus-ssh--reconnect (ssh)
  "Try to log in to the MPD behind SSH's tunnel, retrying while it starts."
  (let ((attempts (1+ (orpheus-ssh-instance-attempts ssh))))
    (setf (orpheus-ssh-instance-attempts ssh) attempts)
    (cond
      ((not (orpheus-ssh--tunnel-live-p ssh))
       (orpheus-ssh--give-up
        ssh "the ssh program could not open the tunnel; see *orpheus ssh*"))
      ((> attempts 100)
       (orpheus-ssh--give-up ssh "mpd did not answer through the tunnel"))
      (t
       ;; A daemon that is not listening yet is expected while ssh opens the
       ;; port, so its refusal is not repeated on every attempt.
       (let ((inhibit-message t))
         (orpheus-mpd--connect
          ssh
          (lambda (conn)
            (if conn
                (orpheus-ssh--ready ssh)
              (when (orpheus-ssh-instance-connecting ssh)
                (run-with-timer 0.1 nil #'orpheus-ssh--reconnect ssh))))))))))

(defun orpheus-ssh--spawn (ssh)
  "Start SSH's tunnel for SSH and connect the MPD behind it.
Return non-nil when a tunnel process was started."
  (orpheus-ssh--stop-tunnel ssh)
  (if-let* ((host (orpheus-ssh--host)))
           (let* ((local (or orpheus-ssh-local-port (orpheus-ssh--free-port)))
                  (program (or (executable-find orpheus-ssh-program)
                               orpheus-ssh-program)))
             (setf (orpheus-mpd-instance-port ssh) local
                   (orpheus-ssh-instance-local-port ssh) local
                   ;; auth-source is asked for the machine the user names, not
                   ;; for the loopback port the tunnel happens to use.
                   (orpheus-mpd-instance-password ssh)
                   (or (orpheus-mpd-instance-password ssh)
                       (orpheus-mpd--credential
                        (make-orpheus-mpd :host host
                                          :port orpheus-ssh-mpd-port))))
             (setf (orpheus-ssh-instance-process ssh)
                   (condition-case err
                       (make-process
                        :name "orpheus-ssh"
                        :command (append (list program)
                                         orpheus-ssh-extra-args
                                         (orpheus-ssh--tunnel-args
                                          host local orpheus-ssh-mpd-port))
                        :buffer nil :noquery t
                        :stderr (get-buffer-create "*orpheus ssh*")
                        :sentinel (lambda (process event)
                                    (orpheus-ssh--sentinel ssh process event)))
                     (error
                      (orpheus-ssh--give-up ssh (error-message-string err))
                      nil)))
             (when (orpheus-ssh--tunnel-live-p ssh)
               (setf (orpheus-ssh-instance-attempts ssh) 0)
               (orpheus-ssh--reconnect ssh)
               t))
    (orpheus-ssh--give-up
     ssh "set `orpheus-ssh-host' to the machine MPD runs on")
    nil))

;;; Backend methods

(cl-defmethod orpheus-backend-connected-p ((ssh orpheus-ssh))
  "Return non-nil when SSH's tunnel runs and the MPD behind it answered."
  (let ((conn (orpheus-mpd-instance-connection ssh)))
    (and (orpheus-ssh--tunnel-live-p ssh)
         conn
         (eq (orpheus-mpd-connection-state conn) 'ready)
         (process-live-p (orpheus-mpd-connection-process conn)))))

(cl-defmethod orpheus-backend-connect ((ssh orpheus-ssh) callback)
  "Start the tunnel when it is not up, then call CALLBACK with SSH."
  (cond
    ((orpheus-backend-connected-p ssh)
     (funcall callback ssh))
    ((orpheus-ssh-instance-connecting ssh)
     (setf (orpheus-ssh-instance-waiting ssh)
           (append (orpheus-ssh-instance-waiting ssh) (list callback))))
    (t
     (setf (orpheus-ssh-instance-connecting ssh) t
           (orpheus-ssh-instance-waiting ssh) (list callback))
     (if (orpheus-ssh--tunnel-live-p ssh)
         (orpheus-ssh--reconnect ssh)
       (orpheus-ssh--spawn ssh)))))

(cl-defmethod orpheus-backend-disconnect ((ssh orpheus-ssh))
  "Close SSH's connection to the remote MPD and stop the tunnel."
  (orpheus-mpd--close ssh)
  (orpheus-ssh--stop-tunnel ssh)
  (setf (orpheus-ssh-instance-connecting ssh) nil))

(provide 'orpheus-ssh)

;;; orpheus-ssh.el ends here
