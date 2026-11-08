;;; orpheus-ssh.el --- Music on another machine, played through mpv -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Some collections live on a machine that is not this one.  This backend
;;; lists a remote directory with ssh, copies the track you choose into a
;;; private temporary directory, and hands that copy to mpv.  There is no
;;; daemon on the far side and no tunnel: each listing, search and download is
;;; one short-lived ssh command, or an sftp process when the shell has no cat.
;;;
;;; Downloads run in a queue.  A play request jumps ahead of background
;;; prefetches, at most `orpheus-ssh-concurrency' ssh streams run at once, and
;;; each stream writes its bytes straight to the cache file.  Nothing copies a
;;; whole track through an Emacs buffer, so starting another download while a
;;; track plays never blocks the editor.
;;;
;;; The cache keeps a few completed tracks, not only the last one, and the next
;;; queue entries are fetched in the background.  Replaying a recent track or
;;; moving to the next one usually opens a file that is already here.  The
;;; queue, the transport and the status are the local backend's, unchanged.

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
machine whose key is not in `~/.ssh/known_hosts' yet, belongs here.  On a slow
link `-C' can make a large, uncompressed track arrive sooner."
  :type '(repeat string) :group 'orpheus)

(defcustom orpheus-ssh-sftp-program "sftp"
  "Program name or path of sftp, used when the remote shell cannot run `cat'."
  :type 'string :group 'orpheus)

(defcustom orpheus-ssh-sftp-extra-args nil
  "Extra arguments passed to sftp before the host.
A non-standard port belongs here as `-P PORT', because sftp spells the port
differently from ssh; the rest of ssh's options have sftp's own spelling too."
  :type '(repeat string) :group 'orpheus)

(defcustom orpheus-ssh-cache-count 8
  "Number of downloaded tracks kept in SSH's temporary directory.
A track played from the cache needs no second transfer.  The playing track is
never evicted."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ssh-prefetch 2
  "Number of upcoming queue tracks fetched in the background.
Nil or zero turns prefetching off.  Prefetches use spare download slots and are
always behind a track the user asked to play."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ssh-concurrency 2
  "Maximum number of simultaneous ssh downloads.
One is enough to keep playback going; two lets the next track arrive while the
current one is still playing.  More than two rarely improves a single link."
  :type 'integer :group 'orpheus)

;;; The backend

(cl-defstruct (orpheus-ssh-transfer
               (:constructor orpheus-ssh-transfer-create)
               (:conc-name orpheus-ssh-transfer-))
  "One track SSH is fetching, or has finished fetching."
  uri path state (bytes 0) total started samples callbacks process
  retried batch timer)

(cl-defstruct (orpheus-ssh (:include orpheus-local)
                           (:constructor orpheus-ssh--create)
                           (:conc-name orpheus-ssh-instance-))
  "The mpv backend whose files come from another machine over ssh."
  temp-directory
  (downloads (make-hash-table :test #'equal))
  (transfers nil)
  (processes nil)
  (cache nil)
  current-file
  search-fallback
  search-warned
  size-failed
  cat-failed
  capabilities
  capabilities-waiting
  capabilities-reported)

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

(defconst orpheus-ssh--speed-window 5.0
  "Seconds of byte samples that make up a transfer's rolling speed.")

(defconst orpheus-ssh--speed-samples 128
  "Maximum number of byte samples kept for one transfer.")

(defun orpheus-ssh--download-command (uri)
  "Return the simple command that copies URI to standard output."
  (format "cat -- %s" (orpheus-ssh--shell-quote uri)))

(defun orpheus-ssh--size-command (uri)
  "Return the simple command that reports URI's size."
  (format "ls -lL -- %s 2>/dev/null" (orpheus-ssh--shell-quote uri)))

(defun orpheus-ssh--parse-size (output)
  "Return the byte size in ls -l OUTPUT, or nil."
  (when-let* ((line (car (split-string (or output "") "\n" t)))
              (fields (and line
                           (split-string (string-trim line)
                                         "[[:space:]]+" t))))
    (when (>= (length fields) 5)
      (let ((size (string-to-number (nth 4 fields))))
        (when (> size 0) size)))))

(defun orpheus-ssh--sftp-program ()
  "Return sftp's program name or path."
  (or (executable-find orpheus-ssh-sftp-program) orpheus-ssh-sftp-program))

(defun orpheus-ssh--sftp-quote (path)
  "Return PATH quoted as one sftp batch word."
  (concat "\""
          (mapconcat
           (lambda (char)
             (if (member char (string-to-list "\\\"*?[]"))
                 (concat "\\" (char-to-string char))
               (char-to-string char)))
           (string-to-list path) "")
          "\""))

(defun orpheus-ssh--sftp-batch (uri path)
  "Return an sftp batch command that copies URI to PATH."
  (when (or (string-match-p "[\r\n]" uri)
            (string-match-p "[\r\n]" path))
    (error "Path contains a newline"))
  (format "get %s %s\n"
          (orpheus-ssh--sftp-quote uri)
          (orpheus-ssh--sftp-quote path)))

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

(defun orpheus-ssh--cover-file (dir output)
  "Return the first cover path in DIR from the remote `ls' OUTPUT, or nil."
  (when dir
    (let ((files (split-string (or output "") "\n" t)))
      (cl-loop
       for name in orpheus-local-cover-names
       for base = (file-name-nondirectory name)
       for match = (cl-find-if
                    (lambda (file)
                      (and (not (string-suffix-p "/" file))
                           (equal (downcase file) (downcase base))))
                    files)
       when match return (orpheus-ssh--join dir match)))))

(defun orpheus-ssh--media-file-p (name)
  "Return non-nil when NAME looks like something mpv can play."
  (member (downcase (or (file-name-extension name) ""))
          orpheus-local-extensions))

;;; Listing and searching a remote directory

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

(defun orpheus-ssh--search-root ()
  "Return the remote directory search starts from."
  (if (and orpheus-ssh-directory
           (not (string-empty-p orpheus-ssh-directory)))
      orpheus-ssh-directory
    "."))

(defun orpheus-ssh--search-command ()
  "Return the find command that lists files below the starting directory."
  (format "find %s -type f -print 2>/dev/null"
          (orpheus-ssh--shell-quote (orpheus-ssh--search-root))))

(defun orpheus-ssh--search-listing-command ()
  "Return the recursive ls command used when find is unavailable."
  (format "ls -1pR -- %s"
          (orpheus-ssh--shell-quote (orpheus-ssh--search-root))))

(defun orpheus-ssh--relative-label (path)
  "Return PATH with the configured remote directory removed."
  (let ((root (and orpheus-ssh-directory
                   (file-name-as-directory orpheus-ssh-directory))))
    (if (and root (string-prefix-p root path))
        (substring path (length root))
      path)))

(defun orpheus-ssh--search-items (query output)
  "Return track rows from search OUTPUT matching QUERY.
OUTPUT is the newline separated result of `orpheus-ssh--search-command'."
  (let ((needle (downcase query)))
    (delq nil
          (mapcar
           (lambda (line)
             (let ((path (string-remove-prefix "./" line)))
               (when (and (orpheus-ssh--media-file-p path)
                          (string-match-p (regexp-quote needle)
                                          (downcase path)))
                 (orpheus-item-create
                  :label (orpheus-ssh--relative-label path)
                  :kind 'track
                  :detail (upcase (or (file-name-extension path) "file"))
                  :data (list (cons 'uri path))))))
           (split-string output "\n" t)))))

(defun orpheus-ssh--search-listing-items (query output)
  "Return track rows from recursive ls OUTPUT matching QUERY."
  (let ((dir nil)
        (paths nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-empty-p line))
       ((string-suffix-p ":" line)
        (setq dir (string-remove-suffix ":" line)))
       ((string-suffix-p "/" line))
       ((orpheus-ssh--media-file-p line)
        (push (orpheus-ssh--join (if (equal dir ".") "" dir) line)
              paths))))
    (orpheus-ssh--search-items query (string-join (nreverse paths) "\n"))))

(defun orpheus-ssh--search (ssh query callback)
  "Search SSH for QUERY and call CALLBACK with rows."
  (cond
   ((or (null query) (string-empty-p query))
    (funcall callback nil))
   ((orpheus-ssh-instance-search-fallback ssh)
    (orpheus-ssh--search-listing ssh query callback))
   (t
    (orpheus-ssh--run
     ssh (orpheus-ssh--search-command)
     (lambda (output)
       (if (null output)
           (orpheus-ssh--search-listing ssh query callback)
         (setf (orpheus-ssh-instance-search-warned ssh) nil)
         (funcall callback (orpheus-ssh--search-items query output))))
     t))))

(defun orpheus-ssh--search-listing (ssh query callback)
  "Search SSH with a recursive listing for QUERY, then call CALLBACK with rows."
  (orpheus-ssh--run
   ssh (orpheus-ssh--search-listing-command)
   (lambda (output)
     (if output
         (progn
           (setf (orpheus-ssh-instance-search-fallback ssh) t)
           (unless (orpheus-ssh-instance-search-warned ssh)
             (setf (orpheus-ssh-instance-search-warned ssh) t)
             (message "orpheus: remote find is unavailable; using a recursive listing"))
           (funcall callback (orpheus-ssh--search-listing-items query output)))
       (setf (orpheus-ssh-instance-search-fallback ssh) t)
       (unless (orpheus-ssh-instance-search-warned ssh)
         (setf (orpheus-ssh-instance-search-warned ssh) t)
         (message "orpheus: remote search failed; browse the Directories tab instead"))
       (funcall callback nil)))
   t))

(defun orpheus-ssh--run (_ssh command callback &optional quiet)
  "Run COMMAND on ssh's host, then call CALLBACK with its output or nil.
When QUIET is non-nil, do not report a command that failed; the caller has a
more useful message.  An empty successful command yields an empty string."
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
                      (let ((success
                             (and (eq (process-status process) 'exit)
                                  (zerop (process-exit-status process))))
                            (output (with-current-buffer buffer
                                      (buffer-string))))
                        (when (buffer-live-p buffer) (kill-buffer buffer))
                        (if success
                            (funcall callback output)
                          (unless quiet
                            (message "orpheus: ssh could not run %s; see *orpheus ssh*"
                                     command))
                          (funcall callback nil))))))
               (error
                (when (buffer-live-p buffer) (kill-buffer buffer))
                (unless quiet
                  (message "orpheus: cannot run ssh: %s"
                           (error-message-string err)))
                (funcall callback nil))))
    (message "orpheus: set `orpheus-ssh-host' to the machine your music is on")
    (funcall callback nil)))

;;; Checking the remote shell

(defconst orpheus-ssh--probe-commands
  '((cat . "cat -- /dev/null")
    (find . "find . -maxdepth 0 -print")
    (size . "ls -lL -- /")
    (substitution . "printf '%s' \"$(printf ok)\"")
    (compound . "printf ok >/dev/null && printf ok"))
  "Remote commands that test the features SSH uses.")

(defun orpheus-ssh--capability-summary (capabilities)
  "Return one line describing CAPABILITIES."
  (format (concat "cat %s, find %s, ls sizes %s, command substitution %s, "
                  "compound commands %s; downloads use %s")
          (if (alist-get 'cat capabilities) "yes" "no")
          (if (alist-get 'find capabilities) "yes" "no")
          (if (alist-get 'size capabilities) "yes" "no")
          (if (alist-get 'substitution capabilities) "yes" "no")
          (if (alist-get 'compound capabilities) "yes" "no")
          (if (alist-get 'cat capabilities) "cat" "sftp")))

(defun orpheus-ssh--probe (ssh)
  "Probe SSH's remote commands once and then call its waiting callbacks."
  (let ((checks orpheus-ssh--probe-commands)
        (results nil))
    (cl-labels
        ((finish ()
          (let ((capabilities (nreverse results)))
            (setf (orpheus-ssh-instance-capabilities ssh) capabilities)
            (unless (alist-get 'cat capabilities)
              (setf (orpheus-ssh-instance-cat-failed ssh) t))
            (unless (alist-get 'size capabilities)
              (setf (orpheus-ssh-instance-size-failed ssh) t))
            (unless (alist-get 'find capabilities)
              (setf (orpheus-ssh-instance-search-fallback ssh) t))
            (let ((waiting (orpheus-ssh-instance-capabilities-waiting ssh)))
              (setf (orpheus-ssh-instance-capabilities-waiting ssh) nil)
              (dolist (callback waiting)
                (ignore-errors (funcall callback capabilities))))))
         (next ()
          (if-let* ((check (pop checks)))
              (orpheus-ssh--run
               ssh (cdr check)
               (lambda (output)
                 (push (cons (car check) (stringp output)) results)
                 (next))
               t)
            (finish))))
      (next))))

(defun orpheus-ssh--capabilities (ssh callback)
  "Call CALLBACK with SSH's capabilities, probing the host only once."
  (cond
   ((orpheus-ssh-instance-capabilities ssh)
    (funcall callback (orpheus-ssh-instance-capabilities ssh)))
   ((orpheus-ssh-instance-capabilities-waiting ssh)
    (setf (orpheus-ssh-instance-capabilities-waiting ssh)
          (append (orpheus-ssh-instance-capabilities-waiting ssh)
                  (list callback))))
   (t
    (setf (orpheus-ssh-instance-capabilities-waiting ssh) (list callback))
    (orpheus-ssh--probe ssh))))

(defun orpheus-ssh--report-capabilities (ssh capabilities)
  "Report CAPABILITIES for SSH once in the echo area."
  (unless (orpheus-ssh-instance-capabilities-reported ssh)
    (setf (orpheus-ssh-instance-capabilities-reported ssh) t)
    (message "orpheus: SSH %s: %s"
             (or (orpheus-ssh--host) "remote")
             (orpheus-ssh--capability-summary capabilities))))

(defconst orpheus-ssh--capability-labels
  '((cat . "cat")
    (find . "find")
    (size . "ls -lL")
    (substitution . "command substitution")
    (compound . "compound commands"))
  "Labels used by `orpheus-ssh-diagnose'.")

(defun orpheus-ssh--diagnose-buffer (ssh buffer)
  "Fill BUFFER with SSH's capability report."
  (let ((capabilities (orpheus-ssh-instance-capabilities ssh)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "SSH host: %s\n\n"
                        (or (orpheus-ssh--host) "not configured")))
        (dolist (entry orpheus-ssh--capability-labels)
          (insert (format "%-22s %s\n"
                          (cdr entry)
                          (if (alist-get (car entry) capabilities)
                              "yes" "no"))))
        (insert (format "%-22s %s\n" "sftp (local)"
                        (if (executable-find orpheus-ssh-sftp-program)
                            "yes" "no")))
        (insert (format "\nDownloads use %s.\n"
                        (if (alist-get 'cat capabilities) "cat" "sftp")))))))

(defun orpheus-ssh-diagnose ()
  "Report which commands and shell features SSH's host supports."
  (interactive)
  (unless (orpheus-ssh--host)
    (user-error "Set `orpheus-ssh-host' to the machine your music is on"))
  (let* ((current (and (boundp 'orpheus-backend-current)
                       (symbol-value 'orpheus-backend-current)))
         (ssh (if (orpheus-ssh-p current) current (orpheus-ssh--make)))
         (buffer (get-buffer-create "*orpheus ssh capabilities*")))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "SSH host: %s\n\nChecking...\n"
                        (orpheus-ssh--host)))))
    (display-buffer buffer)
    (orpheus-ssh--capabilities
     ssh
     (lambda (_capabilities)
       (when (buffer-live-p buffer)
         (orpheus-ssh--diagnose-buffer ssh buffer))
       (setf (orpheus-ssh-instance-capabilities-reported ssh) t)))))

(defun orpheus-ssh--browse-directories (ssh dir callback)
  "Call CALLBACK with DIR's rows on SSH, or nil when ssh fails."
  (orpheus-ssh--run
   ssh (orpheus-ssh--directory-command dir)
   (lambda (output)
     (funcall callback (and output (orpheus-ssh--directory-items dir output))))))

;;; Remote cover art

(defun orpheus-ssh--cover-cache-file (remote)
  "Return the local cache path for REMOTE's cover."
  (orpheus-cache-file
   (format "ssh-cover:%s:%s" (or (orpheus-ssh--host) "") remote)
   (file-name-nondirectory remote)))

(defun orpheus-ssh--cover-ready-p (path)
  "Return non-nil when PATH is a non-empty readable cover file."
  (and path
       (file-readable-p path)
       (> (or (ignore-errors
                (file-attribute-size (file-attributes path))) 0) 0)))

(defun orpheus-ssh--folder-art (ssh dir callback)
  "Find cover art for remote DIR on SSH, then call CALLBACK with a local path."
  (orpheus-ssh--run
   ssh (orpheus-ssh--directory-command dir)
   (lambda (output)
     (let ((remote (and output (orpheus-ssh--cover-file dir output))))
       (if remote
           (orpheus-ssh--fetch-cover ssh remote callback)
         (funcall callback nil))))))

(defun orpheus-ssh--fetch-cover (ssh remote callback)
  "Copy REMOTE cover from SSH into the cache, then call CALLBACK with its path."
  (let ((path (orpheus-ssh--cover-cache-file remote)))
    (cond
     ((orpheus-ssh--cover-ready-p path) (funcall callback path))
     ((orpheus-ssh-instance-cat-failed ssh)
      (orpheus-ssh--fetch-cover-sftp ssh remote path callback))
     (t
      (ignore-errors (delete-file path))
      (orpheus-ssh--fetch-cover-cat ssh remote path callback)))))

(defun orpheus-ssh--fetch-cover-cat (ssh remote path callback)
  "Fetch REMOTE's cover through SSH's shell into PATH, then call CALLBACK."
  (let ((buffer (generate-new-buffer " *orpheus-ssh-cover*")))
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (condition-case nil
        (let ((process
               (make-process
                :name "orpheus-ssh-cover"
                :command (append
                          (list (orpheus-ssh--program))
                          orpheus-ssh-extra-args
                          (list (orpheus-ssh--host)
                                (format "cat -- %s"
                                        (orpheus-ssh--shell-quote remote))))
                :buffer buffer
                :stderr (get-buffer-create "*orpheus ssh*")
                :coding 'binary
                :noquery t
                :sentinel (lambda (process _event)
                            (orpheus-ssh--cover-cat-sentinel
                             ssh remote path buffer callback process)))))
          (push process (orpheus-ssh-instance-processes ssh)))
      (error
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (funcall callback nil)))))

(defun orpheus-ssh--cover-cat-sentinel (ssh remote path buffer callback process)
  "Finish fetching REMOTE's cover into PATH for SSH after PROCESS exits."
  (unless (process-live-p process)
    (setf (orpheus-ssh-instance-processes ssh)
          (delq process (orpheus-ssh-instance-processes ssh)))
    (let ((status (process-status process))
          (code (ignore-errors (process-exit-status process))))
      (if (and (eq status 'exit) (integerp code) (zerop code))
          (progn
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((coding-system-for-write 'binary))
                  (ignore-errors
                    (write-region (point-min) (point-max) path nil 'silent))))
              (kill-buffer buffer))
            (if (orpheus-ssh--cover-ready-p path)
                (funcall callback path)
              (ignore-errors (delete-file path))
              (funcall callback nil)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (and (eq status 'exit)
                   (memq code '(126 127))
                   (memq ssh orpheus-ssh--live))
          (setf (orpheus-ssh-instance-cat-failed ssh) t))
        (if (and (eq status 'exit) (memq ssh orpheus-ssh--live))
            (orpheus-ssh--fetch-cover-sftp ssh remote path callback)
          (funcall callback nil))))))

(defun orpheus-ssh--fetch-cover-sftp (ssh remote path callback)
  "Fetch REMOTE's cover over SFTP for SSH into PATH, then call CALLBACK."
  (let ((batch nil))
    (condition-case nil
        (progn
          (setq batch (make-temp-file "orpheus-sftp-cover-" nil ".batch"))
          (with-temp-file path)
          (with-temp-file batch
            (insert (orpheus-ssh--sftp-batch remote path)))
          (let ((process
                 (make-process
                  :name "orpheus-ssh-sftp-cover"
                  :command (append
                            (list (orpheus-ssh--sftp-program))
                            orpheus-ssh-sftp-extra-args
                            (list "-b" batch "--" (orpheus-ssh--host)))
                  :buffer nil
                  :stderr (get-buffer-create "*orpheus ssh*")
                  :noquery t
                  :sentinel (lambda (process _event)
                              (orpheus-ssh--cover-sftp-sentinel
                               ssh path batch callback process)))))
            (push process (orpheus-ssh-instance-processes ssh))))
      (error
       (when (and batch (file-exists-p batch))
         (ignore-errors (delete-file batch)))
       (ignore-errors (delete-file path))
       (funcall callback nil)))))

(defun orpheus-ssh--cover-sftp-sentinel (ssh path batch callback process)
  "Finish fetching a cover into PATH for SSH after sftp PROCESS exits."
  (unless (process-live-p process)
    (setf (orpheus-ssh-instance-processes ssh)
          (delq process (orpheus-ssh-instance-processes ssh)))
    (when (and batch (file-exists-p batch))
      (ignore-errors (delete-file batch)))
    (let ((status (process-status process))
          (code (ignore-errors (process-exit-status process))))
      (if (and (eq status 'exit) (integerp code) (zerop code)
               (orpheus-ssh--cover-ready-p path))
          (funcall callback path)
        (ignore-errors (delete-file path))
        (funcall callback nil)))))

;;; Downloads and their queue

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
  "Remember PATH as the copy mpv is playing for SSH."
  (setf (orpheus-ssh-instance-current-file ssh) path))

(defun orpheus-ssh--touch-cache (ssh uri)
  "Move URI to the front of SSH's least-recently-used cache."
  (setf (orpheus-ssh-instance-cache ssh)
        (cons uri (delete uri (orpheus-ssh-instance-cache ssh)))))

(defun orpheus-ssh--prune-cache (ssh)
  "Delete cached tracks beyond `orpheus-ssh-cache-count' on SSH.
The playing track and any track still being fetched are never evicted."
  (let* ((limit (max 1 orpheus-ssh-cache-count))
         (cache (delete-dups (orpheus-ssh-instance-cache ssh)))
         (keep (cl-subseq cache 0 (min limit (length cache))))
         (drop (nthcdr (length keep) cache)))
    (setf (orpheus-ssh-instance-cache ssh) keep)
    (dolist (uri drop)
      (let ((path (gethash uri (orpheus-ssh-instance-downloads ssh))))
        (unless (or (equal path (orpheus-ssh-instance-current-file ssh))
                    (cl-find-if
                     (lambda (transfer)
                       (and (memq (orpheus-ssh-transfer-state transfer)
                                  '(queued downloading))
                            (equal uri (orpheus-ssh-transfer-uri transfer))))
                     (orpheus-ssh-instance-transfers ssh)))
          (remhash uri (orpheus-ssh-instance-downloads ssh))
          (when (and path (file-readable-p path))
            (ignore-errors (delete-file path)))
          (setf (orpheus-ssh-instance-transfers ssh)
                (cl-remove-if
                 (lambda (transfer)
                   (and (eq (orpheus-ssh-transfer-state transfer) 'done)
                        (equal uri (orpheus-ssh-transfer-uri transfer))))
                 (orpheus-ssh-instance-transfers ssh))))))))

(defun orpheus-ssh--transfer-percent (transfer)
  "Return TRANSFER's completion percentage, or nil when its size is unknown."
  (let ((total (orpheus-ssh-transfer-total transfer)))
    (when (and total (> total 0))
      (orpheus-progress-percent (orpheus-ssh-transfer-bytes transfer) total))))

(defun orpheus-ssh--transfer-speed (transfer)
  "Return TRANSFER's recent speed in bytes per second, or nil.
Use the last `orpheus-ssh--speed-window' seconds of samples, and the average
since `started' before there are two samples to compare."
  (let* ((now (float-time))
         (recent (cl-remove-if
                  (lambda (sample)
                    (< (car sample) (- now orpheus-ssh--speed-window)))
                  (orpheus-ssh-transfer-samples transfer)))
         (newest (car recent))
         (oldest (car (last recent)))
         (started (orpheus-ssh-transfer-started transfer))
         (bytes (orpheus-ssh-transfer-bytes transfer)))
    (cond
     ((and newest oldest (> (car newest) (car oldest)))
      (let ((seconds (- (car newest) (car oldest)))
            (received (- (cdr newest) (cdr oldest))))
        (when (and (> seconds 0) (>= received 0))
          (/ (float received) seconds))))
     ((and started (> bytes 0))
      (/ (float bytes) (max 0.001 (- now started))))
     (t nil))))

(defun orpheus-ssh--transfer-eta (transfer)
  "Return TRANSFER's estimated seconds left, or nil when unknown."
  (let ((total (orpheus-ssh-transfer-total transfer))
        (bytes (orpheus-ssh-transfer-bytes transfer))
        (speed (orpheus-ssh--transfer-speed transfer)))
    (when (and total speed (> speed 0))
      (max 0 (ceiling (/ (- total bytes) (float speed)))))))

(defun orpheus-ssh--note-transfer-sample (transfer)
  "Record TRANSFER's current byte count for its rolling speed."
  (let ((samples (cons (cons (float-time)
                             (orpheus-ssh-transfer-bytes transfer))
                       (orpheus-ssh-transfer-samples transfer))))
    (setf (orpheus-ssh-transfer-samples transfer)
          (if (> (length samples) orpheus-ssh--speed-samples)
              (cl-subseq samples 0 orpheus-ssh--speed-samples)
            samples))))

(defun orpheus-ssh--request-size (ssh transfer)
  "Ask SSH once for TRANSFER's size, when the host can report it."
  (unless (orpheus-ssh-instance-size-failed ssh)
    (orpheus-ssh--run
     ssh (orpheus-ssh--size-command (orpheus-ssh-transfer-uri transfer))
     (lambda (output)
       (if-let* ((size (orpheus-ssh--parse-size output)))
           (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
             (setf (orpheus-ssh-transfer-total transfer) size))
         (setf (orpheus-ssh-instance-size-failed ssh) t)))
     t)))

(defun orpheus-ssh--transfer-detail (transfer)
  "Return the state, completion and speed shown for TRANSFER."
  (pcase (orpheus-ssh-transfer-state transfer)
         ('queued "queued")
         ('downloading
          (let ((percent (orpheus-ssh--transfer-percent transfer))
                (speed (orpheus-ssh--transfer-speed transfer))
                (eta (orpheus-ssh--transfer-eta transfer)))
            (format "downloading %s %s %s ETA %s"
                    (if percent (format "%d%%" percent) "--%")
                    (if speed
                        (format "%s/s"
                                (file-size-human-readable (round speed) 'iec))
                      "0B/s")
                    (file-size-human-readable
                     (orpheus-ssh-transfer-bytes transfer) 'iec)
                    (if eta (orpheus-format-time eta) "--:--"))))
         ('done (format "ready %s"
                        (file-size-human-readable
                         (orpheus-ssh-transfer-bytes transfer))))
         ('failed "failed")
         (_ "cancelled")))

(defun orpheus-ssh--download-items (ssh)
  "Return SSH's transfers as rows for the downloads tab."
  (mapcar
   (lambda (transfer)
     (let* ((uri (orpheus-ssh-transfer-uri transfer))
            (state (orpheus-ssh-transfer-state transfer))
            (bytes (orpheus-ssh-transfer-bytes transfer)))
       (orpheus-item-create
        :label (file-name-nondirectory (directory-file-name uri))
        :detail (orpheus-ssh--transfer-detail transfer)
        :kind 'download
        :data (list (cons 'uri uri)
                    (cons 'state state)
                    (cons 'bytes bytes)
                    (cons 'total (orpheus-ssh-transfer-total transfer))
                    (cons 'percent (orpheus-ssh--transfer-percent transfer))
                    (cons 'speed (orpheus-ssh--transfer-speed transfer))
                    (cons 'eta (orpheus-ssh--transfer-eta transfer))
                    (cons 'path (orpheus-ssh-transfer-path transfer))))))
   (orpheus-ssh-instance-transfers ssh)))

(defun orpheus-ssh--transfer-for-uri (ssh uri)
  "Return SSH's transfer for URI, if there is one."
  (cl-find-if (lambda (transfer)
                (equal uri (orpheus-ssh-transfer-uri transfer)))
              (orpheus-ssh-instance-transfers ssh)))

(defun orpheus-ssh--pump (ssh)
  "Start queued transfers on SSH until the concurrency is reached."
  (let ((slots (max 1 orpheus-ssh-concurrency)))
    (cl-loop while (> slots 0)
             for transfer = (cl-find-if
                             (lambda (candidate)
                               (eq (orpheus-ssh-transfer-state candidate)
                                   'queued))
                             (orpheus-ssh-instance-transfers ssh))
             while transfer
             do (orpheus-ssh--start-transfer ssh transfer)
             (setq slots (1- slots)))))

(defun orpheus-ssh--clear-transfer-work (transfer)
  "Delete TRANSFER's sftp batch file and stop its size poll."
  (let ((timer (orpheus-ssh-transfer-timer transfer))
        (batch (orpheus-ssh-transfer-batch transfer)))
    (when (timerp timer) (cancel-timer timer))
    (when (and batch (file-exists-p batch))
      (ignore-errors (delete-file batch)))
    (setf (orpheus-ssh-transfer-timer transfer) nil
          (orpheus-ssh-transfer-batch transfer) nil)))

(defun orpheus-ssh--start-transfer (ssh transfer)
  "Start fetching TRANSFER for SSH."
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-ssh--cache-file ssh uri)))
    (setf (orpheus-ssh-transfer-path transfer) path
          (orpheus-ssh-transfer-state transfer) 'downloading
          (orpheus-ssh-transfer-bytes transfer) 0
          (orpheus-ssh-transfer-total transfer) nil
          (orpheus-ssh-transfer-started transfer) (float-time)
          (orpheus-ssh-transfer-samples transfer) nil
          (orpheus-ssh-transfer-retried transfer) nil)
    (condition-case err
        (progn
          (make-directory (file-name-directory path) t)
          (if (orpheus-ssh-instance-cat-failed ssh)
              (orpheus-ssh--start-sftp ssh transfer)
            (orpheus-ssh--start-cat ssh transfer))
          (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
            (orpheus-ssh--request-size ssh transfer)))
      (error
       (orpheus-ssh--clear-transfer-work transfer)
       (orpheus-ssh--fail-transfer
        ssh transfer (error-message-string err))))))

(defun orpheus-ssh--start-cat (ssh transfer)
  "Start the cat process that fetches TRANSFER for SSH."
  (condition-case err
      (let* ((uri (orpheus-ssh-transfer-uri transfer))
             (path (orpheus-ssh-transfer-path transfer))
             (host (orpheus-ssh--host))
             (process
              (progn
                (with-temp-file path)
                (make-process
                 :name "orpheus-ssh-download"
                 :command (append
                           (list (orpheus-ssh--program))
                           orpheus-ssh-extra-args
                           (list host
                                 (orpheus-ssh--download-command uri)))
                 :buffer nil
                 :stderr (get-buffer-create "*orpheus ssh*")
                 :noquery t
                 :coding 'binary
                 :filter (lambda (_process chunk)
                           (orpheus-ssh--transfer-filter ssh transfer chunk))
                 :sentinel (lambda (process _event)
                             (orpheus-ssh--transfer-sentinel
                              ssh transfer process))))))
        (setf (orpheus-ssh-transfer-process transfer) process)
        (push process (orpheus-ssh-instance-processes ssh)))
    (error
     (orpheus-ssh--fail-transfer
      ssh transfer (error-message-string err)))))

(defun orpheus-ssh--start-sftp (ssh transfer)
  "Start the sftp fallback that fetches TRANSFER for SSH."
  (orpheus-ssh--clear-transfer-work transfer)
  (setf (orpheus-ssh-transfer-retried transfer) t
        (orpheus-ssh-transfer-bytes transfer) 0
        (orpheus-ssh-transfer-started transfer) (float-time)
        (orpheus-ssh-transfer-samples transfer) nil)
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-ssh-transfer-path transfer))
         (host (orpheus-ssh--host))
         (batch nil))
    (condition-case err
        (progn
          (setq batch (make-temp-file "orpheus-sftp-" nil ".batch"))
          (setf (orpheus-ssh-transfer-batch transfer) batch)
          (with-temp-file path)
          (with-temp-file batch
            (insert (orpheus-ssh--sftp-batch uri path)))
          (let ((process
                 (make-process
                  :name "orpheus-ssh-sftp"
                  :command (append (list (orpheus-ssh--sftp-program))
                                   orpheus-ssh-sftp-extra-args
                                   (list "-b" batch "--" host))
                  :buffer nil
                  :stderr (get-buffer-create "*orpheus ssh*")
                  :noquery t
                  :sentinel (lambda (process _event)
                              (orpheus-ssh--transfer-sentinel
                               ssh transfer process)))))
            (setf (orpheus-ssh-transfer-process transfer) process)
            (push process (orpheus-ssh-instance-processes ssh))
            (setf (orpheus-ssh-transfer-timer transfer)
                  (run-with-timer 0.2 0.2 #'orpheus-ssh--poll-sftp
                                  ssh transfer process))))
      (error
       (orpheus-ssh--clear-transfer-work transfer)
       (orpheus-ssh--fail-transfer
        ssh transfer (error-message-string err))))))

(defun orpheus-ssh--poll-sftp (_ssh transfer process)
  "Record the bytes sftp PROCESS has written for TRANSFER."
  (if (and (eq (orpheus-ssh-transfer-state transfer) 'downloading)
           (process-live-p process))
      (let ((path (orpheus-ssh-transfer-path transfer)))
        (when (file-readable-p path)
          (let ((size (ignore-errors
                        (file-attribute-size (file-attributes path)))))
            (when (and size (> size (orpheus-ssh-transfer-bytes transfer)))
              (setf (orpheus-ssh-transfer-bytes transfer) size)
              (orpheus-ssh--note-transfer-sample transfer)))))
    (let ((timer (orpheus-ssh-transfer-timer transfer)))
      (when (timerp timer) (cancel-timer timer))
      (setf (orpheus-ssh-transfer-timer transfer) nil))))

(defun orpheus-ssh--transfer-filter (ssh transfer chunk)
  "Write CHUNK into TRANSFER's cache file on SSH, then count it."
  (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
    (condition-case err
        (unless (string-empty-p chunk)
          (let ((coding-system-for-write 'binary))
            (write-region chunk nil (orpheus-ssh-transfer-path transfer)
                          t 'silent))
          (cl-incf (orpheus-ssh-transfer-bytes transfer)
                   (string-bytes chunk))
          (orpheus-ssh--note-transfer-sample transfer))
      (error
       (let ((process (orpheus-ssh-transfer-process transfer)))
         (setf (orpheus-ssh-transfer-state transfer) 'failed)
         (when (process-live-p process) (ignore-errors (delete-process process))))
       (orpheus-ssh--fail-transfer ssh transfer (error-message-string err))))))

(defun orpheus-ssh--transfer-sentinel (ssh transfer process)
  "Finish TRANSFER, one of SSH's downloads, after PROCESS exits."
  (unless (process-live-p process)
    (setf (orpheus-ssh-instance-processes ssh)
          (delq process (orpheus-ssh-instance-processes ssh))
          (orpheus-ssh-transfer-process transfer) nil)
    (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
      (cond
       ((and (eq (process-status process) 'exit)
             (zerop (process-exit-status process))
             (file-readable-p (orpheus-ssh-transfer-path transfer)))
        (orpheus-ssh--finish-transfer ssh transfer))
       ((and (eq (process-status process) 'exit)
             (not (zerop (process-exit-status process)))
             (not (orpheus-ssh-transfer-retried transfer)))
        (when (memq (process-exit-status process) '(126 127))
          (setf (orpheus-ssh-instance-cat-failed ssh) t))
        (orpheus-ssh--start-sftp ssh transfer))
       (t
        (orpheus-ssh--fail-transfer ssh transfer nil))))))

(defun orpheus-ssh--finish-transfer (ssh transfer)
  "Record TRANSFER as complete on SSH and call everyone waiting for it."
  (orpheus-ssh--clear-transfer-work transfer)
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-ssh-transfer-path transfer))
         (callbacks (orpheus-ssh-transfer-callbacks transfer))
         (size (and path (file-readable-p path)
                    (ignore-errors
                      (file-attribute-size (file-attributes path))))))
    (when (and size (> size (orpheus-ssh-transfer-bytes transfer)))
      (setf (orpheus-ssh-transfer-bytes transfer) size))
    (setf (orpheus-ssh-transfer-state transfer) 'done
          (orpheus-ssh-transfer-callbacks transfer) nil)
    (when (and (null (orpheus-ssh-transfer-total transfer))
               (> (orpheus-ssh-transfer-bytes transfer) 0))
      (setf (orpheus-ssh-transfer-total transfer)
            (orpheus-ssh-transfer-bytes transfer)))
    (puthash uri path (orpheus-ssh-instance-downloads ssh))
    (orpheus-ssh--touch-cache ssh uri)
    (when callbacks
      (orpheus-ssh--note-current ssh path))
    (dolist (callback callbacks)
      (ignore-errors (funcall callback path)))
    (orpheus-ssh--prune-cache ssh)
    (orpheus-ssh--pump ssh)))

(defun orpheus-ssh--fail-transfer (ssh transfer error)
  "Mark TRANSFER as failed on SSH, reporting ERROR when there is one."
  (orpheus-ssh--clear-transfer-work transfer)
  (let ((uri (orpheus-ssh-transfer-uri transfer))
        (path (orpheus-ssh-transfer-path transfer))
        (callbacks (orpheus-ssh-transfer-callbacks transfer)))
    (setf (orpheus-ssh-transfer-state transfer) 'failed
          (orpheus-ssh-transfer-callbacks transfer) nil)
    (when (and path (file-readable-p path))
      (ignore-errors (delete-file path)))
    (dolist (callback callbacks)
      (ignore-errors (funcall callback nil)))
    (message "orpheus: ssh could not download %s%s"
             uri (if error (format ": %s" error) "; see *orpheus ssh*"))
    (orpheus-ssh--pump ssh)))

(defun orpheus-ssh--download (ssh uri callback &optional prefetch)
  "Download URI on SSH, then call CALLBACK with the local copy.
When PREFETCH is non-nil the request is background work: it goes behind a
user's request and never becomes the current playing file."
  (let ((cached (gethash uri (orpheus-ssh-instance-downloads ssh)))
        (host (orpheus-ssh--host)))
    (cond
     ((and cached (file-readable-p cached))
      (orpheus-ssh--touch-cache ssh uri)
      (unless prefetch
        (orpheus-ssh--note-current ssh cached)
        (when callback (funcall callback cached))))
     ((null host)
      (unless prefetch
        (message "orpheus: set `orpheus-ssh-host' to the machine your music is on"))
      (when callback (funcall callback nil)))
     (t
      (let ((transfer (orpheus-ssh--transfer-for-uri ssh uri)))
        (cond
         ((and transfer
               (memq (orpheus-ssh-transfer-state transfer) '(queued downloading)))
          (when callback
            (push callback (orpheus-ssh-transfer-callbacks transfer)))
          (unless prefetch
            (setf (orpheus-ssh-instance-transfers ssh)
                  (cons transfer
                        (delq transfer (orpheus-ssh-instance-transfers ssh)))))
          (orpheus-ssh--pump ssh))
         ((and transfer
               (eq (orpheus-ssh-transfer-state transfer) 'done)
               (file-readable-p (orpheus-ssh-transfer-path transfer)))
          (orpheus-ssh--touch-cache ssh uri)
          (unless prefetch
            (orpheus-ssh--note-current ssh
                                       (orpheus-ssh-transfer-path transfer))
            (when callback
              (funcall callback (orpheus-ssh-transfer-path transfer)))))
         (t
          (when transfer
            (setf (orpheus-ssh-instance-transfers ssh)
                  (delq transfer (orpheus-ssh-instance-transfers ssh))))
          (let ((new (orpheus-ssh-transfer-create
                      :uri uri :state 'queued
                      :callbacks (when callback (list callback)))))
            (setf (orpheus-ssh-instance-transfers ssh)
                  (if prefetch
                      (append (orpheus-ssh-instance-transfers ssh) (list new))
                    (cons new (orpheus-ssh-instance-transfers ssh))))
            (orpheus-ssh--pump ssh)))))))))

(defun orpheus-ssh--prefetch (ssh)
  "Fetch the next `orpheus-ssh-prefetch' queue entries on SSH."
  (when (> (or orpheus-ssh-prefetch 0) 0)
    (let ((index (orpheus-local-instance-index ssh))
          (queue (orpheus-local-instance-queue ssh)))
      (cl-loop for offset from 1 to orpheus-ssh-prefetch
               for uri = (nth (+ index offset) queue)
               when uri do (orpheus-ssh--download ssh uri nil t)))))

(defun orpheus-ssh--cancel (ssh uri)
  "Cancel SSH's transfer for URI and forget a completed copy of it."
  (let ((transfer (orpheus-ssh--transfer-for-uri ssh uri))
        (path (or (gethash uri (orpheus-ssh-instance-downloads ssh))
                  (orpheus-ssh-transfer-path
                   (orpheus-ssh--transfer-for-uri ssh uri)))))
    (when transfer
      (let ((process (orpheus-ssh-transfer-process transfer)))
        (setf (orpheus-ssh-transfer-state transfer) 'cancelled)
        (when (process-live-p process) (ignore-errors (delete-process process)))
        (orpheus-ssh--clear-transfer-work transfer))
      (let ((callbacks (orpheus-ssh-transfer-callbacks transfer)))
        (setf (orpheus-ssh-transfer-callbacks transfer) nil)
        (dolist (callback callbacks)
          (ignore-errors (funcall callback nil))))
      (setf (orpheus-ssh-instance-transfers ssh)
            (delq transfer (orpheus-ssh-instance-transfers ssh))))
    (when (and path (file-readable-p path))
      (ignore-errors (delete-file path)))
    (remhash uri (orpheus-ssh-instance-downloads ssh))
    (setf (orpheus-ssh-instance-cache ssh)
          (delete uri (orpheus-ssh-instance-cache ssh)))
    (when (equal path (orpheus-ssh-instance-current-file ssh))
      (setf (orpheus-ssh-instance-current-file ssh) nil))
    (orpheus-ssh--pump ssh)))

(defun orpheus-ssh--cleanup (ssh)
  "Stop SSH's downloads and delete its temporary directory."
  (dolist (transfer (orpheus-ssh-instance-transfers ssh))
    (setf (orpheus-ssh-transfer-state transfer) 'cancelled))
  (dolist (process (orpheus-ssh-instance-processes ssh))
    (when (process-live-p process) (ignore-errors (delete-process process))))
  (setf (orpheus-ssh-instance-processes ssh) nil)
  (dolist (transfer (orpheus-ssh-instance-transfers ssh))
    (let ((process (orpheus-ssh-transfer-process transfer)))
      (when (process-live-p process) (ignore-errors (delete-process process)))
      (orpheus-ssh--clear-transfer-work transfer)
      (let ((callbacks (orpheus-ssh-transfer-callbacks transfer)))
        (setf (orpheus-ssh-transfer-callbacks transfer) nil)
        (dolist (callback callbacks)
          (ignore-errors (funcall callback nil))))))
  (setf (orpheus-ssh-instance-transfers ssh) nil)
  (clrhash (orpheus-ssh-instance-downloads ssh))
  (setf (orpheus-ssh-instance-cache ssh) nil)
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
  (orpheus-ssh--download
   ssh uri
   (lambda (path)
     (funcall callback path)
     (when path (orpheus-ssh--prefetch ssh)))))

(cl-defmethod orpheus-local--prefetch ((ssh orpheus-ssh))
  "Fetch the tracks that follow SSH's current one."
  (orpheus-ssh--prefetch ssh))

(defun orpheus-ssh--backend-downloads (ssh callback)
  "Return SSH's transfer rows to CALLBACK."
  (funcall callback (orpheus-ssh--download-items ssh)))

(cl-defmethod orpheus-backend-connected-p ((ssh orpheus-ssh))
  "Return non-nil when SSH has a host and its mpv is listening."
  (and (orpheus-ssh--host) (orpheus-local--ready-p ssh)))

(cl-defmethod orpheus-backend-connect ((ssh orpheus-ssh) callback)
  "Start mpv for SSH when a host is set, then call CALLBACK with it or nil."
  (if (orpheus-ssh--host)
      (orpheus-local--ensure
       ssh
       (lambda (running)
         (when running
           (orpheus-ssh--capabilities
            ssh
            (lambda (capabilities)
              (orpheus-ssh--report-capabilities ssh capabilities))))
         (funcall callback running)))
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
         ('search (orpheus-ssh--search ssh (car arg) callback))
         ('downloads (orpheus-ssh--backend-downloads ssh callback))
         (_ (funcall callback nil))))

(cl-defmethod orpheus-backend-downloads ((ssh orpheus-ssh) callback)
  "Return SSH's transfers as rows to CALLBACK."
  (orpheus-ssh--backend-downloads ssh callback))

(cl-defmethod orpheus-backend-cancel-download ((ssh orpheus-ssh) uri)
  "Cancel SSH's transfer for URI."
  (orpheus-ssh--cancel ssh uri))

(cl-defmethod orpheus-backend-album-art ((ssh orpheus-ssh) track callback)
  "Find cover art for TRACK in SSH's copy or remote folder, then call CALLBACK."
  (let* ((uri (orpheus-track-uri track))
         (file (and uri (gethash uri (orpheus-ssh-instance-downloads ssh))))
         (dir (and uri (file-name-directory uri))))
    (if (and file (file-readable-p file))
        (orpheus-local--album-art-file
         file
         (lambda (art)
           (if art
               (funcall callback art)
             (orpheus-ssh--folder-art ssh dir callback))))
      (orpheus-ssh--folder-art ssh dir callback))))

(cl-defmethod orpheus-backend-folder-art ((ssh orpheus-ssh) folder callback)
  "Find cover art for remote FOLDER on SSH, then call CALLBACK with a path."
  (orpheus-ssh--folder-art ssh folder callback))

(defun orpheus-ssh--playlist-root ()
  "Return the remote directory playlist URIs are relative to."
  (and orpheus-ssh-directory
       (not (string-empty-p orpheus-ssh-directory))
       orpheus-ssh-directory))

(defun orpheus-ssh--join-playlist-uri (uri)
  "Return URI below SSH's playlist root, unless it already names the root."
  (let ((root (orpheus-ssh--playlist-root)))
    (cond ((null root) uri)
          ((file-name-absolute-p uri) uri)
          ((string-prefix-p (file-name-as-directory root) uri) uri)
          (t (orpheus-ssh--join root uri)))))

(cl-defmethod orpheus-backend-portable-uri ((_ssh orpheus-ssh) uri)
  "Return URI relative to SSH's or the local library root when possible."
  (or (orpheus-backend--relative-uri uri (orpheus-ssh--playlist-root))
      (orpheus-backend--relative-uri uri orpheus-local-directory)
      uri))

(cl-defmethod orpheus-backend-playlist-uri ((_ssh orpheus-ssh) uri)
  "Return URI as a remote path for SSH, or URI when it is absolute."
  (cond ((null uri) nil)
        ((orpheus-backend--url-p uri) uri)
        ((file-name-absolute-p uri)
         (let ((relative (orpheus-backend--relative-uri
                          uri orpheus-local-directory)))
           (if relative
               (orpheus-ssh--join-playlist-uri relative)
             uri)))
        (t (orpheus-ssh--join-playlist-uri uri))))

(cl-defmethod orpheus-backend-capabilities ((_ssh orpheus-ssh))
  "Return what files on another machine can browse."
  '(queue downloads directories search))

(provide 'orpheus-ssh)

;;; orpheus-ssh.el ends here
