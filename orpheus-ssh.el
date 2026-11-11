;;; orpheus-ssh.el --- Music on another machine, played through mpv -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Some collections live on a machine that is not this one.  This backend
;;; lists a remote directory with ssh, copies the track you choose into
;;; `orpheus-cache-directory', and hands that copy to mpv.  There is no daemon
;;; on the far side and no tunnel: a listing, search or cover shares one
;;; multiplexed ssh connection, downloads keep connections of their own, and
;;; sftp takes over when the shell has no cat.  A listing or search that stays
;;; silent past `orpheus-ssh-command-timeout' is dropped instead of left to
;;; hang, which is what makes a dead link visible rather than merely slow.
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
;;;
;;; A disc kept as one file is listed as the tracks its cue sheet names, and
;;; the track you pick is cut out on the host with ffmpeg, so only its own
;;; minutes cross the link.  A host without ffmpeg sends the whole image, once
;;; for the disc, and mpv seeks inside the copy.

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

(defcustom orpheus-ssh-cache-count nil
  "Number of completed tracks kept in SSH's cache, or nil for no count limit.
The cache is kept across sessions in `orpheus-ssh-cache-directory', and
`orpheus-cache-max-bytes' bounds its total size; this option is a second,
stricter limit for a small disk."
  :type '(choice (const :tag "Only the size limit" nil) integer)
  :group 'orpheus)

(defcustom orpheus-ssh-cache-directory nil
  "Directory SSH keeps downloaded tracks in, or nil for the orpheus cache.
The default is a private folder inside `orpheus-cache-directory', which lives
in =~/.cache= and survives a reboot, so a track fetched yesterday plays again
without a second transfer."
  :type '(choice (const :tag "The orpheus cache" nil) directory)
  :group 'orpheus)

(defcustom orpheus-ssh-multiplex t
  "Whether SSH reuses one connection for browsing, searching and covers.
A full key exchange on a saturated link can take seconds; a master connection
answers a listing at once.  Downloads open connections of their own, so their
bytes never queue in front of a directory listing.  Native Windows OpenSSH
has no `ControlMaster', so orpheus leaves multiplexing off there.  When the
connection fails before it can be shared, orpheus retries the command
without multiplexing."
  :type 'boolean :group 'orpheus)

(defcustom orpheus-ssh-control-directory nil
  "Directory for SSH's multiplexed control sockets, or nil to choose one.
The socket name is `%C', which ssh expands to a 40-character hash, so the
directory must be short enough for a Unix-domain socket.  When nil, orpheus
uses a private directory under `$XDG_RUNTIME_DIR' or `/tmp'; if the configured
directory leaves no room, it falls back to one of those and says so."
  :type '(choice (const :tag "Choose a short directory" nil) directory)
  :group 'orpheus)

(defcustom orpheus-ssh-control-persist 600
  "Seconds SSH keeps a shared connection after its last command.
The value is passed as `ControlPersist'; nil leaves ssh's own default.  A
longer life saves a key exchange between two directory changes, while a
shorter one gives the connection back sooner."
  :type '(choice (const :tag "ssh's default" nil) integer)
  :group 'orpheus)

(defcustom orpheus-ssh-command-timeout 20
  "Seconds ssh may stay silent on a listing, search or cover command.
A silent command is killed and its caller is told there was no answer, so a
stalled link never leaves the interface loading forever.  Downloads are not
bound by this: a large track on a slow link takes as long as it takes.  Nil or
zero waits forever."
  :type '(choice (const :tag "Wait forever" nil) number)
  :group 'orpheus)

(defcustom orpheus-ssh-prefetch 2
  "Number of upcoming queue tracks fetched in the background.
Nil or zero turns prefetching off.  Prefetches use spare download slots and are
always behind a track the user asked to play."
  :type 'integer :group 'orpheus)

(defcustom orpheus-ssh-split-images t
  "Whether a track of a remote disc image is fetched as a byte range.
When the host has ffmpeg it cuts the track there and sends only its minutes.
When it has no ffmpeg, this backend reads only the bytes the track occupies
and cuts them here with the local ffmpeg, so a track of a 400 MB image costs
its own megabytes rather than the whole image.  The image is best a FLAC file
with a seek table; without one the position is estimated and corrected from
the first frame.  Nil copies the whole image once and lets mpv seek inside it."
  :type 'boolean :group 'orpheus)

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
  retried batch timer stage range)

(cl-defstruct (orpheus-ssh (:include orpheus-local)
                           (:constructor orpheus-ssh--create)
                           (:conc-name orpheus-ssh-instance-))
  "The mpv backend whose files come from another machine over ssh."
  (downloads (make-hash-table :test #'equal))
  (transfers nil)
  (processes nil)
  (cache nil)
  (images (make-hash-table :test #'equal))
  current-file
  control-failed
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

(defconst orpheus-ssh--control-path-limit 100
  "Longest ssh ControlPath orpheus builds, in characters.
OpenSSH keeps it in a Unix-domain socket address: macOS allows 104 bytes and
Linux 108, counting the terminating NUL, so 100 has room for both.")

(defconst orpheus-ssh--control-hash-length 40
  "Characters ssh substitutes for `%C' in a ControlPath.")

(defvar orpheus-ssh--control-directory nil
  "Directory holding SSH's multiplexed connection sockets.")

(defvar orpheus-ssh--control-owned nil
  "Non-nil when orpheus created the control directory and may delete it.")

(defvar orpheus-ssh--control-warned nil
  "Whether the missing-room warning has already been shown.")

(defun orpheus-ssh--control-path (directory)
  "Return the ControlPath pattern inside DIRECTORY."
  (expand-file-name "%C" directory))

(defun orpheus-ssh--control-path-length (directory)
  "Return the real length of the ControlPath inside DIRECTORY.
The pattern contains `%C', which ssh expands to a 40-character hash."
  (+ (- (length (orpheus-ssh--control-path directory)) 2)
     orpheus-ssh--control-hash-length))

(defun orpheus-ssh--control-path-fits-p (directory)
  "Return non-nil when SSH's socket under DIRECTORY fits the platform limit."
  (<= (orpheus-ssh--control-path-length directory)
      orpheus-ssh--control-path-limit))

(defun orpheus-ssh--make-control-directory (parent)
  "Make a private temporary directory under PARENT, if its socket fits."
  (condition-case nil
      (let* ((temporary-file-directory (file-name-as-directory parent))
             (directory (make-temp-file "orpheus-" t)))
        (if (orpheus-ssh--control-path-fits-p directory)
            directory
          (delete-directory directory)
          nil))
    (error nil)))

(defun orpheus-ssh--automatic-control-directory ()
  "Return a fresh short control directory, or nil when none has room."
  (let ((runtime (getenv "XDG_RUNTIME_DIR")))
    (catch 'found
      (dolist (parent
               (delq nil
                     (list (and runtime
                                (not (string-empty-p runtime))
                                (file-directory-p runtime)
                                (file-name-as-directory runtime))
                           (and (not (memq system-type
                                           '(windows-nt ms-dos)))
                                (file-directory-p "/tmp")
                                (file-name-as-directory "/tmp"))
                           temporary-file-directory)))
        (let ((directory (orpheus-ssh--make-control-directory parent)))
          (when directory
            (setq orpheus-ssh--control-owned t)
            (throw 'found directory)))))))

(defun orpheus-ssh--new-control-directory ()
  "Return a configured or automatic control directory with room, or nil."
  (setq orpheus-ssh--control-owned nil)
  (let ((configured orpheus-ssh-control-directory))
    (if (not configured)
        (orpheus-ssh--automatic-control-directory)
      (let ((directory (expand-file-name configured)))
        (condition-case err
            (progn
              (unless (file-directory-p directory)
                (make-directory directory t))
              (if (orpheus-ssh--control-path-fits-p directory)
                  directory
                (message
                 "orpheus: `%s' is too long for an ssh control socket; finding a shorter directory"
                 configured)
                (orpheus-ssh--automatic-control-directory)))
          (error
           (message "orpheus: cannot use `%s': %s"
                    configured (error-message-string err))
           (orpheus-ssh--automatic-control-directory)))))))

(defun orpheus-ssh--control-args (&optional ssh)
  "Return the ssh arguments that share one connection, or nil.
Only the commands that answer a small question use them, so a listing never
waits behind a download's bytes.  Native Windows OpenSSH has no
`ControlMaster', so it is left out there.  SSH is also left out after its
control socket failed, so a broken setup does not fail every command twice."
  (when (and orpheus-ssh-multiplex
             (not (eq system-type 'windows-nt))
             (orpheus-ssh--host)
             (not (and ssh
                       (orpheus-ssh-instance-control-failed ssh))))
    (unless orpheus-ssh--control-directory
      (setq orpheus-ssh--control-directory
            (orpheus-ssh--new-control-directory)))
    (if (not orpheus-ssh--control-directory)
        (unless orpheus-ssh--control-warned
          (setq orpheus-ssh--control-warned t)
          (message
           "orpheus: no directory has room for ssh's control socket; multiplexing is off.  Set `orpheus-ssh-control-directory'.")
          nil)
      (setq orpheus-ssh--control-warned nil)
      (append
       (list "-o" "ControlMaster=auto"
             "-o" (concat "ControlPath="
                          (orpheus-ssh--control-path
                           orpheus-ssh--control-directory)))
       (when orpheus-ssh-control-persist
         (list "-o" (format "ControlPersist=%s"
                            orpheus-ssh-control-persist)))))))

(defun orpheus-ssh--close-multiplex ()
  "Ask SSH's shared connection to exit and forget its socket."
  (let ((directory orpheus-ssh--control-directory)
        (owned orpheus-ssh--control-owned)
        (host (orpheus-ssh--host)))
    (setq orpheus-ssh--control-directory nil
          orpheus-ssh--control-owned nil)
    (when (and directory (file-directory-p directory))
      (let ((process nil))
        (when host
          (setq process
                (ignore-errors
                  (make-process
                   :name "orpheus-ssh-close"
                   :command (list (orpheus-ssh--program)
                                  "-o" "ControlMaster=no"
                                  "-o" (concat
                                         "ControlPath="
                                         (orpheus-ssh--control-path directory))
                                  "-O" "exit" host)
                   :buffer nil :noquery t
                   :sentinel
                   (lambda (process _event)
                     (unless (process-live-p process)
                       (when owned
                         (ignore-errors
                           (delete-directory directory t)))))))))
        (unless process
          (when owned
            (ignore-errors (delete-directory directory t))))))))

(defconst orpheus-ssh--speed-window 5.0
  "Seconds of byte samples that make up a transfer's rolling speed.")

(defconst orpheus-ssh--speed-samples 128
  "Maximum number of byte samples kept for one transfer.")

(defun orpheus-ssh--cut-p (ssh uri)
  "Return non-nil when SSH cuts URI's track out of a disc on the host.
The host needs ffmpeg for that.  Without it the whole image is copied and mpv
seeks inside the copy, so a disc still plays, it just costs its full size."
  (and (orpheus-track-range uri)
       (alist-get 'ffmpeg (orpheus-ssh-instance-capabilities ssh))
       t))

(defun orpheus-ssh--cut-command (uri)
  "Return the remote command that copies URI's track, and only it, to stdout.
The copy is remuxed, not re-encoded, so it keeps the disc's own audio; the
cover travels with it when the image carried one."
  (let* ((range (orpheus-track-range uri))
         (end (cdr range)))
    (format (concat "ffmpeg -nostdin -v error -ss %.3f%s -i %s"
                    " -map 0:a:0 -map %s -c copy -f matroska -")
            (car range)
            (if end (format " -t %.3f" (- end (car range))) "")
            (orpheus-ssh--shell-quote (orpheus-track-path uri))
            (orpheus-ssh--shell-quote "0:v:0?"))))

(defun orpheus-ssh--download-command (ssh uri)
  "Return the command that copies URI to standard output on SSH.
A track of a disc is cut on the host, so only its own minutes are sent."
  (if (orpheus-ssh--cut-p ssh uri)
      (orpheus-ssh--cut-command uri)
    (format "cat -- %s" (orpheus-ssh--shell-quote (orpheus-track-path uri)))))

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
  "Return the best image path in DIR from the remote `ls' OUTPUT, or nil.
A name from `orpheus-local-cover-names' wins over another image."
  (when dir
    (orpheus-local--art-best
     dir (mapcar (lambda (name) (orpheus-ssh--join dir name))
                 (split-string (or output "") "\n" t))
     nil)))

(defun orpheus-ssh--media-file-p (name)
  "Return non-nil when NAME looks like something mpv can play."
  (member (downcase (or (file-name-extension name) ""))
          orpheus-local-extensions))

;;; Listing and searching a remote directory

(defun orpheus-ssh--cue-names (output)
  "Return the cue sheets in a remote `ls -1p' OUTPUT."
  (let ((case-fold-search t))
    (cl-remove-if-not (lambda (name) (string-match-p "\\.cue\\'" name))
                      (split-string (or output "") "\n" t))))

(defun orpheus-ssh--fetch-cue-sheets (ssh dir names callback)
  "Call CALLBACK with the cue sheets SSH finds in NAMES below DIR.
Each sheet is one cat of its own, so a restricted shell with no loops, pipes
or command substitution still answers.  A sheet that cannot be read is left
out rather than failing the folder."
  (let ((sheets nil))
    (cl-labels
        ((next (remaining)
           (if (null remaining)
               (funcall callback (nreverse sheets))
             (let ((path (orpheus-ssh--join dir (car remaining))))
               (orpheus-ssh--run
                ssh (format "cat -- %s" (orpheus-ssh--shell-quote path))
                (lambda (text)
                  (when (and text (not (string-empty-p text)))
                    (push (cons path text) sheets))
                  (next (cdr remaining)))
                t)))))
      (next names))))

(defun orpheus-ssh--cue-for (name sheets)
  "Return the cue text in SHEETS that belongs to the file NAME, or nil.
The sheet is the one named after the file, with any case."
  (let ((sidecar (concat (file-name-base name) ".cue")))
    (cdr (cl-find-if (lambda (sheet)
                       (string-equal-ignore-case
                        (file-name-nondirectory (car sheet)) sidecar))
                     sheets))))

(defun orpheus-ssh--disc-items (dir name sheets)
  "Return the rows NAME contributes below DIR, one per cue track when it has one."
  (let ((path (orpheus-ssh--join dir name))
        (cue (orpheus-ssh--cue-for name sheets)))
    (if-let* ((tracks (and cue (orpheus-local--cue-tracks cue))))
             (mapcar
              (lambda (track)
                (let* ((start (plist-get track :start))
                       (end (plist-get track :end))
                       (uri (orpheus-track-range-uri path start end))
                       (item-track
                        (orpheus-local--remember-track
                         (orpheus-track-create
                          :uri uri
                          :title (plist-get track :title)
                          :artist (plist-get track :artist)
                          :album (plist-get track :album)
                          :trackno (plist-get track :number)
                          :duration (and end (- end start))))))
                  (orpheus-item-create
                   :label (format "%02d  %s" (or (plist-get track :number) 0)
                                  (or (plist-get track :title) name))
                   :detail (orpheus-format-time (and end (- end start)))
                   :kind 'track
                   :data (list (cons 'uri uri) (cons 'track item-track)))))
              tracks)
      (list (orpheus-item-create
             :label name :kind 'track
             :detail (upcase (or (file-name-extension name) "file"))
             :data (list (cons 'uri path)))))))

(defun orpheus-ssh--directory-items (dir output &optional sheets)
  "Return rows for DIR from the remote `ls' OUTPUT.
SHEETS are the cue sheets fetched for DIR, which turn a disc held as one file
into one row per track."
  (delq nil
        (mapcan
         (lambda (line)
           (cond
            ((string-suffix-p "/" line)
             (list (orpheus-item-create
                    :label line :kind 'directory :detail "remote"
                    :data (list (cons 'browse
                                      (cons 'directories
                                            (list (orpheus-ssh--join
                                                   dir line))))))))
            ((orpheus-ssh--media-file-p line)
             (orpheus-ssh--disc-items dir line sheets))))
         (split-string (or output "") "\n" t))))

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

(defun orpheus-ssh--search-paths (output)
  "Return the file paths in find OUTPUT."
  (mapcar (lambda (line) (string-remove-prefix "./" line))
          (split-string (or output "") "\n" t)))

(defun orpheus-ssh--listing-paths (output)
  "Return the file paths in recursive ls OUTPUT."
  (let ((dir nil)
        (paths nil))
    (dolist (line (split-string (or output "") "\n"))
      (cond
       ((string-empty-p line))
       ((string-suffix-p ":" line)
        (setq dir (string-remove-suffix ":" line)))
       ((string-suffix-p "/" line))
       (t
        (push (orpheus-ssh--join (if (equal dir ".") "" dir) line)
              paths))))
    (nreverse paths)))

(defun orpheus-ssh--search-sheets (ssh paths callback)
  "Fetch the cue sheets among PATHS over SSH, then call CALLBACK with them.
Each sheet is one cat, grouped by directory.  A library with no cue sheets
costs no extra command."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (path paths)
      (when (string-match-p "\\.cue\\'" path)
        (let ((dir (or (file-name-directory path) "")))
          (puthash dir
                   (cons (file-name-nondirectory path) (gethash dir groups))
                   groups))))
    (let ((dirs (hash-table-keys groups))
          (sheets nil))
      (cl-labels
          ((next (remaining)
             (if (null remaining)
                 (funcall callback (nreverse sheets))
               (let* ((dir (car remaining))
                      (names (gethash dir groups)))
                 (orpheus-ssh--fetch-cue-sheets
                  ssh dir names
                  (lambda (found)
                    (setq sheets (append found sheets))
                    (next (cdr remaining))))))))
        (if (null dirs)
            (funcall callback nil)
          (next dirs))))))

(defun orpheus-ssh--search-items (query output &optional sheets)
  "Return track rows from search OUTPUT matching QUERY.
OUTPUT is the newline separated result of `orpheus-ssh--search-command'.
SHEETS are the cue sheets fetched for the paths, so a song title matches its
track of a disc image."
  (when (and query (not (string-empty-p query)))
    (delq nil
          (mapcan
           (lambda (line)
             (let* ((path (string-remove-prefix "./" line))
                    (dir (or (file-name-directory path) ""))
                    (name (file-name-nondirectory path)))
               (when (orpheus-ssh--media-file-p path)
                 (orpheus-search-filter
                  (orpheus-ssh--disc-items dir name sheets)
                  query path))))
           (split-string (or output "") "\n" t)))))

(defun orpheus-ssh--search-listing-items (query output)
  "Return track rows from recursive ls OUTPUT matching QUERY."
  (orpheus-ssh--search-items
   query (string-join (orpheus-ssh--listing-paths output) "\n")))

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
         (let ((paths (orpheus-ssh--search-paths output)))
           (orpheus-ssh--search-sheets
            ssh paths
            (lambda (sheets)
              (funcall callback
                       (orpheus-ssh--search-items query output sheets)))))))
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
           (let ((paths (orpheus-ssh--listing-paths output)))
             (orpheus-ssh--search-sheets
              ssh paths
              (lambda (sheets)
                (funcall callback
                         (orpheus-ssh--search-items
                          query (string-join paths "\n") sheets))))))
       (setf (orpheus-ssh-instance-search-fallback ssh) t)
       (unless (orpheus-ssh-instance-search-warned ssh)
         (setf (orpheus-ssh-instance-search-warned ssh) t)
         (message "orpheus: remote search failed; browse the Directories tab instead"))
       (funcall callback nil)))
   t))

(defun orpheus-ssh--control-error-p (output)
  "Return non-nil when OUTPUT says ssh could not use its control socket."
  (let ((case-fold-search t))
    (and output
         (string-match-p
          (concat "ControlPath.*too long"
                  "\\|too long for Unix domain socket"
                  "\\|unix_listener:.*too long"
                  "\\|control socket.*too long"
                  "\\|ControlMaster.*not supported"
                  "\\|getsockname failed")
          output))))

(defun orpheus-ssh--run (ssh command callback &optional quiet retried)
  "Run COMMAND on SSH's host, then call CALLBACK with its output or nil.
When QUIET is non-nil, do not report a command that failed; the caller has a
more useful message.  An empty successful command yields an empty string.  A
command that stays silent for `orpheus-ssh-command-timeout' is killed and
answered with nil, so a slow link cannot hold a tab loading forever.  When ssh
cannot create a shared connection, the command is retried once without
multiplexing; a login or permission failure is reported and not retried."
  (if-let* ((host (orpheus-ssh--host)))
           (let* ((buffer (generate-new-buffer " *orpheus-ssh-command*"))
                  (stderr (generate-new-buffer " *orpheus-ssh-stderr*"))
                  (done nil)
                  (timed-out nil)
                  (timer nil)
                  (process nil)
                  (control (unless retried
                             (orpheus-ssh--control-args ssh)))
                  (finish
                   (lambda (success output)
                     (unless done
                       (setq done t)
                       (when (timerp timer) (cancel-timer timer))
                       (when (buffer-live-p buffer) (kill-buffer buffer))
                       (when (buffer-live-p stderr) (kill-buffer stderr))
                       (if success
                           (funcall callback output)
                         (funcall callback nil))))))
             (when (and (numberp orpheus-ssh-command-timeout)
                        (> orpheus-ssh-command-timeout 0))
               (setq timer
                     (run-at-time
                      orpheus-ssh-command-timeout nil
                      (lambda ()
                        (unless done
                          (setq timed-out t)
                          (when (process-live-p process)
                            (delete-process process))
                          (unless quiet
                            (message
                             "orpheus: ssh timed out after %ss: %s"
                             orpheus-ssh-command-timeout command))
                          (funcall finish nil nil))))))
             (condition-case err
                 (setq process
                       (make-process
                        :name "orpheus-ssh"
                        :command (append (list (orpheus-ssh--program))
                                         orpheus-ssh-extra-args
                                         control
                                         (list host command))
                        :buffer buffer
                        :stderr stderr
                        :noquery t
                        :sentinel
                        (lambda (process _event)
                          (unless (process-live-p process)
                            (let* ((success
                                    (and (eq (process-status process) 'exit)
                                         (zerop (process-exit-status process))))
                                   (output
                                    (and (buffer-live-p buffer)
                                         (with-current-buffer buffer
                                           (buffer-string))))
                                   (errors
                                    (and (buffer-live-p stderr)
                                         (with-current-buffer stderr
                                           (buffer-string)))))
                              (when (and errors (not (string-empty-p errors)))
                                (with-current-buffer (get-buffer-create
                                                      "*orpheus ssh*")
                                  (insert errors)))
                              (cond
                               ((and (not success) (not timed-out) control
                                     (orpheus-ssh--control-error-p errors))
                                (setq done t)
                                (when (timerp timer) (cancel-timer timer))
                                (when (buffer-live-p buffer) (kill-buffer buffer))
                                (when (buffer-live-p stderr) (kill-buffer stderr))
                                (unless (orpheus-ssh-instance-control-failed ssh)
                                  (setf (orpheus-ssh-instance-control-failed ssh) t)
                                  (unless quiet
                                    (message
                                     "orpheus: ssh multiplexing failed; retrying without it")))
                                (orpheus-ssh--run ssh command callback quiet t))
                               (t
                                (when (and (not success) (not timed-out)
                                           (not quiet))
                                  (message
                                   "orpheus: ssh could not run %s; see *orpheus ssh*"
                                   command))
                                (funcall finish success output))))))))
               (error
                (unless done
                  (unless quiet
                    (message "orpheus: cannot run ssh: %s"
                             (error-message-string err)))
                  (funcall finish nil nil)))))
    (message "orpheus: set `orpheus-ssh-host' to the machine your music is on")
    (funcall callback nil)))

;;; Checking the remote shell

(defconst orpheus-ssh--probe-commands
  '((cat . "cat -- /dev/null")
    (find . "find . -maxdepth 0 -print")
    (size . "ls -lL -- /")
    (ffmpeg . "ffmpeg -version >/dev/null 2>&1 && printf ok")
    (substitution . "printf '%s' \"$(printf ok)\"")
    (compound . "printf ok >/dev/null && printf ok"))
  "Remote commands that test the features SSH uses.")

(defun orpheus-ssh--capability-summary (capabilities)
  "Return one line describing CAPABILITIES."
  (format (concat "cat %s, find %s, ls sizes %s, ffmpeg %s, "
                  "command substitution %s, compound commands %s; "
                  "downloads use %s%s")
          (if (alist-get 'cat capabilities) "yes" "no")
          (if (alist-get 'find capabilities) "yes" "no")
          (if (alist-get 'size capabilities) "yes" "no")
          (if (alist-get 'ffmpeg capabilities) "yes" "no")
          (if (alist-get 'substitution capabilities) "yes" "no")
          (if (alist-get 'compound capabilities) "yes" "no")
          (if (alist-get 'cat capabilities) "cat" "sftp")
          (cond
           ((alist-get 'ffmpeg capabilities)
            ", disc tracks cut out of the image on the host")
           ((and orpheus-ssh-split-images (executable-find "ffmpeg"))
            ", disc tracks fetched by byte range and cut here")
           (t ", disc images whole"))))

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
    (ffmpeg . "ffmpeg")
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
        (insert (format "%-22s %s\n" "ffmpeg (local)"
                        (if (executable-find "ffmpeg") "yes" "no")))
        (insert (format "%-22s %s\n" "ffprobe (local)"
                        (if (executable-find "ffprobe") "yes" "no")))
        (insert (format "\nDownloads use %s.\n"
                        (if (alist-get 'cat capabilities) "cat" "sftp")))
        (insert
         (format "Disc tracks: %s.\n"
                 (cond
                  ((alist-get 'ffmpeg capabilities)
                   "cut on the host")
                  ((and orpheus-ssh-split-images (executable-find "ffmpeg"))
                   "fetched as a byte range and cut here")
                  (t "played from a cached whole image"))))))))

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
  "Call CALLBACK with DIR's rows on SSH, or nil when ssh fails.
A folder holding cue sheets reads each sheet with one cat, so a restricted
shell with no loops still turns a disc kept as one file into its tracks."
  (orpheus-ssh--run
   ssh (orpheus-ssh--directory-command dir)
   (lambda (output)
     (cond
      ((null output) (funcall callback nil))
      ((orpheus-ssh--cue-names output)
       (orpheus-ssh--fetch-cue-sheets
        ssh dir (orpheus-ssh--cue-names output)
        (lambda (sheets)
          (funcall callback
                   (orpheus-ssh--directory-items dir output sheets)))))
      (t (funcall callback (orpheus-ssh--directory-items dir output)))))))

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

(defun orpheus-ssh--cover-roots (dir)
  "Return DIR and its parent, the remote folders searched for a cover."
  (let* ((dir (file-name-as-directory dir))
         (parent (file-name-directory (directory-file-name dir))))
    (if (or (null parent) (equal parent dir))
        (list dir)
      (list dir parent))))

(defun orpheus-ssh--cover-command (dir)
  "Return the remote find that lists image files around DIR.
One command looks in DIR, its parent and DIR's child folders, so a slow link
is crossed once rather than once per folder.  Every word is quoted for the
remote shell, including the parentheses around the name tests."
  (format "find %s -maxdepth 2 -type f %s -print 2>/dev/null"
          (mapconcat #'orpheus-ssh--shell-quote
                     (orpheus-ssh--cover-roots dir) " ")
          (mapconcat #'orpheus-ssh--shell-quote
                     (orpheus-local--art-name-args) " ")))

(defun orpheus-ssh--folder-art (ssh dir callback)
  "Find cover art for remote DIR on SSH, then call CALLBACK with a local path.
DIR, its parent and one level of child folders are searched; when the host
has no find, the folders are listed instead."
  (cond
   ((null dir) (funcall callback nil))
   ((orpheus-ssh-instance-search-fallback ssh)
    (orpheus-ssh--folder-art-listing ssh dir callback))
   (t
    ;; Art is optional, so a failed search is silent; browsing the same
    ;; folder will report a host that is really down.
    (orpheus-ssh--run
     ssh (orpheus-ssh--cover-command dir)
     (lambda (output)
       (let ((remote (and output
                          (orpheus-local--art-best
                           dir (split-string output "\n" t) nil))))
         (if remote
             (orpheus-ssh--fetch-cover ssh remote callback)
           (orpheus-ssh--folder-art-listing ssh dir callback))))
     t))))

(defun orpheus-ssh--folder-art-listing (ssh dir callback)
  "Find a cover for DIR by listing it and then its parent over SSH."
  (orpheus-ssh--run
   ssh (orpheus-ssh--directory-command dir)
   (lambda (output)
     (let ((remote (and output (orpheus-ssh--cover-file dir output))))
       (if remote
           (orpheus-ssh--fetch-cover ssh remote callback)
         (let* ((dir (file-name-as-directory dir))
                (parent (file-name-directory (directory-file-name dir))))
           (if (or (null parent) (equal parent dir))
               (funcall callback nil)
             (orpheus-ssh--run
              ssh (orpheus-ssh--directory-command parent)
              (lambda (parent-output)
                (let ((remote (and parent-output
                                   (orpheus-ssh--cover-file
                                    parent parent-output))))
                  (if remote
                      (orpheus-ssh--fetch-cover ssh remote callback)
                    (funcall callback nil))))
              t))))))
   t))

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
                          (orpheus-ssh--control-args)
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

;;; Cutting a disc image without ffmpeg on the host

;; A host that offers no ffmpeg, or whose shell has no loops or pipes, can
;; still send only the minutes a track occupies: `dd' reads a byte range and
;; ffmpeg here turns that range into the track.  A FLAC file's seek table maps
;; a sample to the byte offset of the frame that starts there, so the range
;; begins where the track does.  Without a seek table the position comes from
;; the image's average bitrate, and the first frame's own timestamp corrects
;; it once the range is here.

(defconst orpheus-ssh--head-bytes 1048576
  "Bytes of a remote image read to find its FLAC metadata.")

(defun orpheus-ssh--flac-info (bytes)
  "Return the FLAC facts in BYTES, or nil when it is not a FLAC header.
The value is a plist with :rate, :samples, :meta-end, :seeks (each a
\(SAMPLE . OFFSET) seek point) and :streaminfo."
  (when (and (>= (length bytes) 42)
             (equal (substring bytes 0 4) "fLaC"))
    (let ((position 4)
          (last nil)
          (rate nil)
          (samples nil)
          (meta-end nil)
          (streaminfo nil)
          (seeks nil))
      (while (and (not last) (<= (+ position 4) (length bytes)))
        (let* ((header (substring bytes position (+ position 4)))
               (type (logand (aref header 0) 127))
               (length (orpheus-local--bytes-uint header 1 3))
               (body-start (+ position 4))
               (body-end (+ body-start length)))
          (when (and (= type 0) (>= (length bytes) (+ body-start 34)))
            (setq streaminfo (substring bytes body-start (+ body-start 34))
                  rate (+ (* (aref streaminfo 10) 4096)
                          (* (aref streaminfo 11) 16)
                          (/ (aref streaminfo 12) 16))
                  samples (logior (ash (logand (aref streaminfo 13) 15) 32)
                                  (orpheus-local--bytes-uint streaminfo 14 4))))
          (when (= type 3)
            (let ((point body-start)
                  (limit (min body-end (length bytes))))
              (while (<= (+ point 18) limit)
                (let ((sample (orpheus-local--bytes-uint bytes point 8))
                      (offset (orpheus-local--bytes-uint bytes (+ point 8) 8)))
                  ;; A placeholder point is all ones and names no frame.
                  (unless (= sample (1- (ash 1 64)))
                    (push (cons sample offset) seeks)))
                (setq point (+ point 18)))))
          (setq last (/= 0 (logand (aref header 0) 128))
                position body-end)
          (when last (setq meta-end position))))
      (when (and rate (> rate 0) meta-end (> meta-end 0))
        (list :rate rate
              :samples samples
              :meta-end meta-end
              :seeks (sort (nreverse seeks) (lambda (a b) (< (car a) (car b))))
              :streaminfo streaminfo)))))

(defun orpheus-ssh--flac-header (streaminfo)
  "Return a minimal FLAC header holding only STREAMINFO.
The picture and seek table of the image point at offsets a cut range no
longer has, and only STREAMINFO is needed to decode the frames."
  (when (and (stringp streaminfo) (= (length streaminfo) 34))
    (concat "fLaC" (unibyte-string 128 0 0 34) streaminfo)))

(defun orpheus-ssh--split-plan (info range)
  "Return how to fetch the track RANGE holds in the FLAC image INFO.
The value is a plist of :start (the first byte), :length, :offset (seconds
from the first frame to the track), :duration, :cue-start, :exact and
:streaminfo, or nil when the range cannot be planned.  :exact is non-nil when
the seek table placed the first frame; otherwise :offset is nil and the first
frame's timestamp is read once the range is here."
  (let* ((rate (plist-get info :rate))
         (samples (plist-get info :samples))
         (size (plist-get info :size))
         (meta-end (plist-get info :meta-end))
         (streaminfo (plist-get info :streaminfo))
         (start (car range)))
    (when (and (numberp rate) (> rate 0)
               (numberp samples) (> samples 0)
               (numberp size) (> size 0)
               (numberp meta-end) (> meta-end 0)
               (stringp streaminfo)
               (numberp start))
      (let* ((duration (/ (float samples) rate))
             (end (min duration (or (cdr range) duration)))
             (seeks (plist-get info :seeks)))
        (when (< start end)
          (let ((start-sample (round (* start rate)))
                (end-sample (round (* end rate))))
            (if seeks
                (let ((pre nil))
                  (dolist (point seeks)
                    (when (<= (car point) start-sample)
                      (setq pre point)))
                  (setq pre (or pre (car seeks)))
                  (let* ((post (cl-find-if (lambda (point)
                                             (>= (car point) end-sample))
                                           seeks))
                         (first (+ meta-end (cdr pre)))
                         (last (max first
                                    (min size
                                         (if post
                                             (+ meta-end (cdr post))
                                           size)))))
                    (when (> last first)
                      (list :start first
                            :length (- last first)
                            :offset (max 0 (- start
                                               (/ (car pre) (float rate))))
                            :duration (- end start)
                            :cue-start start
                            :exact t
                            :streaminfo streaminfo))))
              ;; No seek table: estimate from the average bitrate, keep a
              ;; margin on both sides, and read the first frame's timestamp.
              (let* ((audio (- size meta-end))
                     (margin (max (* 2 1024 1024)
                                  (round (* audio 0.03))))
                     (first (max meta-end
                                 (- (+ meta-end
                                       (round (* audio (/ start duration))))
                                    margin)))
                     (last (min size
                                (+ (+ meta-end
                                      (round (* audio (/ end duration))))
                                   margin)))
                     (last (max first last)))
                (when (> last first)
                  (list :start first
                        :length (- last first)
                        :offset nil
                        :duration (- end start)
                        :cue-start start
                        :exact nil
                        :streaminfo streaminfo))))))))))

(defun orpheus-ssh--range-command (path start length)
  "Return the remote command that copies LENGTH bytes of PATH from START."
  (format "dd if=%s iflag=skip_bytes,count_bytes bs=1M skip=%d count=%d"
          (orpheus-ssh--shell-quote path) start length))

(defun orpheus-ssh--head-command (path)
  "Return the remote command that copies the first MiB of PATH."
  (format "dd if=%s bs=1M count=1" (orpheus-ssh--shell-quote path)))

(defun orpheus-ssh--region-file (path)
  "Return the file PATH's fetched image range is assembled in."
  (concat path ".region"))

(defun orpheus-ssh--split-p (ssh uri)
  "Return non-nil when SSH cuts URI's track here from a range of its image.
The host must lack ffmpeg, the image must be a FLAC file, and this machine
must have ffmpeg; `orpheus-ssh-split-images' turns the whole path off."
  (and orpheus-ssh-split-images
       (orpheus-track-range uri)
       (not (orpheus-ssh--cut-p ssh uri))
       (equal (downcase (or (file-name-extension (orpheus-track-path uri)) ""))
              "flac")
       (executable-find "ffmpeg")))

(defun orpheus-ssh--trim-p (ssh uri)
  "Return non-nil when SSH's copy of URI is the track, not the whole image."
  (or (orpheus-ssh--cut-p ssh uri)
      (orpheus-ssh--split-p ssh uri)))

(defun orpheus-ssh--fetch-head (ssh path callback)
  "Read the first MiB of PATH over SSH, then call CALLBACK with its bytes."
  (let ((buffer (generate-new-buffer " *orpheus-ssh-head*")))
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (condition-case nil
        (make-process
         :name "orpheus-ssh-head"
         :command (append (list (orpheus-ssh--program))
                          orpheus-ssh-extra-args
                          (orpheus-ssh--control-args ssh)
                          (list (orpheus-ssh--host)
                                (orpheus-ssh--head-command path)))
         :buffer buffer
         :stderr (get-buffer-create "*orpheus ssh*")
         :coding 'binary
         :noquery t
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((ok (and (eq (process-status process) 'exit)
                            (zerop (process-exit-status process))))
                   (bytes (and (buffer-live-p buffer)
                               (with-current-buffer buffer (buffer-string)))))
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (funcall callback (and ok bytes))))))
      (error
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (funcall callback nil)))))

(defun orpheus-ssh--image-info (ssh path callback)
  "Call CALLBACK with the FLAC facts SSH reads about PATH, or nil.
The size and the first MiB are read once per image and remembered for the
session; a path that is not a FLAC image is remembered too, so its tracks
fall back without another round trip."
  (let* ((images (orpheus-ssh-instance-images ssh))
         (cached (gethash path images)))
    (cond
     ((eq cached 'unsupported) (funcall callback nil))
     (cached (funcall callback cached))
     (t
      (orpheus-ssh--run
       ssh (orpheus-ssh--size-command path)
       (lambda (output)
         (let ((size (orpheus-ssh--parse-size output)))
           (if (null size)
               (progn
                 (puthash path 'unsupported images)
                 (funcall callback nil))
             (orpheus-ssh--fetch-head
              ssh path
              (lambda (bytes)
                (let ((info (and bytes (orpheus-ssh--flac-info bytes))))
                  (when info
                    (setq info (plist-put info :size size)))
                  (puthash path (or info 'unsupported) images)
                  (funcall callback info)))))))
       t)))))

(defun orpheus-ssh--start-split (ssh transfer)
  "For SSH, plan TRANSFER's byte range, then fetch and cut it here."
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-track-path uri))
         (range (orpheus-track-range uri)))
    (setf (orpheus-ssh-transfer-stage transfer) 'planning)
    (orpheus-ssh--image-info
     ssh path
     (lambda (info)
       (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
         (let ((plan (and info (orpheus-ssh--split-plan info range))))
           (cond
            ((null plan)
             (orpheus-ssh--fallback-range ssh transfer))
            ((and (not (plist-get plan :exact))
                  (null (executable-find "ffprobe")))
             (orpheus-ssh--fallback-range ssh transfer))
            (t
             (orpheus-ssh--start-range ssh transfer plan)))))))))

(defun orpheus-ssh--start-range (ssh transfer plan)
  "Fetch the byte range PLAN names for SSH's TRANSFER into its region file."
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-ssh-transfer-path transfer))
         (region (orpheus-ssh--region-file path))
         (header (orpheus-ssh--flac-header (plist-get plan :streaminfo))))
    (setf (orpheus-ssh-transfer-range transfer) plan
          (orpheus-ssh-transfer-total transfer) (plist-get plan :length)
          (orpheus-ssh-transfer-bytes transfer) 0
          (orpheus-ssh-transfer-started transfer) (float-time)
          (orpheus-ssh-transfer-samples transfer) nil
          (orpheus-ssh-transfer-stage transfer) 'fetching)
    (let ((coding-system-for-write 'binary))
      (with-temp-file region
        (set-buffer-multibyte nil)
        (insert header)))
    (condition-case nil
        (let ((process
               (make-process
                :name "orpheus-ssh-range"
                :command (append
                          (list (orpheus-ssh--program))
                          orpheus-ssh-extra-args
                          (orpheus-ssh--control-args ssh)
                          (list (orpheus-ssh--host)
                                (orpheus-ssh--range-command
                                 (orpheus-track-path uri)
                                 (plist-get plan :start)
                                 (plist-get plan :length))))
                :buffer nil
                :stderr (get-buffer-create "*orpheus ssh*")
                :noquery t
                :coding 'binary
                :filter (lambda (_process chunk)
                          (orpheus-ssh--range-filter ssh transfer chunk))
                :sentinel (lambda (process _event)
                            (orpheus-ssh--range-sentinel
                             ssh transfer process)))))
          (setf (orpheus-ssh-transfer-process transfer) process)
          (push process (orpheus-ssh-instance-processes ssh)))
      (error
       (orpheus-ssh--fallback-range ssh transfer)))))

(defun orpheus-ssh--range-filter (ssh transfer chunk)
  "Append CHUNK, part of SSH's TRANSFER range, to its region file."
  (when (and (eq (orpheus-ssh-transfer-state transfer) 'downloading)
             (not (string-empty-p chunk)))
    (condition-case err
        (let ((coding-system-for-write 'binary)
              (path (orpheus-ssh--region-file
                     (orpheus-ssh-transfer-path transfer))))
          (write-region chunk nil path t 'silent)
          (cl-incf (orpheus-ssh-transfer-bytes transfer) (string-bytes chunk))
          (orpheus-ssh--note-transfer-sample transfer))
      (error
       (let ((process (orpheus-ssh-transfer-process transfer)))
         (setf (orpheus-ssh-transfer-state transfer) 'failed)
         (when (process-live-p process) (ignore-errors (delete-process process))))
       (orpheus-ssh--fail-transfer ssh transfer (error-message-string err))))))

(defun orpheus-ssh--range-sentinel (ssh transfer process)
  "Finish SSH's TRANSFER range fetch after PROCESS exits, then cut its track."
  (unless (process-live-p process)
    (setf (orpheus-ssh-instance-processes ssh)
          (delq process (orpheus-ssh-instance-processes ssh))
          (orpheus-ssh-transfer-process transfer) nil)
    (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
      (if (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process))
               (> (orpheus-ssh-transfer-bytes transfer) 0))
          (orpheus-ssh--cut-range ssh transfer)
        (orpheus-ssh--fallback-range ssh transfer)))))

(defun orpheus-ssh--cut-range (ssh transfer)
  "Cut SSH's fetched region into the track TRANSFER holds."
  (let* ((plan (orpheus-ssh-transfer-range transfer))
         (path (orpheus-ssh-transfer-path transfer))
         (region (orpheus-ssh--region-file path))
         (part (orpheus-ssh--part-file path)))
    (setf (orpheus-ssh-transfer-stage transfer) 'cutting)
    (if (plist-get plan :exact)
        (orpheus-ssh--cut-region ssh transfer region part
                                 (plist-get plan :offset)
                                 (plist-get plan :duration))
      (orpheus-ssh--probe-region
       region
       (lambda (start-time)
         (when (eq (orpheus-ssh-transfer-state transfer) 'downloading)
           (if (and start-time
                    (<= start-time (plist-get plan :cue-start)))
               (orpheus-ssh--cut-region
                ssh transfer region part
                (- (plist-get plan :cue-start) start-time)
                (plist-get plan :duration))
             (orpheus-ssh--fallback-range ssh transfer))))))))

(defun orpheus-ssh--probe-region (region callback)
  "Call CALLBACK with the time of REGION's first frame, or nil."
  (let ((buffer (generate-new-buffer " *orpheus-ffprobe*"))
        (done nil))
    (cl-labels
        ((finish (value)
           (unless done
             (setq done t)
             (when (buffer-live-p buffer) (kill-buffer buffer))
             (funcall callback value))))
      (condition-case nil
          (make-process
           :name "orpheus-ssh-ffprobe"
           :command (list (executable-find "ffprobe")
                          "-v" "error" "-select_streams" "a:0"
                          "-show_entries" "frame=pts_time"
                          "-of" "csv=p=0"
                          "-read_intervals" "%+#1" region)
           :buffer buffer
           :noquery t
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (finish
                (and (eq (process-status process) 'exit)
                     (zerop (process-exit-status process))
                     (with-current-buffer buffer
                       (when-let* ((line (car (split-string
                                               (buffer-string) "\n" t))))
                         (and (string-match-p "\\`[0-9]" line)
                              (string-to-number line)))))))))
        (error (finish nil))))))

(defun orpheus-ssh--tag-arguments (track)
  "Return ffmpeg metadata arguments for TRACK, or nil."
  (when track
    (append
     (when-let* ((title (orpheus-track-title track)))
               (unless (string-empty-p title)
                 (list "-metadata" (concat "title=" title))))
     (when-let* ((artist (orpheus-track-artist track)))
               (unless (string-empty-p artist)
                 (list "-metadata" (concat "artist=" artist))))
     (when-let* ((album (orpheus-track-album track)))
               (unless (string-empty-p album)
                 (list "-metadata" (concat "album=" album))))
     (when-let* ((number (orpheus-track-trackno track)))
               (when (and (numberp number) (> number 0))
                 (list "-metadata" (format "track=%d" number)))))))

(defun orpheus-ssh--region-cut-command (region part offset duration track)
  "Return the ffmpeg command that cuts REGION into PART for TRACK.
PART is one FLAC track, OFFSET is its start within REGION and DURATION its
length.  The format is named because PART ends in `.part', which ffmpeg
cannot use to pick a muxer."
  (append
   (list (executable-find "ffmpeg")
         "-nostdin" "-v" "error" "-y"
         "-i" region
         "-ss" (format "%.6f" (max 0 offset))
         "-t" (format "%.3f" duration)
         "-map" "0:a:0" "-c:a" "flac")
   (orpheus-ssh--tag-arguments track)
   (list "-f" "flac" part)))

(defun orpheus-ssh--cut-region (ssh transfer region part offset duration)
  "Cut REGION into PART as one FLAC track for SSH, then finish TRANSFER.
OFFSET is the track's start within REGION and DURATION its length."
  (let ((command
         (orpheus-ssh--region-cut-command
          region part offset duration
          (orpheus-local--known-track (orpheus-ssh-transfer-uri transfer)))))
    (condition-case nil
        (make-process
         :name "orpheus-ssh-cut"
         :command command
         :buffer nil
         :stderr (get-buffer-create "*orpheus ssh*")
         :noquery t
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((ok (and (eq (process-status process) 'exit)
                            (zerop (process-exit-status process))))
                   (size (and (file-readable-p part)
                              (ignore-errors
                                (file-attribute-size (file-attributes part))))))
               (ignore-errors (delete-file region))
               (if (not (eq (orpheus-ssh-transfer-state transfer) 'downloading))
                   nil
                 (if (and ok size (> size 0))
                     (progn
                       (setf (orpheus-ssh-transfer-bytes transfer) size
                             (orpheus-ssh-transfer-stage transfer) nil)
                       (orpheus-ssh--finish-transfer ssh transfer))
                   (when (file-readable-p part) (delete-file part))
                   (orpheus-ssh--fallback-range ssh transfer)))))))
      (error
       (orpheus-ssh--fallback-range ssh transfer)))))

(defun orpheus-ssh--fallback-range (ssh transfer)
  "Give up on SSH's TRANSFER range and copy the whole image instead."
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (old (orpheus-ssh-transfer-path transfer))
         (path (orpheus-ssh--cache-file ssh uri t)))
    (setf (orpheus-ssh-transfer-path transfer) path
          (orpheus-ssh-transfer-stage transfer) nil
          (orpheus-ssh-transfer-range transfer) nil
          (orpheus-ssh-transfer-bytes transfer) 0
          (orpheus-ssh-transfer-total transfer) nil
          (orpheus-ssh-transfer-started transfer) (float-time)
          (orpheus-ssh-transfer-samples transfer) nil
          (orpheus-ssh-transfer-retried transfer) nil)
    (dolist (file (list (orpheus-ssh--region-file old)
                        (orpheus-ssh--part-file old)
                        (orpheus-ssh--part-file path)))
      (when (file-exists-p file) (ignore-errors (delete-file file))))
    (condition-case nil
        (progn
          (make-directory (file-name-directory path) t)
          (if (orpheus-ssh-instance-cat-failed ssh)
              (orpheus-ssh--start-sftp ssh transfer)
            (orpheus-ssh--start-cat ssh transfer))
          (orpheus-ssh--request-size ssh transfer))
      (error
       (orpheus-ssh--fail-transfer ssh transfer nil)))))

;;; Downloads and their queue

(defun orpheus-ssh--cache-root ()
  "Return the directory SSH keeps downloaded tracks in, without making it."
  (if orpheus-ssh-cache-directory
      (expand-file-name orpheus-ssh-cache-directory)
    (expand-file-name "ssh/" orpheus-cache-directory)))

(defun orpheus-ssh--cache-directory ()
  "Return the directory SSH keeps downloaded tracks in, making it."
  (let ((dir (orpheus-ssh--cache-root)))
    (make-directory dir t)
    dir))

(defun orpheus-ssh--cache-file (ssh uri &optional whole)
  "Return the cache path SSH keeps URI's copy at.
A track cut on the host or here is a file of its own; a whole image is cached
once, under its path, so every track of the disc shares it.  WHOLE asks for
that shared copy even when URI names one track of the image."
  (let* ((dir (orpheus-ssh--cache-root))
         (path (orpheus-track-path uri))
         (key (if (and (not whole) (orpheus-ssh--trim-p ssh uri)) uri path))
         (base (or (file-name-nondirectory (directory-file-name path))
                   "track"))
         (safe (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" base)))
    (expand-file-name
     (format "%s-%s" (substring (secure-hash 'sha1 key) 0 8) safe)
     dir)))

(defun orpheus-ssh--part-file (path)
  "Return the incomplete-download path that becomes PATH when it finishes."
  (concat path ".part"))

(defun orpheus-ssh--cached-file (ssh uri)
  "Return the local copy of URI SSH already has, or nil.
The copy may come from an earlier session: the cache lives in
`orpheus-cache-directory' rather than a temporary directory.  A track may also
find the whole image cached by an earlier fallback, which it then plays from
its own range."
  (let ((known (gethash uri (orpheus-ssh-instance-downloads ssh)))
        (trim (orpheus-ssh--cache-file ssh uri))
        (whole (orpheus-ssh--cache-file ssh uri t)))
    (cond
     ((and known (file-readable-p known)) known)
     ((and (file-readable-p trim)
           (> (or (ignore-errors
                    (file-attribute-size (file-attributes trim))) 0) 0))
      (puthash uri trim (orpheus-ssh-instance-downloads ssh))
      trim)
     ((and (not (equal trim whole))
           (file-readable-p whole)
           (> (or (ignore-errors
                    (file-attribute-size (file-attributes whole))) 0) 0))
      (puthash uri whole (orpheus-ssh-instance-downloads ssh))
      whole))))

(defun orpheus-ssh--touch-file (path)
  "Make PATH the most recently used file in the cache."
  (ignore-errors (set-file-times path)))

(defun orpheus-ssh--note-current (ssh path)
  "Remember PATH as the copy mpv is playing for SSH."
  (setf (orpheus-ssh-instance-current-file ssh) path))

(defun orpheus-ssh--touch-cache (ssh uri)
  "Move URI to the front of SSH's least-recently-used cache."
  (setf (orpheus-ssh-instance-cache ssh)
        (cons uri (delete uri (orpheus-ssh-instance-cache ssh)))))

(defun orpheus-ssh--prune-cache (ssh)
  "Keep SSH's cache within its count and size limits.
`orpheus-ssh-cache-count' is honoured when set, and the whole
`orpheus-cache-directory' is then trimmed to `orpheus-cache-max-bytes'.  The
playing track and any track still being fetched are never evicted."
  (when (integerp orpheus-ssh-cache-count)
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
  (orpheus-cache-prune-later
   (delq nil
         (cons (orpheus-ssh-instance-current-file ssh)
               (mapcar #'orpheus-ssh-transfer-path
                       (cl-remove-if-not
                        (lambda (transfer)
                          (memq (orpheus-ssh-transfer-state transfer)
                                '(queued downloading)))
                        (orpheus-ssh-instance-transfers ssh)))))))

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
     ssh (orpheus-ssh--size-command
          (orpheus-track-path (orpheus-ssh-transfer-uri transfer)))
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
          (if (eq (orpheus-ssh-transfer-stage transfer) 'cutting)
              (format "cutting %s locally"
                      (file-size-human-readable
                       (orpheus-ssh-transfer-bytes transfer) 'iec))
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
                      (if eta (orpheus-format-time eta) "--:--")))))
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
        :label (orpheus-ssh--transfer-label uri)
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
  "Delete TRANSFER's sftp batch and fetched range, and stop its size poll."
  (let ((timer (orpheus-ssh-transfer-timer transfer))
        (batch (orpheus-ssh-transfer-batch transfer))
        (path (orpheus-ssh-transfer-path transfer)))
    (when (timerp timer) (cancel-timer timer))
    (when (and batch (file-exists-p batch))
      (ignore-errors (delete-file batch)))
    (when path
      (let ((region (orpheus-ssh--region-file path)))
        (when (file-exists-p region) (ignore-errors (delete-file region)))))
    (setf (orpheus-ssh-transfer-timer transfer) nil
          (orpheus-ssh-transfer-batch transfer) nil)))

(defun orpheus-ssh--start-transfer (ssh transfer)
  "Start fetching TRANSFER for SSH.
A track of a disc is a byte range cut here when the host has no ffmpeg, or
cut on the host when it has one; a whole file is copied with cat, or sftp
when the shell cannot run it."
  (let* ((uri (orpheus-ssh-transfer-uri transfer))
         (path (orpheus-ssh--cache-file ssh uri)))
    (setf (orpheus-ssh-transfer-path transfer) path
          (orpheus-ssh-transfer-state transfer) 'downloading
          (orpheus-ssh-transfer-stage transfer) nil
          (orpheus-ssh-transfer-range transfer) nil
          (orpheus-ssh-transfer-bytes transfer) 0
          (orpheus-ssh-transfer-total transfer) nil
          (orpheus-ssh-transfer-started transfer) (float-time)
          (orpheus-ssh-transfer-samples transfer) nil
          (orpheus-ssh-transfer-retried transfer) nil)
    (condition-case err
        (progn
          (make-directory (file-name-directory path) t)
          (cond
           ((orpheus-ssh--split-p ssh uri)
            (orpheus-ssh--start-split ssh transfer))
           ((orpheus-ssh-instance-cat-failed ssh)
            (orpheus-ssh--start-sftp ssh transfer)
            (orpheus-ssh--request-size ssh transfer))
           (t
            (orpheus-ssh--start-cat ssh transfer)
            ;; A track cut on the host has no size to ask for.
            (when (and (eq (orpheus-ssh-transfer-state transfer) 'downloading)
                       (not (orpheus-ssh--cut-p ssh uri)))
              (orpheus-ssh--request-size ssh transfer)))))
      (error
       (orpheus-ssh--clear-transfer-work transfer)
       (orpheus-ssh--fail-transfer
        ssh transfer (error-message-string err))))))

(defun orpheus-ssh--start-cat (ssh transfer)
  "Start the cat process that fetches TRANSFER for SSH."
  (condition-case err
      (let* ((uri (orpheus-ssh-transfer-uri transfer))
             (path (orpheus-ssh--part-file
                    (orpheus-ssh-transfer-path transfer)))
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
                                 (orpheus-ssh--download-command ssh uri)))
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
         (path (orpheus-ssh--part-file
                (orpheus-ssh-transfer-path transfer)))
         (host (orpheus-ssh--host))
         (batch nil))
    (condition-case err
        (progn
          (setq batch (make-temp-file "orpheus-sftp-" nil ".batch"))
          (setf (orpheus-ssh-transfer-batch transfer) batch)
          (with-temp-file path)
          (with-temp-file batch
            (insert (orpheus-ssh--sftp-batch (orpheus-track-path uri) path)))
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
      (let ((path (orpheus-ssh--part-file
                   (orpheus-ssh-transfer-path transfer))))
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
          (let ((coding-system-for-write 'binary)
                (path (orpheus-ssh--part-file
                       (orpheus-ssh-transfer-path transfer))))
            (write-region chunk nil path t 'silent))
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
             (file-readable-p
              (orpheus-ssh--part-file (orpheus-ssh-transfer-path transfer))))
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
         (part (orpheus-ssh--part-file path))
         (callbacks (orpheus-ssh-transfer-callbacks transfer))
         (size nil))
    (when (file-readable-p part)
      (ignore-errors (rename-file part path t)))
    (setq size (and path (file-readable-p path)
                    (ignore-errors
                      (file-attribute-size (file-attributes path)))))
    (when (and size (> size (orpheus-ssh-transfer-bytes transfer)))
      (setf (orpheus-ssh-transfer-bytes transfer) size))
    (setf (orpheus-ssh-transfer-state transfer) 'done
          (orpheus-ssh-transfer-callbacks transfer) nil)
    (when (and (null (orpheus-ssh-transfer-total transfer))
               (> (orpheus-ssh-transfer-bytes transfer) 0))
      (setf (orpheus-ssh-transfer-total transfer)
            (orpheus-ssh-transfer-bytes transfer)))
    (puthash uri path (orpheus-ssh-instance-downloads ssh))
    (orpheus-ssh--touch-file path)
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
    (dolist (file (list path
                        (and path (orpheus-ssh--part-file path))
                        (and path (orpheus-ssh--region-file path))))
      (when (and file (file-readable-p file))
        (ignore-errors (delete-file file))))
    (dolist (callback callbacks)
      (ignore-errors (funcall callback nil)))
    (message "orpheus: ssh could not download %s%s"
             uri (if error (format ": %s" error) "; see *orpheus ssh*"))
    (orpheus-ssh--pump ssh)))

(defun orpheus-ssh--download (ssh uri callback &optional prefetch)
  "Download URI on SSH, then call CALLBACK with the local copy.
When PREFETCH is non-nil the request is background work: it goes behind a
user's request and never becomes the current playing file."
  (let ((cached (orpheus-ssh--cached-file ssh uri))
        (host (orpheus-ssh--host)))
    (cond
     ((and cached (file-readable-p cached))
      (orpheus-ssh--touch-file cached)
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
    (dolist (file (list path
                        (and path (orpheus-ssh--part-file path))
                        (and path (orpheus-ssh--region-file path))))
      (when (and file (file-readable-p file))
        (ignore-errors (delete-file file))))
    (remhash uri (orpheus-ssh-instance-downloads ssh))
    (setf (orpheus-ssh-instance-cache ssh)
          (delete uri (orpheus-ssh-instance-cache ssh)))
    (when (equal path (orpheus-ssh-instance-current-file ssh))
      (setf (orpheus-ssh-instance-current-file ssh) nil))
    (orpheus-ssh--pump ssh)))

(defun orpheus-ssh--cleanup (ssh)
  "Stop SSH's transfers and forget them, keeping the downloaded files."
  (orpheus-ssh--close-multiplex)
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
  (setf (orpheus-ssh-instance-current-file ssh) nil))

(defun orpheus-ssh--cleanup-all ()
  "Stop every SSH backend still alive before Emacs exits.
The downloaded files stay in the cache; a track played tomorrow is not
fetched again."
  (dolist (ssh orpheus-ssh--live)
    (ignore-errors (orpheus-ssh--cleanup ssh))))

(add-hook 'kill-emacs-hook #'orpheus-ssh--cleanup-all)

;;; Backend methods

(defun orpheus-ssh--played-path (ssh uri path)
  "Return what mpv should load for URI, a copy of it at PATH on SSH.
A track cut on the host, or cut here from a byte range, is already only its
own minutes.  A whole image, which a fallback sends, is played from the range
URI carries."
  (cond
   ((orpheus-ssh--cut-p ssh uri) path)
   ((and (orpheus-ssh--split-p ssh uri)
         (equal path (orpheus-ssh--cache-file ssh uri)))
    path)
   (t
    (let ((range (orpheus-track-range uri)))
      (if range
          (orpheus-track-range-uri path (car range) (cdr range))
        path)))))

(cl-defmethod orpheus-local--resolve-uri ((ssh orpheus-ssh) uri callback)
  "Download URI from SSH to a local copy, then call CALLBACK with its path."
  (orpheus-ssh--download
   ssh uri
   (lambda (path)
     (funcall callback (and path (orpheus-ssh--played-path ssh uri path)))
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
  "Stop SSH's mpv and its transfers, keeping the downloaded files."
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
  "Find cover art for TRACK in SSH's copy or remote folder, then call CALLBACK.
A cut track lives in the cache, whose folder holds every other album's cover,
so only a whole image is asked for the art inside it; a cut one is looked up
in its remote folder instead."
  (let* ((uri (orpheus-track-uri track))
         (file (and uri (gethash uri (orpheus-ssh-instance-downloads ssh))))
         (dir (and uri (file-name-directory (orpheus-track-path uri)))))
    (if (and (not (orpheus-ssh--trim-p ssh uri))
             file (file-readable-p file))
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

(defun orpheus-ssh--lyrics-command (uri)
  "Return the remote command that prints URI's sidecar lyrics, if any.
The loop tries the usual names in one round trip and prints nothing when the
track has no sidecar file."
  (let* ((base (file-name-sans-extension uri))
         (names (mapcar (lambda (extension) (concat base extension))
                        '(".lrc" ".LRC" ".txt" ".TXT"))))
    (format (concat "for f in %s; do if [ -f \"$f\" ]; then"
                    " cat -- \"$f\"; break; fi; done")
            (mapconcat #'orpheus-ssh--shell-quote names " "))))

(cl-defmethod orpheus-backend-lyrics ((ssh orpheus-ssh) track callback)
  "Read TRACK's sidecar lyrics on SSH's machine, then call CALLBACK with them."
  (let ((uri (orpheus-track-path (orpheus-track-uri track))))
    (if (or (null uri) (string-empty-p uri))
        (funcall callback nil)
      (orpheus-ssh--run
       ssh (orpheus-ssh--lyrics-command uri)
       (lambda (output)
         (funcall callback
                  (when (and output
                             (not (string-empty-p (string-trim output))))
                    (orpheus-lyrics--from-text output uri))))
       t))))

(cl-defmethod orpheus-backend-local-file ((ssh orpheus-ssh) track)
  "Return the local copy of TRACK that SSH has, or nil."
  (let ((uri (orpheus-track-uri track)))
    (and uri (orpheus-ssh--cached-file ssh uri))))

(defun orpheus-ssh--transfer-label (uri)
  "Return the row label for URI, a transfer's track."
  (let ((range (orpheus-track-range uri))
        (name (file-name-nondirectory (orpheus-track-path uri))))
    (if range
        (format "%s [%s]" name (orpheus-format-time (car range)))
      name)))

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
