;;; orpheus-spotify.el --- Spotify Web API backend -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Spotify plays the music itself, on a phone, a desktop app or a small
;;; daemon such as spotifyd, and the Web API is how another program tells it
;;; what to do.  So this backend is shaped like the MPD one: a remote player
;;; owns the audio output and the queue, and orpheus is the interface, the
;;; library browser and the set of transport keys.
;;;
;;; Two consequences are worth knowing before reading the code.  Playback
;;; control needs a Premium account, and something has to count as an active
;;; device; a command sent with no device playing fails with Spotify's own
;;; words, which are shown as they come.  And nothing is streamed to Emacs:
;;; the Web API moves queue entries, not audio.
;;;
;;; Signing in uses the authorization code flow with PKCE, because a desktop
;;; program has no safe place for a client secret.  `orpheus-spotify-login'
;;; opens the account page, listens on the loopback port of the redirect URI
;;; for the one request that carries the code, swaps the code for a token, and
;;; stores the token in a private file that later sessions read.
;;;
;;; Every request goes out through `url-retrieve', so replies arrive in a
;;; process filter and no command waits on the network.  Spotify counts
;;; requests over a rolling window, so the status is read on an interval of
;;; its own, an answer already in hand is reused, and a `Retry-After' silences
;;; the client until it expires instead of earning a longer ban.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'orpheus-backend)

(declare-function browse-url "browse-url" (url &rest args))
(declare-function orpheus-connect "orpheus" (&optional callback))

;;; Settings

(defcustom orpheus-spotify-client-id nil
  "Client id of your Spotify app, or nil before you create one.
Create a free app at https://developer.spotify.com/dashboard, add
`orpheus-spotify-redirect-uri' to its redirect URIs, and put its client id
here.  No client secret is needed: orpheus signs in with PKCE."
  :type '(choice (const :tag "Not set up yet" nil) string)
  :group 'orpheus)

(defcustom orpheus-spotify-redirect-uri "http://127.0.0.1:8899/orpheus/callback"
  "Where Spotify sends the browser after you allow the app.
The sign-in listens on the loopback address and port named here for that one
request, so the URI must be registered on the app exactly as written.  A
loopback address is used rather than a page on the internet because the code
it carries belongs to you and never has to leave this machine."
  :type 'string
  :group 'orpheus)

(defcustom orpheus-spotify-token-file
  (expand-file-name "orpheus/spotify-token.json"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "File where the access and refresh tokens are kept.
It is written readable by your user alone.  It is not in the cache directory,
which is pruned; losing it only costs one sign-in."
  :type 'file
  :group 'orpheus)

(defcustom orpheus-spotify-api-url "https://api.spotify.com/v1"
  "Base URL of the Spotify Web API.
Change it to reach the API through a proxy."
  :type 'string
  :group 'orpheus)

(defcustom orpheus-spotify-auth-url "https://accounts.spotify.com"
  "Base URL of the Spotify accounts service, which signs you in."
  :type 'string
  :group 'orpheus)

(defcustom orpheus-spotify-poll-interval 3.0
  "Seconds between two reads of what Spotify is playing.
The player asks once a second; this backend answers from the last reply until
this interval has passed, because Spotify counts its requests over a rolling
window and a player that asks too often is banned for a while."
  :type 'number
  :group 'orpheus)

(defconst orpheus-spotify-scopes
  '("user-read-playback-state" "user-modify-playback-state"
    "user-read-currently-playing" "playlist-read-private"
    "playlist-read-collaborative" "user-library-read" "user-follow-read")
  "Permissions orpheus asks for when it signs in.")

(defconst orpheus-spotify-page-size 50
  "Rows asked for per page.  Spotify allows no more than 50.")

(defconst orpheus-spotify-page-limit 20
  "Most pages one listing follows.
Shortcut: a playlist longer than 1000 tracks is shown in part, because reading
all of it costs one request per 50 tracks on a rate-limited API; raise this
when a longer playlist has to be whole.")

(defconst orpheus-spotify-login-timeout 300
  "Seconds a sign-in waits for the browser before it gives up listening.")

(defvar orpheus-spotify--art-urls (make-hash-table :test #'equal)
  "Cover URL of each track URI orpheus has read, keyed by the URI.
A listing and the player state both carry the album images, and the API counts
requests over a rolling window, so asking `/tracks/ID' again for a picture
already in hand would be the sort of request that earns a ban.")

;;; The backend

(cl-defstruct (orpheus-spotify (:include orpheus-backend)
                               (:constructor make-orpheus-spotify)
                               (:conc-name orpheus-spotify--))
  "The Spotify backend, pointing at one account through the Web API."
  api-url
  auth-url
  token-file
  client-id
  token
  (status nil)
  (track nil)
  (status-at 0)
  (pending nil)
  (not-before 0)
  (refreshing nil)
  (auth-failed nil)
  (mode-repeat nil)
  (mode-single nil)
  (last-error nil))

(defun orpheus-spotify--make ()
  "Return a new Spotify backend built from the user options."
  (make-orpheus-spotify :name 'spotify
                        :api-url orpheus-spotify-api-url
                        :auth-url orpheus-spotify-auth-url
                        :token-file orpheus-spotify-token-file
                        :client-id orpheus-spotify-client-id))

(orpheus-register-backend 'spotify #'orpheus-spotify--make
                          "Spotify, controlled through its Web API")

;;; Small helpers

(defun orpheus-spotify--field (object key)
  "Return KEY's value in OBJECT, a JSON object read as an alist.
`json-parse-string' names its keys with symbols, so a string KEY is looked up
as the symbol it becomes."
  (when (and (consp object) (consp (car object)))
    (alist-get (if (stringp (car (car object))) key (intern key))
               object nil nil #'equal)))

(defun orpheus-spotify--path (object &rest keys)
  "Return the value at KEYS inside OBJECT, or nil when a step is missing."
  (while (and keys object)
    (setq object (orpheus-spotify--field object (pop keys))))
  object)

(defun orpheus-spotify--base64url (string)
  "Return STRING, bytes, as base64url without padding.
Base64url is what PKCE names: the ordinary base64 alphabet with `-' and `_'
in place of `+' and `/', and the padding left off."
  (replace-regexp-in-string
   "=+\\'" ""
   (replace-regexp-in-string
    "[/+]" (lambda (character)
             (if (equal character "/") "_" "-"))
    (base64-encode-string string t) nil 'literal)
   nil 'literal))

(defun orpheus-spotify--random-bytes (count)
  "Return COUNT bytes of entropy, as a unibyte string.
Emacs has no cryptographic generator, so this reads the system's pool where
there is one and otherwise mixes the clock with the pseudo-random state."
  (if (file-readable-p "/dev/urandom")
      (with-temp-buffer
        (set-buffer-multibyte nil)
        ;; A character device takes no start position, only an end.
        (insert-file-contents-literally "/dev/urandom" nil nil count)
        (buffer-string))
    ;; shortcut: no CSPRNG in Emacs outside /dev/urandom, so the fallback is
    ;; only as good as the clock and `random'; add a source when one exists.
    (secure-hash 'sha256 (format "%s-%s-%s" (float-time) (random) (random))
                 nil nil t)))

(defun orpheus-spotify--pkce-verifier ()
  "Return a fresh code verifier, as the 43 url-safe characters S256 wants."
  (orpheus-spotify--base64url (orpheus-spotify--random-bytes 32)))

(defun orpheus-spotify--pkce-challenge (verifier)
  "Return the S256 challenge for VERIFIER."
  (orpheus-spotify--base64url (secure-hash 'sha256 verifier nil nil t)))

(defun orpheus-spotify--query-encode (parameters)
  "Return PARAMETERS, an alist of strings, as a query string.
A pair whose value is nil is left out; an empty string is kept, because the
API tells the two apart in a few places."
  (mapconcat (lambda (pair)
               (format "%s=%s" (url-hexify-string (car pair))
                       (url-hexify-string (cdr pair))))
             (cl-remove-if-not #'cdr (mapcar #'identity parameters))
             "&"))

(defun orpheus-spotify--callback-parameters (target)
  "Return TARGET's query string as an alist of decoded names and values."
  (let ((query (cadr (split-string target "?"))))
    (when (and query (not (string-empty-p query)))
      (delq nil
            (mapcar (lambda (pair)
                      (let ((parts (split-string pair "=")))
                        (when (car parts)
                          (cons (url-unhex-string (car parts))
                                (url-unhex-string (or (cadr parts) ""))))))
                    (split-string query "&"))))))

;;; Talking HTTP

(defun orpheus-spotify--headers (head)
  "Return the header lines of HEAD, the reply before its body, as an alist."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([^:]+\\): *\\(.*\\)\\'" line)
                    (cons (downcase (match-string 1 line))
                          (string-trim (match-string 2 line)))))
                (cdr (split-string head "\r\n")))))

(defun orpheus-spotify--json (body)
  "Return BODY, a JSON text, as alists and lists, or nil when it is not JSON."
  (when (and body (not (string-empty-p (string-trim body))))
    (condition-case nil
        (json-parse-string body :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil)
      (error nil))))

(defun orpheus-spotify--api-error (status json)
  "Return the sentence STATUS and JSON make, or nil.
Spotify says why it refused in the JSON body, and that sentence is worth
showing; the status code is only the fallback."
  (let ((message (or (orpheus-spotify--path json "error" "message")
                     (orpheus-spotify--field json "error_message")
                     (orpheus-spotify--field json "error_description"))))
    (cond ((and (stringp message) (not (string-empty-p message))) message)
          ((and (numberp status) (>= status 400))
           (format "Spotify answered %d" status)))))

(defun orpheus-spotify--reason (error)
  "Return ERROR, what `url-retrieve' reported, as a short sentence."
  (or (and (listp error) (stringp (car (cdr error))) (car (cdr error)))
      (and (listp error) (stringp (car (cdr (cdr error))))
           (car (cdr (cdr error))))
      (ignore-errors (error-message-string error))
      "no reason given"))

(defun orpheus-spotify--reply (parsed network-error)
  "Return PARSED, a reply read from the buffer, judged.
NETWORK-ERROR is what `url-retrieve' reported when the request never got an
answer.  The reply is a plist of :ok, :status, :headers, :body, :json and
:error."
  (let* ((status (plist-get parsed :status))
         (body (or (plist-get parsed :body) ""))
         (json (orpheus-spotify--json body))
         (answered (and (numberp status) (>= status 200) (< status 300)))
         ;; A command answers with no body at all; a listing answers with JSON.
         ;; Anything else came back from something that is not the API, and an
         ;; empty tab would hide it.
         (readable (or json (string-empty-p (string-trim body))))
         (ok (and answered readable)))
    (list :ok (and ok t)
          :status status
          :headers (plist-get parsed :headers)
          :body body
          :json json
          :error (cond
                  ((and answered (not readable))
                   (format "Spotify's reply could not be read: %s"
                           (string-trim
                            (substring body 0 (min 60 (length body))))))
                  (answered nil)
                  (t (or (orpheus-spotify--api-error status json)
                         (when network-error
                           (format "cannot reach Spotify: %s"
                                   (orpheus-spotify--reason network-error)))
                         "Spotify did not answer"))))))

(defun orpheus-spotify--response (buffer)
  "Return BUFFER's HTTP reply as a plist of :status, :headers and :body."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((end (when (re-search-forward "\r?\n\r?\n" nil t) (point)))
             (head (buffer-substring-no-properties (point-min)
                                                   (or end (point-max))))
             (body (if end
                       (buffer-substring-no-properties end (point-max))
                     "")))
        (list :status (when (string-match "\\`HTTP/[0-9.]+ +\\([0-9]+\\)" head)
                        (string-to-number (match-string 1 head)))
              :headers (orpheus-spotify--headers head)
              :body body)))))

(defun orpheus-spotify--deliver (buffer status callback)
  "Read BUFFER's reply for STATUS, then call CALLBACK with it.
The buffer is killed first: its only reason to exist was this reply."
  (let* ((network-error (plist-get status :error))
         (parsed (and buffer (buffer-live-p buffer)
                      (orpheus-spotify--response buffer)))
         (reply (orpheus-spotify--reply parsed network-error)))
    (when (and buffer (buffer-live-p buffer))
      (kill-buffer buffer))
    (funcall callback reply)))

(defun orpheus-spotify--http (method url headers body callback)
  "Send one HTTP request and call CALLBACK with the reply plist.
METHOD is \"GET\", \"POST\", \"PUT\" or \"DELETE\"; URL is absolute; HEADERS
is an alist; BODY is a string or nil.  The reply is the plist README means by
a reply.  Nothing waits: the answer arrives through `url-retrieve' while the
editor keeps working."
  (condition-case err
      (let ((url-request-method method)
            (url-request-extra-headers headers)
            (url-request-data body)
            ;; The bodies here are a few kilobytes of JSON; asking for them
            ;; uncompressed keeps one decoding step out of the path.
            (url-mime-encoding-string nil))
        ;; SILENT leaves the reply unparsed, so the status line, the headers
        ;; and the body are all still in the buffer; INHIBIT-COOKIES keeps
        ;; one account's cookies out of the next account's requests.
        (let ((buffer (url-retrieve
                       url
                       (lambda (status)
                         (orpheus-spotify--deliver
                          (current-buffer) status callback))
                       nil t t)))
          (when (buffer-live-p buffer)
            (let ((process (get-buffer-process buffer)))
              (when process (set-process-query-on-exit-flag process nil))))
          buffer))
    (error
     (funcall callback
              (list :ok nil :status nil
                    :error (format "cannot reach Spotify: %s"
                                   (error-message-string err)))))))

(defun orpheus-spotify--url (client path)
  "Return PATH on CLIENT's API."
  (concat (string-remove-suffix "/" (orpheus-spotify--api-url client)) path))

(defun orpheus-spotify--form (client path parameters callback)
  "Post PARAMETERS, an alist, as a form to PATH on CLIENT's accounts host."
  (orpheus-spotify--http
   "POST"
   (concat (string-remove-suffix "/" (orpheus-spotify--auth-url client)) path)
   '(("Content-Type" . "application/x-www-form-urlencoded")
     ("Accept" . "application/json"))
   (orpheus-spotify--query-encode parameters)
   callback))

;;; The token

(defun orpheus-spotify--token-from-json (json &optional previous)
  "Return JSON, a token reply, as the token plist.
PREVIOUS is the token it replaces, whose refresh token and scopes are kept
when Spotify does not send new ones."
  (let ((access (orpheus-spotify--field json "access_token")))
    (when (stringp access)
      (list :access-token access
            :refresh-token (or (orpheus-spotify--field json "refresh_token")
                               (plist-get previous :refresh-token))
            ;; Renew a minute early, so a request is not sent with a token
            ;; that expires while it is on its way.
            :expires-at (+ (float-time)
                           (max 60 (- (or (orpheus-spotify--field json
                                                                   "expires_in")
                                          3600)
                                      60)))
            :scope (or (orpheus-spotify--field json "scope")
                       (plist-get previous :scope))))))

(defun orpheus-spotify--read-token (file)
  "Return the token stored in FILE, or nil when there is none to read."
  (when (and file (file-readable-p file))
    (condition-case nil
        (let ((json (orpheus-spotify--json
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))))
          (let ((access (orpheus-spotify--field json "access_token")))
            (when (stringp access)
              (list :access-token access
                    :refresh-token (orpheus-spotify--field json "refresh_token")
                    :expires-at (orpheus-spotify--field json "expires_at")
                    :scope (orpheus-spotify--field json "scope")))))
      (error nil))))

(defun orpheus-spotify--write-token (file token)
  "Write TOKEN to FILE, readable by its owner alone."
  (when (and file token)
    (make-directory (file-name-directory (expand-file-name file)) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (json-serialize (list (cons 'access_token
                                                (plist-get token :access-token))
                                          (cons 'refresh_token
                                                (plist-get token :refresh-token))
                                          (cons 'expires_at
                                                (plist-get token :expires-at))
                                          (cons 'scope
                                                (plist-get token :scope))))
                    nil file nil 'silent))
    ;; The file names a live account: it is created private, and made private
    ;; again in case an older version wrote it open.
    (set-file-modes file #o600)
    file))

(defun orpheus-spotify--adopt-token (client token)
  "Make TOKEN CLIENT's own, in memory and in its token file."
  (setf (orpheus-spotify--token client) token
        (orpheus-spotify--auth-failed client) nil)
  (when token
    (orpheus-spotify--write-token (orpheus-spotify--token-file client) token))
  token)

(defun orpheus-spotify--forget-token (client)
  "Drop CLIENT's token from memory, keeping the file for the next session."
  (setf (orpheus-spotify--token client) nil
        (orpheus-spotify--status client) nil
        (orpheus-spotify--pending client) nil
        (orpheus-spotify--auth-failed client) nil))

(defun orpheus-spotify--sign-in-hint (client)
  "Return the sentence that says how to get CLIENT signed in."
  (cond
   ((null (orpheus-spotify--client-id client))
    (concat "set `orpheus-spotify-client-id' to the id of your Spotify app, "
            "then run M-x orpheus-spotify-login"))
   ((orpheus-spotify--auth-failed client)
    "Spotify no longer accepts this session; run M-x orpheus-spotify-login")
   (t "orpheus is not signed in to Spotify; run M-x orpheus-spotify-login")))

(defun orpheus-spotify--refresh (client callback)
  "Ask Spotify for a new access token for CLIENT, then call CALLBACK with it.
CALLBACK receives the token or nil.  One renewal runs at a time: a second
caller waits for the same answer instead of spending another request on the
token endpoint."
  (let ((refresh (plist-get (orpheus-spotify--token client) :refresh-token))
        (client-id (orpheus-spotify--client-id client)))
    (cond
     ((orpheus-spotify--refreshing client)
      (setf (orpheus-spotify--refreshing client)
            (append (orpheus-spotify--refreshing client) (list callback))))
     ((or (null refresh) (null client-id))
      (setf (orpheus-spotify--auth-failed client) t)
      (funcall callback nil))
     (t
      (setf (orpheus-spotify--refreshing client) (list callback))
      (orpheus-spotify--form
       client "/api/token"
       (list (cons "grant_type" "refresh_token")
             (cons "refresh_token" refresh)
             (cons "client_id" client-id))
       (lambda (reply)
         (let* ((waiting (orpheus-spotify--refreshing client))
                (fresh (and (plist-get reply :ok)
                            (orpheus-spotify--token-from-json
                             (plist-get reply :json)
                             (orpheus-spotify--token client)))))
           (setf (orpheus-spotify--refreshing client) nil)
           (if fresh
               (progn
                 (orpheus-spotify--adopt-token client fresh)
                 (dolist (wait waiting)
                   (funcall wait (plist-get fresh :access-token))))
             (setf (orpheus-spotify--auth-failed client) t)
             (message "orpheus: Spotify refused to renew the session: %s"
                      (or (plist-get reply :error) "no answer"))
             (dolist (wait waiting) (funcall wait nil))))))))))

(defun orpheus-spotify--with-token (client callback)
  "Call CALLBACK with an access token CLIENT can use now, or with nil."
  (let ((token (orpheus-spotify--token client)))
    (cond
     ((null token) (funcall callback nil))
     ((and (plist-get token :refresh-token)
           (< (or (plist-get token :expires-at) 0) (+ (float-time) 30)))
      (orpheus-spotify--refresh client callback))
     (t (funcall callback (plist-get token :access-token))))))

(defun orpheus-spotify--throttle (client reply)
  "Hold CLIENT's requests until the wait REPLY asks for has passed."
  (let ((text (cdr (assoc "retry-after" (plist-get reply :headers)))))
    (setf (orpheus-spotify--not-before client)
          (+ (float-time)
             (max 1 (if (and text (string-match-p "\\`[0-9.]+\\'" text))
                        (string-to-number text)
                      30))))))

(defun orpheus-spotify--attempt (client method path body token retried callback)
  "Send one attempt at METHOD PATH on CLIENT, then call CALLBACK.
BODY travels as JSON, TOKEN is the access token, and RETRIED says whether a
refusal has already been answered with a renewal."
  (orpheus-spotify--http
   method
   (orpheus-spotify--url client path)
   (append (list (cons "Authorization" (concat "Bearer " token))
                 (cons "Accept" "application/json"))
           (when body
             (list (cons "Content-Type" "application/json"))))
   (when body (json-serialize body))
   (lambda (reply)
     (cond
      ((equal (plist-get reply :status) 429)
       (orpheus-spotify--throttle client reply)
       (funcall callback reply))
      ((and (equal (plist-get reply :status) 401) (not retried))
       (let ((current (plist-get (orpheus-spotify--token client) :access-token)))
         (cond
          ;; The token that failed is the one still stored, so it is stale.
          ((equal token current)
           (orpheus-spotify--refresh
            client
            (lambda (fresh)
              (if fresh
                  (orpheus-spotify--attempt client method path body fresh t
                                            callback)
                (funcall callback reply)))))
          ;; Another request already renewed it: retry with that one, and do
          ;; not spend a second request on the token endpoint for one token.
          (current
           (orpheus-spotify--attempt client method path body current t
                                     callback))
          (t (funcall callback reply)))))
      (t (funcall callback reply))))))

(defun orpheus-spotify--request (client method path &optional body callback)
  "Call METHOD PATH on CLIENT with BODY, then call CALLBACK with the reply.
BODY is an alist that travels as JSON, or nil.  A token that has run out is
renewed before the request; one that Spotify rejects is renewed and the
request tried once more.  A request Spotify asked to delay is not sent at all,
and the reply says so with :throttled."
  (let ((callback (or callback #'ignore)))
    (cond
     ((> (orpheus-spotify--not-before client) (float-time))
      (funcall callback
               (list :ok nil :throttled t :status nil
                     :error (format "Spotify asked orpheus to wait %.0fs"
                                    (- (orpheus-spotify--not-before client)
                                       (float-time))))))
     (t
      (orpheus-spotify--with-token
       client
       (lambda (token)
         (if (null token)
             (funcall callback
                      (list :ok nil :status nil
                            :error (orpheus-spotify--sign-in-hint client)))
           (orpheus-spotify--attempt client method path body token nil
                                     callback))))))))

;;; Signing in

(defun orpheus-spotify--authorize-url (verifier state)
  "Return the page that asks you to allow the app, for VERIFIER and STATE."
  (concat (string-remove-suffix "/" orpheus-spotify-auth-url) "/authorize?"
          (orpheus-spotify--query-encode
           (list (cons "client_id" orpheus-spotify-client-id)
                 (cons "response_type" "code")
                 (cons "redirect_uri" orpheus-spotify-redirect-uri)
                 (cons "scope" (string-join orpheus-spotify-scopes " "))
                 (cons "state" state)
                 (cons "code_challenge_method" "S256")
                 (cons "code_challenge"
                       (orpheus-spotify--pkce-challenge verifier))))))

(defvar orpheus-spotify--login-server nil
  "The listener that waits for the sign-in answer, while it is open.")

(defvar orpheus-spotify--login-text ""
  "What the sign-in listener has received so far.")

(defun orpheus-spotify--login-page (message)
  "Return an HTML page saying MESSAGE, as the reply to the browser."
  (let ((body (format (concat "<!doctype html><html><head><meta charset=\"utf-8\">"
                              "<title>orpheus</title></head>"
                              "<body style=\"font-family: sans-serif; margin: 4em\">"
                              "<p>%s</p></body></html>")
                      message)))
    (concat "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n"
            (format "Content-Length: %d\r\nConnection: close\r\n\r\n" (length body))
            body)))

(defun orpheus-spotify--stop-listening ()
  "Close the sign-in listener, if one is open."
  (when (process-live-p orpheus-spotify--login-server)
    (delete-process orpheus-spotify--login-server))
  (setq orpheus-spotify--login-server nil
        orpheus-spotify--login-text ""))

(defun orpheus-spotify--exchange (client code verifier)
  "Swap CODE, proved by VERIFIER, for a token and store it for CLIENT."
  (orpheus-spotify--form
   client "/api/token"
   (list (cons "grant_type" "authorization_code")
         (cons "code" code)
         (cons "redirect_uri" orpheus-spotify-redirect-uri)
         (cons "client_id" (orpheus-spotify--client-id client))
         (cons "code_verifier" verifier))
   (lambda (reply)
     (let ((token (and (plist-get reply :ok)
                       (orpheus-spotify--token-from-json
                        (plist-get reply :json)))))
       (if token
           (progn
             (orpheus-spotify--adopt-token client token)
             (message "orpheus: signed in to Spotify; the token is in %s"
                      (orpheus-spotify--token-file client))
             (when (eq (bound-and-true-p orpheus-backend) 'spotify)
               (ignore-errors (orpheus-connect))))
         (message "orpheus: Spotify did not give a token: %s"
                  (or (plist-get reply :error) "no answer")))))))

(defun orpheus-spotify--answer (process chunk client state verifier)
  "Read the sign-in answer in CHUNK on PROCESS and finish signing CLIENT in.
STATE is the value the sign-in started with, and VERIFIER proves the code."
  (setq orpheus-spotify--login-text (concat orpheus-spotify--login-text chunk))
  (when (string-match "\r?\n\r?\n" orpheus-spotify--login-text)
    (let* ((head (car (split-string orpheus-spotify--login-text "\r\n")))
           (target (or (nth 1 (split-string head " ")) "/"))
           (parameters (orpheus-spotify--callback-parameters target))
           (code (cdr (assoc "code" parameters)))
           (error (cdr (assoc "error" parameters)))
           (answered (cdr (assoc "state" parameters))))
      (orpheus-spotify--stop-listening)
      (ignore-errors
        (process-send-string
         process (orpheus-spotify--login-page
                  (if code "orpheus is signed in.  You can close this tab."
                    "This sign-in did not finish; see the Emacs echo area."))))
      (ignore-errors (delete-process process))
      (cond
       ((and error (not (string-empty-p error)))
        (message "orpheus: Spotify did not allow the app: %s" error))
       ((not (equal answered state))
        (message "orpheus: ignored a sign-in answer that did not start here"))
       (code (orpheus-spotify--exchange client code verifier))
       (t (message "orpheus: the sign-in answer carried no code"))))))

(defun orpheus-spotify--listen (client state verifier)
  "Wait for the sign-in answer on the redirect URI's address, for CLIENT.
STATE and VERIFIER are what the sign-in started with.  Return the listener."
  (setq orpheus-spotify--login-text "")
  (let* ((parts (url-generic-parse-url orpheus-spotify-redirect-uri))
         (host (url-host parts))
         (port (url-port parts)))
    (condition-case err
        (setq orpheus-spotify--login-server
              (make-network-process
               :name "orpheus-spotify-login" :server t :host host
               :service port
               :family (if (string-match-p ":" host) 'ipv6 'ipv4)
               :coding 'binary :noquery t
               :filter (lambda (process chunk)
                         (orpheus-spotify--answer process chunk client state
                                                  verifier))))
      (error
       (user-error "Cannot listen on %s:%s for the sign-in: %s"
                   host port (error-message-string err))))))

(defun orpheus-spotify-login ()
  "Sign orpheus in to Spotify and store the token it gets.
The browser opens on Spotify's own page; allowing the app sends one request
to `orpheus-spotify-redirect-uri', which this command is listening on."
  (interactive)
  (unless orpheus-spotify-client-id
    (user-error "Set `orpheus-spotify-client-id' to your app's client id first"))
  (let* ((client (orpheus-spotify--make))
         (verifier (orpheus-spotify--pkce-verifier))
         (state (orpheus-spotify--base64url (orpheus-spotify--random-bytes 16)))
         (url (orpheus-spotify--authorize-url verifier state))
         (listener (orpheus-spotify--listen client state verifier)))
    (run-at-time orpheus-spotify-login-timeout nil
                 (lambda ()
                   (when (eq listener orpheus-spotify--login-server)
                     (orpheus-spotify--stop-listening)
                     (message "orpheus: the Spotify sign-in timed out"))))
    (message "orpheus: waiting for the browser to finish the sign-in")
    (if (fboundp 'browse-url)
        (browse-url url)
      (message "orpheus: open this page to sign in: %s" url))))

(defun orpheus-spotify-logout ()
  "Forget the Spotify token, here and on disk."
  (interactive)
  (let ((file orpheus-spotify-token-file))
    (when (file-exists-p file)
      (delete-file file))
    (when (and (boundp 'orpheus-backend-current)
               orpheus-backend-current
               (eq (orpheus-backend-name orpheus-backend-current) 'spotify))
      (orpheus-spotify--forget-token orpheus-backend-current))
    (message "orpheus: signed out of Spotify")))

;;; Reading what is playing

(defun orpheus-spotify--join (items)
  "Return ITEMS, a list of strings, joined by commas, or nil."
  (let ((items (delq nil (mapcar #'identity (or items nil)))))
    (when items (string-join items ", "))))

(defun orpheus-spotify--names (items key)
  "Return KEY's value from each of ITEMS, joined by commas, or nil."
  (orpheus-spotify--join
   (mapcar (lambda (item) (orpheus-spotify--field item key)) items)))

(defun orpheus-spotify--year (album)
  "Return the year ALBUM was released in, or nil."
  (let ((date (orpheus-spotify--field album "release_date")))
    (when (and (stringp date) (>= (length date) 4))
      (substring date 0 4))))

(defun orpheus-spotify--status-from-player (player)
  "Return PLAYER, a `GET /me/player' reply, as an `orpheus-status'.
A reply with no body is what Spotify sends when no device is playing, which is
a stopped player and not a backend that is not answering."
  (let* ((device (orpheus-spotify--field player "device"))
         (item (orpheus-spotify--field player "item"))
         (repeat (orpheus-spotify--field player "repeat_state"))
         (progress (orpheus-spotify--field player "progress_ms"))
         (duration (orpheus-spotify--field item "duration_ms")))
    (orpheus-status-create
     :state (cond ((null player) 'stop)
                  ((orpheus-spotify--field player "is_playing") 'play)
                  (t 'pause))
     :elapsed (/ (or progress 0) 1000.0)
     :total (/ (or duration 0) 1000.0)
     :volume (or (orpheus-spotify--field device "volume_percent") 0)
     :repeat (equal repeat "context")
     :random (orpheus-spotify--field player "shuffle_state")
     :single (equal repeat "track")
     :songid (orpheus-spotify--field item "id"))))

(defun orpheus-spotify--track-from-player (player)
  "Return PLAYER's playing item as an `orpheus-track', or nil."
  (let ((item (orpheus-spotify--field player "item")))
    (when item (orpheus-spotify--track-from-item item))))

(defun orpheus-spotify--report (client text)
  "Show TEXT once, until CLIENT has a different thing to report."
  (unless (equal text (orpheus-spotify--last-error client))
    (setf (orpheus-spotify--last-error client) text)
    (when text (message "orpheus: %s" text))))

(defun orpheus-spotify--remember-player (client reply)
  "Store what REPLY says about CLIENT's player, and return non-nil when it said."
  (if (plist-get reply :ok)
      (progn
        (orpheus-spotify--report client nil)
        (let ((status (orpheus-spotify--status-from-player
                       (plist-get reply :json))))
          (setf (orpheus-spotify--status client) status
                (orpheus-spotify--mode-repeat client)
                (orpheus-status-repeat status)
                (orpheus-spotify--mode-single client)
                (orpheus-status-single status)
                (orpheus-spotify--track client)
                (orpheus-spotify--track-from-player (plist-get reply :json))
                (orpheus-spotify--status-at client) (float-time)))
        t)
    (orpheus-spotify--report client (plist-get reply :error))
    nil))

(defun orpheus-spotify--read-player (client callback)
  "Read `/me/player' for CLIENT when the last answer is stale, then call CALLBACK.
One reply holds the status and the track, so both come from one request.  A
read already in flight takes the next caller with it instead of sending a
second one, and a read that fails answers with what was known."
  (let ((pending (orpheus-spotify--pending client)))
    (cond
     (pending
      (setf (orpheus-spotify--pending client)
            (cons pending (list callback))))
     ((and (orpheus-spotify--status-at client)
           (< (- (float-time) (orpheus-spotify--status-at client))
              orpheus-spotify-poll-interval))
      (funcall callback))
     (t
      (setf (orpheus-spotify--pending client) (list callback))
      (orpheus-spotify--request
       client "GET" "/me/player" nil
       (lambda (reply)
         (orpheus-spotify--remember-player client reply)
         (let ((waiting (orpheus-spotify--pending client)))
           (setf (orpheus-spotify--pending client) nil)
           (dolist (wait waiting)
             (condition-case err
                 (funcall wait)
               (error (message "orpheus: Spotify callback failed: %S"
                               err)))))))))))

(defun orpheus-spotify--status-for-player (client)
  "Return CLIENT's last status, as of now.
A reply a few seconds old would otherwise freeze the progress bar between
reads, so the seconds that passed are added back while it plays."
  (let ((status (orpheus-spotify--status client)))
    (when status
      (let ((passed (- (float-time) (orpheus-spotify--status-at client))))
        (if (and (eq (orpheus-status-state status) 'play) (> passed 0))
            (let ((copy (copy-orpheus-status status)))
              (setf (orpheus-status-elapsed copy)
                    (max 0 (min (orpheus-status-total status)
                                (+ (orpheus-status-elapsed status) passed))))
              copy)
          status)))))

;;; Rows for the tabs

(defun orpheus-spotify--with-query (path &optional parameters)
  "Return PATH with PARAMETERS as its query string."
  (let ((query (orpheus-spotify--query-encode parameters)))
    (if (string-empty-p query) path (concat path "?" query))))

(defun orpheus-spotify--track-row (track &optional detail)
  "Return TRACK, a JSON item, as a row that plays.
DETAIL fills the second column; the length of the track does when it is nil."
  (let* ((parsed (orpheus-spotify--track-from-item track))
         (uri (orpheus-track-uri parsed)))
    (when uri
      (orpheus-item-create
       :label (orpheus-track-label parsed)
       :detail (or detail (orpheus-format-time (orpheus-track-duration parsed)))
       :kind 'track
       :data (list (cons 'uri uri) (cons 'track parsed))))))

(defun orpheus-spotify--remember-art (track item)
  "Remember ITEM's largest album image as the cover of TRACK's URI."
  (let* ((uri (and track (orpheus-track-uri track)))
         (url (and (stringp uri)
                   (orpheus-spotify--field
                    (car (orpheus-spotify--field
                          (orpheus-spotify--field item "album") "images"))
                    "url"))))
    (when (and uri url)
      (puthash uri url orpheus-spotify--art-urls))
    track))

(defun orpheus-spotify--track-from-item (item)
  "Return ITEM, a track JSON object, as an `orpheus-track'."
  (let ((album (orpheus-spotify--field item "album")))
    (orpheus-spotify--remember-art
     (orpheus-track-create
      :uri (orpheus-spotify--field item "uri")
      :title (orpheus-spotify--field item "name")
      :artist (orpheus-spotify--names (orpheus-spotify--field item "artists")
                                      "name")
      :album (orpheus-spotify--field album "name")
      :albumartist (orpheus-spotify--names
                    (orpheus-spotify--field album "artists") "name")
      :duration (/ (or (orpheus-spotify--field item "duration_ms") 0) 1000.0)
      :trackno (orpheus-spotify--field item "track_number")
      :year (orpheus-spotify--year album))
     item)))

(defun orpheus-spotify--album-row (album &optional leading)
  "Return ALBUM, a JSON object, as a row that descends into its tracks.
LEADING, when given, is the path already walked inside the artists tab, so a
row read from an artist's page descends there and not into the saved albums."
  (let ((id (orpheus-spotify--field album "id")))
    (when id
      (orpheus-item-create
       :label (or (orpheus-spotify--field album "name") "album")
       :detail (orpheus-spotify--names (orpheus-spotify--field album "artists")
                                       "name")
       :kind 'album
       :data (list (cons 'browse
                         (if leading
                             (cons 'artists (append leading (list id)))
                           (cons 'albums (list id))))
                   (cons 'id id))))))

(defun orpheus-spotify--artist-row (artist)
  "Return ARTIST, a JSON object, as a row that descends into its albums."
  (let ((id (orpheus-spotify--field artist "id")))
    (when id
      (orpheus-item-create
       :label (or (orpheus-spotify--field artist "name") "artist")
       :detail (orpheus-spotify--join (orpheus-spotify--field artist
                                               "genres"))
       :kind 'artist
       :data (list (cons 'browse (cons 'artists (list id)))
                   (cons 'id id))))))

(defun orpheus-spotify--playlist-row (playlist)
  "Return PLAYLIST, a JSON object, as a row that descends into its tracks."
  (let ((id (orpheus-spotify--field playlist "id")))
    (when id
      (let ((total (orpheus-spotify--path playlist "tracks" "total")))
        (orpheus-item-create
         :label (or (orpheus-spotify--field playlist "name") "playlist")
         :detail (when (numberp total) (format "%d tracks" total))
         :kind 'playlist
         :data (list (cons 'browse (cons 'playlists (list id)))
                     (cons 'id id)))))))

(defun orpheus-spotify--paged (client path parameters items-function parse
                                    callback)
  "Read every page of PATH on CLIENT, then call CALLBACK with the rows.
PARAMETERS are the query parameters of the first page, ITEMS-FUNCTION picks
the entries out of one page and PARSE turns one entry into a row, or nil to
skip it.  When a page fails, the rows read so far are kept."
  (let ((rows nil)
        (pages 0)
        (send nil))
    (setq send
          (lambda (offset)
            (orpheus-spotify--request
             client "GET"
             (orpheus-spotify--with-query
              path
              (append parameters
                      (list (cons "limit"
                                  (number-to-string orpheus-spotify-page-size))
                            (cons "offset" (number-to-string offset)))))
             nil
             (lambda (reply)
               (if (not (plist-get reply :ok))
                   (progn
                     (orpheus-spotify--report client (plist-get reply :error))
                     (funcall callback rows))
                 (let ((page (plist-get reply :json)))
                   (setq rows (append rows
                                      (delq nil
                                            (mapcar parse
                                                    (funcall items-function page))))
                         pages (1+ pages))
                   (if (and (orpheus-spotify--field page "next")
                            (< pages orpheus-spotify-page-limit))
                       (funcall send (+ offset orpheus-spotify-page-size))
                     (funcall callback rows))))))))
    (funcall send 0)))

(defun orpheus-spotify--items (page)
  "Return the entries of PAGE, an ordinary Spotify listing."
  (orpheus-spotify--field page "items"))

(defun orpheus-spotify--followed-artists (client callback)
  "Read the artists CLIENT follows, then call CALLBACK with the rows.
This listing is the one that pages by cursor: the entries and the link to the
next page sit under `artists', and the next page is asked for with the id of
the last artist read rather than with an offset."
  (let ((rows nil)
        (pages 0)
        (after nil)
        (send nil))
    (setq send
          (lambda ()
            (orpheus-spotify--request
             client "GET"
             (orpheus-spotify--with-query
              "/me/following"
              (append (list (cons "type" "artist")
                            (cons "limit"
                                  (number-to-string
                                   orpheus-spotify-page-size)))
                      (when after (list (cons "after" after)))))
             nil
             (lambda (reply)
               (if (not (plist-get reply :ok))
                   (progn
                     (orpheus-spotify--report client (plist-get reply :error))
                     (funcall callback rows))
                 (let* ((listing (orpheus-spotify--field (plist-get reply :json)
                                                         "artists"))
                        (entries (orpheus-spotify--items listing))
                        (last-id (orpheus-spotify--field (car (last entries))
                                                         "id")))
                   (setq rows (append rows
                                      (delq nil
                                            (mapcar #'orpheus-spotify--artist-row
                                                    entries)))
                         pages (1+ pages)
                         after last-id)
                   (if (and last-id (orpheus-spotify--field listing "next")
                            (< pages orpheus-spotify-page-limit))
                       (funcall send)
                     (funcall callback rows))))))))
    (funcall send)))

(defun orpheus-spotify--liked-row ()
  "Return the row that opens the saved songs, which Spotify has no playlist for."
  (orpheus-item-create
   :label "Liked Songs"
   :detail "saved tracks"
   :kind 'playlist
   :data (list (cons 'browse (cons 'playlists (list "liked")))
               (cons 'id "liked"))))

;;; Playing something

(defun orpheus-spotify--parts (uri)
  "Return what URI names, as a cons of its kind and its id.
A Spotify URI spells it `spotify:track:ID'; a link from the web player spells
the same thing as a path, sometimes behind an `intl-xx' language segment and
usually with a `si' tracking query, both of which are noise here."
  (cond
   ((and (stringp uri)
         (string-match "\\`spotify:\\([a-z]+\\):\\([^:]+\\)\\'" uri))
    (cons (match-string 1 uri) (match-string 2 uri)))
   ((and (stringp uri)
         (string-match (concat "\\`https?://open\\.spotify\\.com/"
                               "\\(?:intl-[a-z-]+/\\)?"
                               "\\([a-z]+\\)/\\([^/?#]+\\)")
                       uri))
    (cons (match-string 1 uri) (match-string 2 uri)))))

(defun orpheus-spotify--spotify-uri-p (uri)
  "Return non-nil when URI names something on Spotify."
  (and (orpheus-spotify--parts uri) t))

(defun orpheus-spotify--id (uri)
  "Return the bare id URI names, whether it is a URI or a web link."
  (cdr (orpheus-spotify--parts uri)))

(defun orpheus-spotify--play (client &optional body)
  "Tell CLIENT to play BODY, which names what, and report a refusal.
Spotify answers with nothing when it accepts and with its own words when it
does not, which is how a device that is not running gets said out loud."
  (orpheus-spotify--request
   client "PUT" "/me/player/play" body
   (lambda (reply)
     (if (plist-get reply :ok)
         (orpheus-spotify--report client nil)
       (orpheus-spotify--report client (plist-get reply :error))))))

(defun orpheus-spotify--play-body (uri)
  "Return what to send Spotify to play URI.
A track is played as a list of one, so the queue becomes that track; an album,
a playlist or an artist is played as a context, which is the whole thing."
  (let ((kind (car (orpheus-spotify--parts uri))))
    (if (member kind '("album" "playlist" "artist" "show"))
        (list (cons 'context_uri uri))
      (list (cons 'uris (vector uri))))))

(defun orpheus-spotify--seek-position (client step)
  "Return where STEP seconds from now is for CLIENT, in milliseconds.
Spotify only seeks to a place, so the step is added to the last known position
of CLIENT, which is also the one the progress bar shows."
  (let ((status (orpheus-spotify--status-for-player client)))
    (round (* 1000 (max 0 (+ (if status
                                 (orpheus-status-elapsed status)
                               0)
                             (or step 0)))))))

(defun orpheus-spotify--ask-modes (client repeat single)
  "Record REPEAT and SINGLE as CLIENT's wish, then ask Spotify for what they mean.
Spotify has one repeat state, not two flags: repeating one track wins over
repeating the queue.  Keeping both wishes here is what lets the player's cycle
key turn one off without undoing the other, since its two commands arrive
before either answer does."
  (setf (orpheus-spotify--mode-repeat client) repeat
        (orpheus-spotify--mode-single client) single)
  (orpheus-spotify--repeat client (cond (single "track")
                                        (repeat "context")
                                        (t "off"))))

(defun orpheus-spotify--repeat (client state)
  "Ask CLIENT for repeat STATE, and report a refusal."
  (orpheus-spotify--request
   client "PUT"
   (orpheus-spotify--with-query "/me/player/repeat" (list (cons "state" state)))
   nil
   (lambda (reply)
     (unless (plist-get reply :ok)
       (orpheus-spotify--report client (plist-get reply :error))))))

(cl-defmethod orpheus-backend-command ((client orpheus-spotify) action
                                       &optional value)
  "Perform ACTION on CLIENT, with VALUE when it takes one."
  (let ((status (orpheus-spotify--status client))
        (send (lambda (method path &optional body)
                (orpheus-spotify--request
                 client method path body
                 (lambda (reply)
                   (unless (plist-get reply :ok)
                     (orpheus-spotify--report client
                                              (plist-get reply :error))))))))
    (pcase action
      ('play (orpheus-spotify--play client))
      ('pause (funcall send "PUT" "/me/player/pause"))
      ('toggle (if (eq (and status (orpheus-status-state status)) 'play)
                   (funcall send "PUT" "/me/player/pause")
                 (orpheus-spotify--play client)))
      ;; Spotify has no stop: pausing is what keeps the queue where it is.
      ('stop (funcall send "PUT" "/me/player/pause"))
      ('next (funcall send "POST" "/me/player/next"))
      ('previous (funcall send "POST" "/me/player/previous"))
      ('seek (funcall send "PUT"
                      (orpheus-spotify--with-query
                       "/me/player/seek"
                       (list (cons "position_ms"
                                   (number-to-string
                                    (orpheus-spotify--seek-position client
                                                                    value)))))))
      ('volume (funcall send "PUT"
                        (orpheus-spotify--with-query
                         "/me/player/volume"
                         (list (cons "volume_percent"
                                     (number-to-string
                                      (max 0 (min 100 (round (or value 0))))))))))
      ('random (funcall send "PUT"
                        (orpheus-spotify--with-query
                         "/me/player/shuffle"
                         (list (cons "state" (if value "true" "false"))))))
      ('repeat (orpheus-spotify--ask-modes
                client (and value t) (orpheus-spotify--mode-single client)))
      ('single (orpheus-spotify--ask-modes
                client (orpheus-spotify--mode-repeat client) (and value t)))
      ('playpos (orpheus-spotify--play-from-queue client value))
      ('update (message "orpheus: Spotify keeps its own library; nothing to scan"))
      ('rescan (message "orpheus: Spotify keeps its own library; nothing to rescan"))
      ('consume (message "orpheus: Spotify has no consume mode"))
      ('save (message "orpheus: Spotify playlists are saved on Spotify itself"))
      ('load (message "orpheus: open a playlist in the playlists tab to play it"))
      ('clear (message "orpheus: Spotify's queue cannot be emptied from here"))
      ('shuffle (message "orpheus: Spotify shuffles with the random key"))
      ('delete (message "orpheus: Spotify's queue cannot be edited from here"))
      (_ (message "orpheus: Spotify cannot %s" action)))))

(defun orpheus-spotify--play-from-queue (client position)
  "Play CLIENT's queue from POSITION on.
Spotify has no command that jumps inside its queue, so the tracks from
POSITION on are sent as what to play: the queue is then that list, which is
the one the tab showed."
  (orpheus-spotify--request
   client "GET" "/me/player/queue" nil
   (lambda (reply)
     (if (not (plist-get reply :ok))
         (orpheus-spotify--report client (plist-get reply :error))
       (let* ((json (plist-get reply :json))
              (all (cons (orpheus-spotify--field json "currently_playing")
                         (orpheus-spotify--field json "queue")))
              (uris (delq nil (mapcar (lambda (item)
                                        (orpheus-spotify--field item "uri"))
                                      (nthcdr (or position 0) all)))))
         (if uris
             (orpheus-spotify--play client (list (cons 'uris (vconcat uris))))
           (message "orpheus: there is no queue row there")))))))

(cl-defmethod orpheus-backend-play-uri ((client orpheus-spotify) uri)
  "Play URI on CLIENT now, without discarding the rest of the queue."
  (if (orpheus-spotify--spotify-uri-p uri)
      (orpheus-spotify--play client (orpheus-spotify--play-body uri))
    (message "orpheus: Spotify plays its own tracks, not %s" uri)))

(cl-defmethod orpheus-backend-add ((client orpheus-spotify) uris)
  "Append URIS to CLIENT's queue, one request each, in order.
Spotify takes a single track per request, so the requests follow each other
rather than racing, and a refusal stops the rest with its reason shown."
  (let ((uris (if (listp uris) uris (list uris))))
    (cl-labels
        ((queue-one
          (rest)
          (let ((uri (car rest)))
            (if (null uri)
                (progn (orpheus-spotify--report client nil)
                       (message "orpheus: queued %d track(s) on Spotify"
                                (length uris)))
              (if (not (orpheus-spotify--spotify-uri-p uri))
                  (progn
                    (message "orpheus: Spotify queues its own tracks, not %s"
                             uri)
                    (queue-one (cdr rest)))
                (orpheus-spotify--request
                 client "POST"
                 (orpheus-spotify--with-query "/me/player/queue"
                                              (list (cons "uri" uri)))
                 nil
                 (lambda (reply)
                   (if (plist-get reply :ok)
                       (queue-one (cdr rest))
                     (orpheus-spotify--report client (plist-get reply :error))))))))))
      (queue-one uris))))

;;; Devices

(defun orpheus-spotify--current ()
  "Return the Spotify backend in use, or nil."
  (and (boundp 'orpheus-backend-current)
       (orpheus-spotify-p orpheus-backend-current)
       orpheus-backend-current))

(defun orpheus-spotify--choose-device (client devices)
  "Ask which of DEVICES to play on, then move CLIENT's playback there."
  (let* ((choices (mapcar (lambda (device)
                            (cons (format "%s (%s)"
                                          (orpheus-spotify--field device "name")
                                          (orpheus-spotify--field device "type"))
                                  (orpheus-spotify--field device "id")))
                          devices))
         (answer (completing-read "Play on: " choices nil t))
         (id (cdr (assoc answer choices))))
    (orpheus-spotify--request
     client "PUT" "/me/player/transfer"
     (list (cons 'device_ids (vector id)) (cons 'play t))
     (lambda (reply)
       (if (plist-get reply :ok)
           (progn
             (orpheus-spotify--report client nil)
             (message "orpheus: Spotify plays on %s" answer))
         (message "orpheus: Spotify would not move playback: %s"
                  (plist-get reply :error)))))))

(defun orpheus-spotify--list-devices (client)
  "List the devices Spotify sees for CLIENT, then ask which one to play on."
  (orpheus-spotify--request
   client "GET" "/me/player/devices" nil
   (lambda (reply)
     (if (not (plist-get reply :ok))
         (message "orpheus: cannot list Spotify's devices: %s"
                  (plist-get reply :error))
       (let ((devices (orpheus-spotify--field (plist-get reply :json)
                                              "devices")))
         (if (null devices)
             (message "orpheus: Spotify sees no device; start the app on one")
           ;; A question cannot be asked from inside a process filter.
           (run-at-time 0.01 nil
                        (lambda ()
                          (orpheus-spotify--choose-device client
                                                          devices)))))))))

(defun orpheus-spotify-devices ()
  "Choose the device Spotify plays on, from the ones it can see.
A phone, a desktop app or spotifyd is a device: playback needs one of them
running, and this is how a different one is picked.  It works before the
player is open, since the token it needs is already on disk."
  (interactive)
  (let ((client (orpheus-spotify--current)))
    (cond
     (client (orpheus-spotify--list-devices client))
     ((not (eq (bound-and-true-p orpheus-backend) 'spotify))
      (user-error "The Spotify backend is not the one in use"))
     (t
      (let ((fresh (orpheus-spotify--make)))
        (orpheus-backend-connect
         fresh
         (lambda (connected)
           (if connected
               (orpheus-spotify--list-devices connected)
             (message "orpheus: %s" (orpheus-spotify--sign-in-hint fresh))))))))))

;;; Cover art

(defun orpheus-spotify--fetch-file (url file callback)
  "Fetch URL into FILE, then call CALLBACK with FILE or nil.
The bytes are written from the reply buffer rather than taken out as text, so
an image survives the trip unchanged."
  (condition-case err
      (let ((url-request-method "GET")
            (url-mime-encoding-string nil))
        (url-retrieve
         url
         (lambda (status)
           (let ((buffer (current-buffer)))
             (with-current-buffer buffer
               (set-buffer-multibyte nil)
               (goto-char (point-min))
               (if (or (plist-get status :error)
                       (not (re-search-forward "\r?\n\r?\n" nil t)))
                   (progn (when (buffer-live-p buffer) (kill-buffer buffer))
                          (funcall callback nil))
                 (let ((start (point)))
                   (condition-case nil
                       (progn
                         (write-region start (point-max) file nil 'silent)
                         (when (buffer-live-p buffer) (kill-buffer buffer))
                         (funcall callback file))
                     (error
                      (when (buffer-live-p buffer) (kill-buffer buffer))
                      (funcall callback nil))))))))
         nil t t))
    (error
     (ignore err)
     (funcall callback nil))))

(defun orpheus-spotify--cover (client uri callback)
  "Fetch the cover of URI from CLIENT, then call CALLBACK with its file.
The URL a listing already gave for that track is used as it is; only a track
nothing has listed yet costs the lookup that finds its album's images."
  (let ((file (orpheus-cache-file uri "cover.jpg"))
        (known (gethash uri orpheus-spotify--art-urls))
        (id (orpheus-spotify--id uri)))
    (cond
     (known (orpheus-spotify--fetch-file known file callback))
     ((null id) (funcall callback nil))
     (t
      (orpheus-spotify--request
       client "GET" (format "/tracks/%s" id) nil
       (lambda (reply)
         (let* ((item (plist-get reply :json))
                (url (orpheus-spotify--field
                      (car (orpheus-spotify--field
                            (orpheus-spotify--field item "album") "images"))
                      "url")))
           (if (not url)
               (funcall callback nil)
             (puthash uri url orpheus-spotify--art-urls)
             (orpheus-spotify--fetch-file url file callback)))))))))

(cl-defmethod orpheus-backend-album-art ((client orpheus-spotify) track callback)
  "Fetch TRACK's cover from CLIENT, then call CALLBACK with its file.
The cover belongs to the album, so the biggest image the album lists is the
one worth downloading, and the cache keeps it until the library changes."
  (let ((uri (orpheus-track-uri track)))
    (if (and (stringp uri) (not (string-empty-p uri)))
        (orpheus-spotify--cover client uri callback)
      (funcall callback nil))))

;;; Backend methods

(cl-defmethod orpheus-backend-connected-p ((client orpheus-spotify))
  "Return non-nil when CLIENT holds a token Spotify has not refused."
  (and (orpheus-spotify--token client)
       (not (orpheus-spotify--auth-failed client))
       t))

(cl-defmethod orpheus-backend-connect ((client orpheus-spotify) callback)
  "Read CLIENT's stored token, renew it when it has run out, then call CALLBACK."
  (if (null (orpheus-spotify--client-id client))
      (progn
        (message "orpheus: %s" (orpheus-spotify--sign-in-hint client))
        (funcall callback nil))
    (orpheus-spotify--adopt-token
     client (orpheus-spotify--read-token (orpheus-spotify--token-file client)))
    (when (null (orpheus-spotify--token client))
      (setf (orpheus-spotify--auth-failed client) nil))
    (if (orpheus-spotify--token client)
        (orpheus-spotify--with-token
         client
         (lambda (token)
           (when token
             (setf (orpheus-spotify--status-at client) 0
                   (orpheus-spotify--pending client) nil))
           (funcall callback (when token client))))
      (message "orpheus: %s" (orpheus-spotify--sign-in-hint client))
      (funcall callback nil))))

(cl-defmethod orpheus-backend-disconnect ((client orpheus-spotify))
  "Stop talking to Spotify for CLIENT, keeping the token for the next session."
  (orpheus-spotify--stop-listening)
  (setf (orpheus-spotify--status client) nil
        (orpheus-spotify--track client) nil
        (orpheus-spotify--status-at client) 0
        (orpheus-spotify--pending client) nil
        (orpheus-spotify--auth-failed client) nil
        (orpheus-spotify--token client) nil))

(cl-defmethod orpheus-backend-status ((client orpheus-spotify) callback)
  "Ask Spotify what CLIENT is playing, then call CALLBACK with an `orpheus-status'."
  (orpheus-spotify--read-player
   client
   (lambda ()
     (funcall callback
              (let ((status (orpheus-spotify--status-for-player client)))
                (when (and status (not (orpheus-spotify--auth-failed client)))
                  status))))))

(cl-defmethod orpheus-backend-track ((client orpheus-spotify) callback)
  "Ask Spotify for the track CLIENT is playing, then call CALLBACK with it."
  (orpheus-spotify--read-player
   client
   (lambda () (funcall callback (orpheus-spotify--track client)))))

(cl-defmethod orpheus-backend-capabilities ((_client orpheus-spotify))
  "Return the tabs Spotify can fill.
There are no directories: a Spotify library is the playlists, the saved
albums, the followed artists and the saved songs."
  '(queue artists albums playlists search))

(defun orpheus-spotify--tracks-of (client path callback)
  "Read the tracks of PATH on CLIENT, then call CALLBACK with their rows.
A playlist and the saved songs wrap each track in an object of their own; an
album lists the tracks themselves.  A track that was removed from a playlist
is only a nil in that wrapper, and it is skipped."
  (orpheus-spotify--paged
   client path nil #'orpheus-spotify--items
   (lambda (entry)
     (orpheus-spotify--track-row (or (orpheus-spotify--field entry "track")
                                     entry)))
   callback))

(cl-defmethod orpheus-backend-browse ((client orpheus-spotify) kind arg callback)
  "Browse KIND in Spotify on behalf of CLIENT, with ARG as the path down it."
  (let ((arg (if (listp arg) arg (when arg (list arg)))))
    (pcase kind
      ('playlists
       (cond
        ((null arg)
         (orpheus-spotify--paged
          client "/me/playlists" nil #'orpheus-spotify--items
          #'orpheus-spotify--playlist-row
          (lambda (rows) (funcall callback (cons (orpheus-spotify--liked-row)
                                                 rows)))))
        ((equal (car arg) "liked")
         (orpheus-spotify--tracks-of client "/me/tracks" callback))
        (t
         (orpheus-spotify--tracks-of
          client (format "/playlists/%s/tracks" (car arg)) callback))))
      ('albums
       (if arg
           (orpheus-spotify--tracks-of
            client (format "/albums/%s/tracks" (car arg)) callback)
         (orpheus-spotify--paged
          client "/me/albums" nil #'orpheus-spotify--items
          (lambda (entry)
            (orpheus-spotify--album-row (orpheus-spotify--field entry "album")))
          callback)))
      ('artists
       (pcase (length arg)
         (0 (orpheus-spotify--followed-artists client callback))
         (1 (orpheus-spotify--paged
             client (format "/artists/%s/albums" (car arg))
             '(("include_groups" . "album,single"))
             #'orpheus-spotify--items
             (lambda (album) (orpheus-spotify--album-row album arg))
             callback))
         (_ (orpheus-spotify--tracks-of
             client (format "/albums/%s/tracks" (car (last arg)))
             callback))))
      ('search (orpheus-spotify--search client (car arg) callback))
      ('queue (orpheus-backend-queue client callback))
      (_ (funcall callback nil)))))

(defun orpheus-spotify--search (client query callback)
  "Search CLIENT for QUERY, then call CALLBACK with the rows.
The rows are in the order a search is read: the tracks first, because they
are what plays, then the artists, albums and playlists that lead there.  An
empty query answers nothing: Spotify would refuse it, and an empty tab says
the same thing without the request."
  (if (or (null query) (string-empty-p (string-trim query)))
      (funcall callback nil)
    (orpheus-spotify--request
   client "GET"
   (orpheus-spotify--with-query "/search"
                                (list (cons "q" (or query ""))
                                      (cons "type" "track,artist,album,playlist")
                                      (cons "limit" "10")))
   nil
   (lambda (reply)
     (if (not (plist-get reply :ok))
         (progn
           (orpheus-spotify--report client (plist-get reply :error))
           (funcall callback nil))
       (let ((json (plist-get reply :json))
             (rows nil))
         (dolist (section '(("tracks" . track) ("artists" . artist)
                            ("albums" . album) ("playlists" . playlist)))
           (let ((entries (orpheus-spotify--items
                           (orpheus-spotify--field json (car section))))
                 (parse (pcase (cdr section)
                          ('track #'orpheus-spotify--track-row)
                          ('artist #'orpheus-spotify--artist-row)
                          ('album #'orpheus-spotify--album-row)
                          (_ #'orpheus-spotify--playlist-row))))
             (setq rows (append rows (delq nil (mapcar parse entries))))))
         (funcall callback rows)))))))

(cl-defmethod orpheus-backend-queue ((client orpheus-spotify) callback)
  "Ask Spotify for CLIENT's queue, then call CALLBACK with the rows.
The playing track comes first, so a row's position is where it sits in the
list the tab shows, which is also what playing that row means."
  (orpheus-spotify--request
   client "GET" "/me/player/queue" nil
   (lambda (reply)
     (if (not (plist-get reply :ok))
         (progn
           (orpheus-spotify--report client (plist-get reply :error))
           (funcall callback nil))
       (let ((json (plist-get reply :json))
             (pos 0)
             (rows nil))
         (dolist (item (cons (orpheus-spotify--field json "currently_playing")
                             (orpheus-spotify--field json "queue")))
           (let ((row (orpheus-spotify--track-row item)))
             (when row
               (setf (orpheus-item-detail row)
                     (if (zerop pos) "playing now"
                       (orpheus-item-detail row)))
               (setf (orpheus-item-data row)
                     (append (orpheus-item-data row)
                             (list (cons 'pos pos))))
               (push row rows))
             (setq pos (1+ pos))))
         (funcall callback (nreverse rows)))))))

(provide 'orpheus-spotify)

;;; orpheus-spotify.el ends here
