;; orpheus.el

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; SPDX-License-Identifier: MIT

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-13)
  ;; The container helpers are imported under a prefix because the tasks below
  ;; are named the way maak exposes them on the command line -- `maak
  ;; container-build' -- and a task may not shadow the helper it calls.
  #:use-module ((maak container) #:prefix ct:))

;; ====== Maak constants ======

;; The Emacs the package is compiled and tested with, overridable with the
;; EMACS environment variable, exactly as one would expect.
(define emacs-command
  (or (getenv "EMACS") "emacs"))

;; The package: seven files, loaded in this order.
(define source-files
  '("orpheus-backend.el"
    "orpheus-mpd.el"
    "orpheus-local.el"
    "orpheus-ssh.el"
    "orpheus-ui.el"
    "orpheus-vis.el"
    "orpheus.el"))

;; The ERT suites: the unit suite needs nothing but Emacs, the integration
;; suite starts a real MPD and a real mpv and skips itself when they are
;; missing.
(define test-file
  "test/orpheus-test.el")

(define integration-file
  "test/orpheus-integration-test.el")

(define checkdoc-file
  "test/orpheus-checkdoc.el")

;; What byte-compilation leaves behind.
(define compiled-files
  '("orpheus-backend.elc" "orpheus-mpd.elc" "orpheus-local.elc"
    "orpheus-ssh.elc" "orpheus-ui.elc" "orpheus-vis.elc" "orpheus.elc"
    "test/orpheus-test.elc" "test/orpheus-integration-test.elc"
    "test/orpheus-checkdoc.elc"))

;; ====== Maak tasks ======

(define (compile)
  "Byte-compile the package, then its test suites."
  (run (append (list emacs-command "-Q" "--batch" "-L" "." "-L" "test"
                     "-f" "batch-byte-compile")
               source-files
               (list test-file integration-file checkdoc-file))
       #:verbose? #t))

(define (test)
  "Run the unit suite: no daemon, no socket, no audio device."
  (run (list emacs-command "-Q" "--batch" "-L" "." "-L" "test"
             "-l" test-file "-f" "ert-run-tests-batch-and-exit")
       #:verbose? #t))

(define (integration)
  "Run the suite that plays through a real MPD and a real mpv.
Both are skipped when they are not installed, so this task always finishes."
  (run (list emacs-command "-Q" "--batch" "-L" "." "-L" "test"
             "-l" integration-file "-f" "ert-run-tests-batch-and-exit")
       #:verbose? #t))

(define (lint)
  "Compile with warnings turned into errors, then enforce checkdoc."
  (run (append (list emacs-command "-Q" "--batch" "-L" "." "-L" "test"
                     "--eval" "(setq byte-compile-error-on-warn t)"
                     "-f" "batch-byte-compile")
               source-files
               (list test-file integration-file checkdoc-file))
       #:verbose? #t)
  (run (list emacs-command "-Q" "--batch" "-L" "."
             "-l" checkdoc-file)
       #:verbose? #t))

(define (clean)
  "Remove the byte-compiled files."
  (for-each (lambda (file)
              (when (file-exists? file) (delete-file file)))
            compiled-files))

(define (check)
  "The whole quality gate, from a clean tree: lint, then test."
  (clean)
  (lint)
  (test))

(define (repl)
  "Open an Emacs with the package loaded for hacking."
  (run (list emacs-command "-Q" "-L" "."
             "--eval" "(progn (require (quote orpheus)) (orpheus))")))

(define (tasks)
  "List the available tasks."
  (list-tasks))

;; `maak' on its own runs `check', that is: the whole gate in one command.
(define (default)
  "Run the full quality gate (lint and tests)."
  (check))

;; ====== Agent skills ======

;; AGENTS.md is generated, never edited by hand.  The section has two halves:
;; the data below, which is this project's, and the task itself, which is not.
;; Another repository copies the whole section and replaces the data - the
;; header, the pinned skills (repository, commit, path, licence) and the
;; practices - and `maak agents' writes its own AGENTS.md.  Regeneration is
;; deterministic: the same pins and the same text produce the same file, byte
;; for byte, and it needs nothing but curl.  The skill URLs assume GitHub, so a
;; skill hosted elsewhere needs a URL of its own.

(define ponytail-commit
  "96608e078b2d5ea607c5c3b8b5a54d1c1fba6846")

(define tdd-commit
  "22f0b463e7a5e579904dd6211a520321035526f7")

;; Name, repository, commit, the path the repository keeps the skill at and the
;; licence.  A skill arrives whole: trimming it means scanning for headings, and
;; such a scan cannot tell a heading from a `##' line inside a code fence.
(define agent-skills
  (list (list "ponytail"
              "DietrichGebert/ponytail"
              ponytail-commit
              "skills/ponytail/SKILL.md"
              "MIT")
        (list "test-driven-development"
              "obra/superpowers"
              tdd-commit
              "skills/test-driven-development/SKILL.md"
              "MIT")))

(define agent-skills-dir
  "reports/agent-skills")

(define agents-header
  "# AGENTS.md

Generated by `maak agents` from the pinned skills and the practices in
`maak.scm`; edit that task, never this file.  The pins and the text are the whole
input, so regenerating gives the same document.  The repository's own rules win
over general advice, and the narrowest rule here wins over the broadest.
")

;; Practices this project keeps.  They are short on purpose: a rule a reader
;; checks in a second is a rule that gets followed.  The pinned skills are the
;; long form of the same habits.
(define agent-practices
  (list
   "## Documentation

- Every user-visible change reaches the documentation: the page that owns the
  feature, the manual page for a flag or an environment variable, and the
  release notes under the unreleased heading.
- Keep the table of contents in step with the headings.  A section with no entry
  is a section nobody finds.
- The guide is Org: one `*` heading per page, `**` for sections, `#+TITLE` and
  `#+EMAIL` at the top, a `CUSTOM_ID` on anything linked from elsewhere, and
  `file:` links between pages.
- Explain why and when, never what: the code already says what.  A docstring's
  first line works as a summary on its own.
- Spell an identifier the way the code spells it - the exact name, not a
  description of it.
- An example must run as written, copied from a real invocation.
- No promises about the future and no changelog in the guide; history lives in
  the release notes.
"
   "## Security

- No secrets in the tree.  Tokens, keys and passwords arrive through the
  environment, and no recipe, workflow or example embeds one.
- Validate at the edge: anything from a file, a report, a path or a subprocess
  is untrusted until it has been checked.
- Never build a shell string out of data.  Tasks pass argument lists, and Guile
  calls `(system \"program\" \"arg\")` or `open-pipe*`, never a command
  assembled with `string-append`.
- Do not `eval` or `read` input you did not write; parse with `(json)` or a
  reader built for the format.
- Escape what leaves the process: a name that came from input is escaped when
  it is written to XML, HTML or a data file.
- Fail closed.  A file that cannot be read is an error, not an empty result.
- Input never chooses the path of a written file; writes stay inside the
  directory reserved for them.
- A dependency is a security decision: it must be needed, MIT compatible and
  pinned.
- Report a vulnerability privately, not in a public issue.
"
   "## Performance

- Measure first.  A before and after belongs in the benchmark, and a speedup
  claimed without a number is not a speedup.
- Prefer not doing the work: cache it, skip it, batch it, or delete the call,
  before making the call faster.
- Then the algorithm, then the data structure (hash table, vector), then the
  micro-optimisation.  In that order, for that reason.
- Guile specifics: keep module resolution and long `assoc` chains out of loops,
  use `#:declarative?` modules and compiled `.go` files at startup, and reach
  for `statprof` instead of guessing where the time goes.
- A shortcut that buys speed costs clarity; it gets a measurement and a
  `shortcut:` comment saying what it costs and when to revisit it.
- Coverage instrumentation is slow by design: iterate without it, and measure
  before finishing.
"
   "## Architecture decisions

- A decision that is expensive to reverse gets a short record: the context, the
  decision, the consequences, and the alternatives that lost.  A small decision
  belongs in the commit body instead.
- Two paragraphs beat a design document; record the choice while it is still a
  choice, not after it has become a fact.
- An accepted record is not rewritten.  Supersede it with a new one and link the
  two directions.
- The record lives next to its subject: the documentation for anything a user
  would notice, the release notes for the release-level result.
- Changes to the public API, a file format, the pipeline or the licence get this
  treatment at a minimum in the commit body.
"
   "## Semantic versioning

- The project follows SemVer 2.0.0 for its public API: the exported functions,
  the CLI flags and environment variables, and the shape of its file formats.
- A breaking change bumps MAJOR, a new feature MINOR, a fix PATCH.  A new export
  is MINOR; removing or changing one is MAJOR.
- One version string, named everywhere it appears: the source, the build
  recipes, the manual page and the packaging.
- A release is a dated release-notes section, an annotated `vX.Y.Z` tag on the
  release commit, and a packaging checksum updated afterwards from the published
  archive.
"
   "## Conventional commits

- `type(scope): summary`, imperative, lower case, no full stop, about seventy
  characters.
- Types in use: `feat`, `fix`, `perf`, `refactor`, `test`, `docs`, `build`, `ci`,
  `chore`.  A `!` after the type or a `BREAKING CHANGE:` footer marks a MAJOR
  change, and says what breaks.
- The subject says what changed.  The body says why, and what was rejected.
- One logical change per commit: a refactor and a fix travel apart, so either
  can be reverted alone.
- Run the full validation task before committing, and never commit a red suite
  without saying so in the message.
- Reference the issue in the footer - `Closes #12` - not in the subject.
"
   "## Functional programming and immutability

- Pure functions in the core, effects at the edges.  Whatever touches the file
  system or the clock sits at the edge; the core does not.
- Prefer `let` and new values to `set!`.  Build results with `map`, `filter`,
  `fold` and named `let`, not with index loops that push into a list.
- A record is a value: construct a new one instead of mutating a shared one, and
  pass configuration as arguments or parameters rather than as globals.
- Handle an exception at the boundary: catch it there, turn it into a result
  record, and let the caller decide what it means.
- A function that both computes and prints is two functions.
- Temporary effects - a changed directory, a bound environment variable - go
  through `dynamic-wind`.
- Prefer the expression style: `if` and `cond` that return a value, not a branch
  that assigns one.
"
   "## Do not reinvent the wheel

- Guile's standard library first: `(ice-9 ...)` for the runtime, `(srfi srfi-1)`
  for lists, `(srfi srfi-9)` for records, `(srfi srfi-13)` for strings, `(json)`
  for JSON.  The project already depends on all of them.
- Then this codebase: read the existing modules before writing a helper, and use
  the one that is nearly right instead of adding a second one.
- A new dependency needs a reason the standard library cannot meet, a licence
  the project can carry, and a paragraph in the commit body.
- Prefer deleting a special case to writing code that handles it.
- The best diff removes lines and keeps the behaviour; the second best is a
  small diff that is obvious a year later.
"))

(define (slurp path)
  "Return the whole of @var{path} as a string."
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '())
                 (line (read-line port)))
        (if (eof-object? line)
            (string-concatenate-reverse lines)
            (loop (cons (string-append line "\n") lines)
                  (read-line port)))))))

(define (fetch-skill name repository commit path)
  "Download one skill pinned at @var{commit}, into the reports tree.

  Returns the skill's text."
  (let ((target (~ "~a/~a.md" agent-skills-dir name))
        (url (~ "https://raw.githubusercontent.com/~a/~a/~a"
                repository commit path)))
    (run (list "curl" "--fail" "--silent" "--show-error" "--location"
               "--output" target url)
         #:verbose? #t)
    (slurp target)))

(define (agents)
  "Regenerate AGENTS.md from the pinned skills and the practices."
  (run (list "mkdir" "-p" agent-skills-dir))
  ;; Every download happens before the file is opened, so a fetch that fails
  ;; leaves the previous AGENTS.md in place rather than a half-written one.
  (let ((skills (map (lambda (skill)
                       (append skill
                               (list (fetch-skill (list-ref skill 0)
                                                  (list-ref skill 1)
                                                  (list-ref skill 2)
                                                  (list-ref skill 3)))))
                     agent-skills)))
    (call-with-output-file "AGENTS.md"
      (lambda (port)
        (display agents-header port)
        (for-each (lambda (skill)
                    (format port
                            "~%## ~a~%~%`~a` at `~a`, `~a`, ~a licensed.~%~%"
                            (list-ref skill 0)
                            (list-ref skill 1)
                            (list-ref skill 2)
                            (list-ref skill 3)
                            (list-ref skill 4))
                    (display (list-ref skill 5) port))
                  skills)
        (for-each (lambda (practice)
                    (newline port)
                    (display practice port))
                  agent-practices)))))
;; ====== Releasing ======

;; A release is the one task that writes to the repository, tags, pushes and
;; publishes.  It refuses to start anywhere but a clean, synced trunk, and it
;; runs the gate after the version and changelog are closed; a red gate puts
;; both files back the way they were.

(define (release-capture program . args)
  "Run PROGRAM with ARGS and return its standard output.
Signal an error when PROGRAM exits non-zero."
  (let ((port (apply open-pipe* OPEN_READ program args)))
    (let loop ((lines '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (let ((status (close-pipe port)))
              (unless (zero? status)
                (error (format #f "~a ~a failed with status ~a"
                               program (string-join args " ") status)))
              (string-concatenate-reverse
               (map (lambda (line) (string-append line "\n")) lines)))
            (loop (cons line lines)))))))

(define (release-trim text)
  "Remove leading and trailing whitespace from TEXT."
  (let loop ((start 0) (end (string-length text)))
    (cond ((and (< start end) (char-whitespace? (string-ref text start)))
           (loop (1+ start) end))
          ((and (< start end) (char-whitespace? (string-ref text (1- end))))
           (loop start (1- end)))
          (else (substring text start end)))))

(define (release-git-output . args)
  "Return what `git ARGS' writes, with surrounding whitespace removed."
  (release-trim (apply release-capture "git" args)))

(define (release-require-clean-tree)
  "Refuse to release with local changes."
  (unless (string-null? (release-git-output "status" "--porcelain"))
    (error "release: the worktree is not clean; commit or stash first")))

(define (release-require-synced-trunk)
  "Refuse to release from a branch other than a trunk at origin/trunk."
  (let ((branch (release-git-output "branch" "--show-current")))
    (unless (string=? branch "trunk")
      (error (format #f "release: on branch ~s, not trunk" branch))))
  (run (list "git" "fetch" "origin" "trunk") #:verbose? #t)
  (let ((head (release-git-output "rev-parse" "HEAD"))
        (remote (release-git-output "rev-parse" "origin/trunk")))
    (unless (string=? head remote)
      (error "release: trunk and origin/trunk differ; push or pull first"))))

(define (release-require-tag-absent tag)
  "Refuse to reuse TAG."
  (when (zero? (system* "git" "show-ref" "--verify" "--quiet" tag))
    (error (format #f "release: tag ~a already exists" tag))))

(define (release-require-gh)
  "Refuse to publish when gh is not authenticated."
  (unless (zero? (system* "gh" "auth" "status"))
    (error "release: gh is not authenticated")))

(define (release-version-parts version)
  "Return VERSION's three numbers, or signal an error."
  (let ((match (string-match "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$" version)))
    (unless match
      (error (format #f "release: ~s is not an X.Y.Z version" version)))
    (map (lambda (n) (string->number (match:substring match n))) '(1 2 3))))

(define (release-version>? a b)
  "Return true when version A is newer than version B."
  (let loop ((a (release-version-parts a))
             (b (release-version-parts b)))
    (cond ((null? a) #f)
          ((> (car a) (car b)) #t)
          ((< (car a) (car b)) #f)
          (else (loop (cdr a) (cdr b))))))

(define (release-next-version current bump)
  "Return the version after CURRENT for BUMP.
BUMP is an explicit X.Y.Z, or patch, minor or major."
  (cond
   ((string=? bump "patch")
    (let* ((parts (release-version-parts current))
           (major (car parts))
           (minor (cadr parts))
           (patch (caddr parts)))
      (format #f "~a.~a.~a" major minor (1+ patch))))
   ((string=? bump "minor")
    (let* ((parts (release-version-parts current))
           (major (car parts))
           (minor (cadr parts)))
      (format #f "~a.~a.0" major (1+ minor))))
   ((string=? bump "major")
    (let ((major (car (release-version-parts current))))
      (format #f "~a.0.0" (1+ major))))
   (else
    (release-version-parts bump)
    (unless (release-version>? bump current)
      (error (format #f "release: ~a is not newer than ~a" bump current)))
    bump)))

(define (release-source-version)
  "Return the version in orpheus.el, after checking both declarations."
  (let* ((source (slurp "orpheus.el"))
         (header (string-match
                  ";;; Version: ([0-9]+\\.[0-9]+\\.[0-9]+)" source))
         (constant (string-match
                    "defconst orpheus-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\""
                    source)))
    (unless (and header constant)
      (error "release: orpheus.el has no Version header or orpheus-version"))
    (let ((header-version (match:substring header 1))
          (constant-version (match:substring constant 1)))
      (unless (string=? header-version constant-version)
        (error "release: the Version header and orpheus-version disagree"))
      header-version)))

(define (release-replace text old new what)
  "Replace OLD with NEW in TEXT, or signal an error naming WHAT."
  (unless (string-contains text old)
    (error (format #f "release: cannot find ~a" what)))
  (regexp-substitute/global #f (regexp-quote old) text 'pre new 'post))

(define (release-source-text source current version)
  "Return SOURCE with CURRENT replaced by VERSION in both declarations."
  (release-replace
   (release-replace source
                    (format #f ";;; Version: ~a" current)
                    (format #f ";;; Version: ~a" version)
                    "the Version header")
   (format #f "(defconst orpheus-version \"~a\"" current)
   (format #f "(defconst orpheus-version \"~a\"" version)
   "orpheus-version"))

(define (release-news-text news version date)
  "Return NEWS with its Unreleased heading closed as VERSION on DATE."
  (let* ((marker "* Unreleased")
         (at (string-contains news marker))
         (start (and at (+ at (string-length marker))))
         (next (and start (string-contains news "\n* " start)))
         (body (and start (substring news start
                                     (or next (string-length news))))))
    (unless at
      (error "release: NEWS.org has no * Unreleased heading"))
    (unless (and body (string-match "[^ \t\n]" body))
      (error "release: the Unreleased section is empty"))
    (when (string-contains news (format #f "* ~a (" version))
      (error (format #f "release: NEWS.org already has a ~a section" version)))
    (release-replace news marker
                     (format #f "* ~a (~a)" version date)
                     "the Unreleased heading")))

(define (release-section news version)
  "Return the NEWS section for VERSION."
  (let* ((marker (format #f "* ~a (" version))
         (at (string-contains news marker)))
    (unless at
      (error (format #f "release: NEWS.org has no section for ~a" version)))
    (let* ((next (string-contains news "\n* "
                                 (+ at (string-length marker))))
           (end (or next (string-length news))))
      (release-trim (substring news at end)))))

(define (release-notes section)
  "Return SECTION as the Markdown body of a release."
  (let ((lines (map (lambda (line)
                      (cond ((string-prefix? "*** " line)
                             (string-append "### " (substring line 4)))
                            ((string-prefix? "** " line)
                             (string-append "## " (substring line 3)))
                            ((string-prefix? "* " line)
                             (string-append "# " (substring line 2)))
                            (else line)))
                    (string-split section #\newline))))
    (release-trim (string-join lines "\n"))))

(define (release-run-gate source news)
  "Run the full gate, restoring SOURCE and NEWS if it fails."
  (catch #t
    (lambda ()
      (check)
      (integration))
    (lambda (key . args)
      (release-write "orpheus.el" source)
      (release-write "NEWS.org" news)
      (apply throw key args))))

(define (release-write path text)
  "Write TEXT to PATH."
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (release . args)
  "Cut a release: bump VERSION, close NEWS, gate, commit, tag, push and publish.
VERSION is an explicit X.Y.Z, or patch, minor or major.
Usage: maak release -- 0.4.1"
  (when (dry-run?)
    (error "release: refusing to run under --dry-run"))
  (unless (= (length args) 1)
    (error "usage: maak release -- X.Y.Z|patch|minor|major"))
  (let* ((bump (car args))
         (current (release-source-version))
         (version (release-next-version current bump))
         (date (strftime "%Y-%m-%d" (localtime (current-time))))
         (tag (format #f "v~a" version))
         (source (slurp "orpheus.el"))
         (news (slurp "NEWS.org"))
         (new-source (release-source-text source current version))
         (new-news (release-news-text news version date)))
    (release-require-clean-tree)
    (release-require-synced-trunk)
    (release-require-tag-absent tag)
    (release-require-gh)
    (release-write "orpheus.el" new-source)
    (release-write "NEWS.org" new-news)
    (release-run-gate source news)
    (let* ((notes (release-notes (release-section new-news version)))
           (body (string-append notes
                                "\n\nThe gate is green: lint, unit tests, "
                                "and integration tests.\n")))
      (when (string-null? notes)
        (error "release: the changelog section is empty"))
      (run (list "git" "add" "NEWS.org" "orpheus.el") #:verbose? #t)
      (run (list "git" "commit" "-m" (format #f "chore(release): ~a" version)
                 "-m" body)
           #:verbose? #t)
      (run (list "git" "tag" "-a" tag "-m" (format #f "orpheus.el ~a" version))
           #:verbose? #t)
      (run (list "git" "push" "origin" "HEAD") #:verbose? #t)
      (run (list "git" "push" "origin" tag) #:verbose? #t)
      (run (list "gh" "release" "create" tag "--title" tag "--notes" notes)
           #:verbose? #t)
      (format #t "orpheus: released ~a\n" version))))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for orpheus-el."
  (ct:container-build "." #:tag "orpheus-el" #:verbose? #t))

(define (container-run)
  "Run the container image for orpheus-el."
  (ct:container-run "orpheus-el:latest"
                    #:interactive? #t
                    #:tty? #t))

(define (package-docker)
  "Build the container image for orpheus-el."
  (container-build))

;; Local Variables:
;; mode: scheme
;; End:
