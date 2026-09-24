;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Transparent distributed (multi-repository) Two-Phase-Commit (2PC)
;;; transactions on top of GitHack's existing, single-repository
;;; GIT-TRANSACTION/TRANSACTION machinery. From Prepare until the
;;; prepared commit is published, the participant branch is held
;;; with Git's own `refs/heads/<branch>.lock`, so an ordinary
;;; update-ref cannot move it. Recovery of a dead process publishes
;;; that same lock file.
;;;
;;; WITH-GITHACK-TRANSACTION/CALL-WITH-GITHACK-TRANSACTION bind
;;; *CURRENT-TRANSACTION* (declared very early, in distributed-
;;; transaction-context.lisp, precisely so every other file can
;;; cheaply check it) to a fresh GITHACK-TRANSACTION for the extent
;;; of a body that may open any number of ordinary, single-repository
;;; WITH-TRANSACTION/CALL-WITH-TRANSACTION or WITH-GIT-TRANSACTION/
;;; CALL-WITH-GIT-TRANSACTION blocks against any number of distinct
;;; repositories (or orphan branches within one repository). Each
;;; such single-repository transaction still runs its full, ordinary
;;; commit logic -- persisting its own root, wrapping it, building
;;; and persisting its own GIT-COMMIT -- except that
;;; COMMIT-GIT-TRANSACTION-NOW! (git-transaction.lisp), on noticing
;;; *CURRENT-TRANSACTION* is bound, stops just short of actually
;;; advancing its branch: it instead calls ENLIST-TRANSACTION-WRITE!,
;;; which records a PENDING-WRITE (repository, branch, old SHA, new
;;; commit SHA) onto *CURRENT-TRANSACTION*'s own PENDING-WRITES list.
;;;
;;; When WITH-GITHACK-TRANSACTION's body returns normally,
;;; %FINISH-GITHACK-TRANSACTION! inspects that list:
;;;   * Zero pending writes: no-op (nothing was ever mutated).
;;;   * Exactly one: the "Fast Path" -- a single atomic
;;;     `git update-ref --stdin` call, no 2PC overhead at all.
;;;   * More than one: the full distributed Two-Phase-Commit
;;;     protocol below.
;;;
;;; If the body signals an error instead, %FINISH-GITHACK-
;;; TRANSACTION! is never reached at all: every participant's own
;;; commit object was already flushed to its own Git object database
;;; by its own (inner) transaction, but never made reachable from any
;;; ref, so it is simply orphaned, harmless Git garbage -- exactly
;;; GIT-TRANSACTION's own existing single-repository rollback
;;; guarantee, now extended transparently across every participant at
;;; once, entirely for free.
;;;
;;; THE 2PC PROTOCOL ITSELF, for >1 participants:
;;;
;;; Phase 1 (PREPARE): the first participating repository
;;; encountered is elected the LEDGER. A Transaction Manifest --
;;; TX-ID, the Ledger's own pathname, and every participant's own
;;; (repository . branch . old-sha) -- is built once and reused for
;;; every participant. For each participant, in turn: create an
;;; annotated tag object (via `git mktag`, so a malformed manifest or
;;; missing tagger line is rejected up front rather than silently
;;; accepted) whose target is that participant's own already-
;;; persisted, not-yet-ref-visible commit and whose message is the
;;; Manifest, then point a tracking ref,
;;; `refs/githack/prepare/<tx-id>/<encoded-branch>`, at that tag.
;;; The branch is one percent-encoded segment (`/` is `%2F`), so a
;;; slash in the branch name is not a path separator. The raw name
;;; is only in the Manifest. If any
;;; participant's own Prepare step fails, every prepare ref already
;;; created for this transaction is deleted again (best-effort) and
;;; DISTRIBUTED-TRANSACTION-ERROR is signalled: nothing further
;;; happens, and no Ledger ref is ever written.
;;;
;;; Phase 2 (POINT OF NO RETURN & ROLL FORWARD): once every
;;; participant's own Prepare step has succeeded, a single
;;; `refs/githack/ledger/<tx-id>` ref (pointing at a fresh, otherwise
;;; meaningless blob) is written in the Ledger repository. The
;;; instant that ref exists, this transaction is permanently
;;; committed, no matter what happens next -- even if this very Lisp
;;; process crashes one line later. Every participant is then
;;; published by atomically replacing `refs/heads/<branch>` with the
;;; lock file written at Prepare (the new SHA and a newline). That
;;; replace both makes the prepared commit the branch tip and
;;; releases the lock. The prepare ref is deleted after the
;;; publish. A crash between those two leaves the branch already at
;;; the prepared commit; RUN-GITHACK-EXORCIST! sees that and only
;;; deletes the prepare ref. A crash before the publish leaves the
;;; lock file in place, so an ordinary update-ref still cannot move
;;; the branch, and the Exorcist performs the same replace.
;;;
;;; CRASH RECOVERY ("The Exorcist"): if this Lisp process dies
;;; between Phase 1 and Phase 2 (or partway through Phase 2's own
;;; roll-forward loop), some participants are left with a stranded
;;; `refs/githack/prepare/<tx-id>/...` ref and no corresponding real
;;; branch update. RUN-GITHACK-EXORCIST!, run against any single
;;; repository (on boot, or lazily on first access), finds every such
;;; stranded ref, reads its own annotated tag's Manifest back out to
;;; find the Ledger, and asks the Ledger directly whether
;;; `refs/githack/ledger/<tx-id>` exists: if so, the transaction had
;;; already passed its Point of No Return, so the stranded ref's own
;;; participant is rolled forward exactly as Phase 2 would have; if
;;; not, the crash happened during Phase 1, so the stranded ref is
;;; simply deleted, leaving the branch untouched and its orphaned
;;; commit for `git gc` to eventually collect.
;;;
;;; A participant's identity is the pair (repository, branch), not
;;; the branch name. ENLIST-TRANSACTION-WRITE! coalesces a second
;;; top-level write to the same pair in place and keeps the original
;;; old SHA, so a manifest contains at most one entry per pair.
;;; Recovery compares the repository as UIOP:NATIVE-NAMESTRING
;;; (EQUAL, not EQ of pathname objects) and the branch with STRING=,
;;; the same two checks enlist uses. Branch name alone is not an
;;; identity: two participants on "main" are the ordinary case, and
;;; the first plist with that name is the wrong compare-and-swap
;;; baseline for every later one.
;;;
;;; Every one of these operations relies solely on standard Git
;;; plumbing (`hash-object`, `cat-file`, `update-ref`, `update-ref
;;; --stdin`, `mktag`, `rev-parse`, `for-each-ref`) against ordinary
;;; refs and tag objects -- Git's own Merkle tree and refs are the
;;; entire source of truth; no external coordination service of any
;;; kind is used or required.

;;; ------------------------------------------------------------------
;;; Low-level, generic Git ref/tag plumbing, beyond GIT-BRANCH.LISP's
;;; own refs/heads-specific GIT-SHOW-REF-SHA/GIT-UPDATE-REF!: arbitrary
;;; ref paths, a batched atomic `update-ref --stdin` transaction,
;;; annotated tag objects via `mktag`, ref enumeration via
;;; `for-each-ref`, and ref-to-commit resolution via `rev-parse`.
;;; ------------------------------------------------------------------

(defun %git-run (repository args &key input)
  "Shell out to `git --git-dir=<REPOSITORY> ARGS...`, optionally
piping INPUT (a string, if supplied) to its standard input, and
return (VALUES OUTPUT ERROR-OUTPUT EXIT-CODE) exactly as
UIOP:RUN-PROGRAM does -- never signals on a non-zero exit code
itself; every caller below inspects EXIT-CODE and decides what, if
anything, to signal."
  (uiop:run-program (append (list "git" (format nil "--git-dir=~A" (uiop:native-namestring repository)))
                             args)
                     :input (and input (make-string-input-stream input))
                     :output :string
                     :error-output :string
                     :ignore-error-status t))

(defun %git-raw-show-ref (repository ref-path)
  "Like GIT-SHOW-REF-SHA, but against an arbitrary REF-PATH (e.g.
\"refs/githack/ledger/<tx-id>\") rather than being hardwired to
\"refs/heads/<name>\". Returns REF-PATH's current SHA, or NIL if it
does not exist."
  (multiple-value-bind (output error-output exit-code) (%git-run repository (list "show-ref" "--verify" "--hash" ref-path))
    (declare (ignore error-output))
    (and (zerop exit-code) (string-trim '(#\Space #\Newline #\Return) output))))

(defun %git-raw-update-ref! (repository ref-path sha &key (expected-sha :unconditional))
  "Like GIT-UPDATE-REF!, but against an arbitrary REF-PATH rather
than being hardwired to \"refs/heads/<name>\". EXPECTED-SHA has the
same meaning as GIT-UPDATE-REF!'s own argument of the same name.
Signals CONCURRENT-MODIFICATION-ERROR (naming REF-PATH) if Git's own
compare-and-swap check fails, and REF-HIERARCHY-CONFLICT-ERROR when
REF-PATH collides with an existing ref along a shared slash-prefix
(the same distinction GIT-UPDATE-REF! makes for a branch name).
Returns SHA on success."
  (let ((args (append (list "update-ref" ref-path sha)
                       (unless (eq expected-sha :unconditional)
                         (list (or expected-sha ""))))))
    (multiple-value-bind (output error-output exit-code) (%git-run repository args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (%signal-ref-update-failure repository ref-path ref-path sha expected-sha error-output))
      sha)))

(defun %git-raw-delete-ref! (repository ref-path)
  "Best-effort, idempotent deletion of REF-PATH in REPOSITORY: does
not signal if REF-PATH does not exist (or has already been deleted
by someone else). Returns T if REF-PATH existed and was deleted, NIL
otherwise. Used only for cleaning up GitHack's own internal
`refs/githack/prepare/...` tracking refs, where no caller ever needs
to distinguish \"already gone\" from \"just deleted\"."
  (multiple-value-bind (output error-output exit-code) (%git-run repository (list "update-ref" "-d" ref-path))
    (declare (ignore output error-output))
    (zerop exit-code)))

(defgeneric %format-git-update-ref-stdin-command (kind stream command)
  (:documentation
   "Write COMMAND's own `git update-ref --stdin` line to STREAM,
dispatching on KIND (COMMAND's own head, :UPDATE or :DELETE) via an
EQL specializer -- the per-command-kind step of %GIT-UPDATE-REF-
STDIN!'s own batch-input assembly."))

(defmethod %format-git-update-ref-stdin-command ((kind (eql :update)) stream command)
  (destructuring-bind (ref new old) (rest command)
    (format stream "update ~A ~A ~A~%" ref new (or old ""))))

(defmethod %format-git-update-ref-stdin-command ((kind (eql :delete)) stream command)
  (destructuring-bind (ref) (rest command)
    (format stream "delete ~A~%" ref)))

(defun %git-update-ref-stdin! (repository commands)
  "Execute COMMANDS -- a list of (:UPDATE REF NEW-SHA OLD-SHA) or
(:DELETE REF) entries, OLD-SHA being NIL to require REF not already
exist -- as a single atomic batch of Git ref updates against
REPOSITORY, via `git update-ref --stdin`. Per Git's own documented
behaviour, every command in one --stdin invocation is applied as a
single atomic transaction: if any one update's compare-and-swap
check fails, Git performs NONE of the updates in COMMANDS at all.
Returns T on success. Signals REF-HIERARCHY-CONFLICT-ERROR when an
:UPDATE in COMMANDS collides with an existing ref along a shared
slash-prefix, and CONCURRENT-MODIFICATION-ERROR for a
compare-and-swap miss."
  (let ((input (with-output-to-string (s)
                 (dolist (command commands)
                   (%format-git-update-ref-stdin-command (first command) s command)))))
    (multiple-value-bind (output error-output exit-code) (%git-run repository (list "update-ref" "--stdin") :input input)
      (declare (ignore output))
      (unless (zerop exit-code)
        (%signal-ref-batch-update-failure repository commands error-output))
      t)))

(defun %git-mktag (repository content)
  "Shell out to `git mktag` against REPOSITORY, feeding it CONTENT
\(the exact plain-text payload of a Git annotated-tag object -- see
FORMAT-ANNOTATED-TAG-CONTENT) on its standard input, and return the
resulting 40-character hexadecimal tag SHA. Unlike a bare
`hash-object -t tag -w`, `mktag` runs a strict `git fsck` check
first and refuses to write anything malformed (e.g. missing a
\"tagger\" header) -- so a bug in CONTENT's own construction fails
loudly, here, rather than corrupting a later step. Signals
DISTRIBUTED-TRANSACTION-ERROR if `git mktag` rejects CONTENT."
  (multiple-value-bind (output error-output exit-code) (%git-run repository (list "mktag") :input content)
    (unless (zerop exit-code)
      (error 'distributed-transaction-error
             :format-control "`git mktag` rejected a Two-Phase-Commit tag in ~A: ~A"
             :format-arguments (list repository error-output)))
    (string-trim '(#\Space #\Newline #\Return) output)))

(defun split-lines (string)
  "Return a list of STRING's own lines, split on #\\Newline, with no
trailing empty line for a STRING that itself ends in a newline."
  (let ((length (length string)))
    (let next ((start 0))
      (if (>= start length)
          '()
          (let ((newline (position #\Newline string :start start)))
            (cons (subseq string start (or newline length))
                  (next (if newline (1+ newline) length))))))))

(defun %git-for-each-ref (repository pattern)
  "Shell out to `git for-each-ref --format=... PATTERN` against
REPOSITORY and return a list of (SHA OBJECT-TYPE REFNAME) for every
ref PATTERN (e.g. \"refs/githack/prepare/\") matches. Returns the
empty list if PATTERN matches nothing (or REPOSITORY has no refs at
all yet)."
  (multiple-value-bind (output error-output exit-code)
      (%git-run repository (list "for-each-ref" "--format=%(objectname) %(objecttype) %(refname)" pattern))
    (declare (ignore error-output))
    (if (zerop exit-code)
        (mapcar (lambda (line)
                  (let* ((sp1 (position #\Space line))
                         (sp2 (position #\Space line :start (1+ sp1))))
                    (list (subseq line 0 sp1) (subseq line (1+ sp1) sp2) (subseq line (1+ sp2)))))
                (remove nil (split-lines output)
                        :key (lambda (line) (zerop (length line)))
                        :test-not #'eq))
        '())))

(defun %git-rev-parse (repository rev-expr)
  "Shell out to `git rev-parse --verify --quiet REV-EXPR` against
REPOSITORY and return the resulting SHA, or NIL if REV-EXPR cannot
be resolved (e.g. peeling a tag via \"<ref>^{commit}\" that does not
in fact point at a commit, directly or transitively)."
  (multiple-value-bind (output error-output exit-code)
      (%git-run repository (list "rev-parse" "--verify" "--quiet" rev-expr))
    (declare (ignore error-output))
    (and (zerop exit-code) (string-trim '(#\Space #\Newline #\Return) output))))

;;; ------------------------------------------------------------------
;;; Transaction Manifests and annotated-tag content.
;;; ------------------------------------------------------------------

(defun %encode-ref-path-segment (string)
  "Return STRING as one Git ref path segment. #\\% becomes \"%25\"
and #\\/ becomes \"%2F\"; every other character is copied. Used so a
branch name is a single segment of a prepare ref. The raw name is
stored only in the transaction manifest."
  (let ((n (length string)))
    (with-output-to-string (out)
      (let next ((i 0))
        (when (< i n)
          (let ((char (char string i)))
            (cond
              ((char= char #\%) (write-string "%25" out))
              ((char= char #\/) (write-string "%2F" out))
              (t (write-char char out))))
          (next (1+ i)))))))

(defun %decode-ref-path-segment (string)
  "Inverse of %ENCODE-REF-PATH-SEGMENT. A #\\% followed by two
hexadecimal digits is one character. Anything else is copied."
  (let ((n (length string)))
    (with-output-to-string (out)
      (let next ((i 0))
        (when (< i n)
          (if (and (char= (char string i) #\%)
                   (<= (+ i 3) n)
                   (digit-char-p (char string (+ i 1)) 16)
                   (digit-char-p (char string (+ i 2)) 16))
              (progn
                (write-char (code-char (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)) out)
                (next (+ i 3)))
              (progn
                (write-char (char string i) out)
                (next (1+ i)))))))))

(defun prepare-ref-path (tx-id branch-name)
  "Return the Git ref path GitHack's own Two-Phase-Commit Phase 1
uses to track TX-ID's prepared-but-uncommitted write against
BRANCH-NAME. BRANCH-NAME is one percent-encoded path segment
(%ENCODE-REF-PATH-SEGMENT): a slash is \"%2F\", not another
directory. The raw branch name is stored only in the transaction
manifest. The path is
\"refs/githack/prepare/<tx-id>/<encoded-branch>\"."
  (format nil "refs/githack/prepare/~A/~A" tx-id (%encode-ref-path-segment branch-name)))

(defun ledger-ref-path (tx-id)
  "Return the Git ref path GitHack's own Two-Phase-Commit Phase 2
uses as TX-ID's Point of No Return marker in its elected Ledger
repository: \"refs/githack/ledger/<tx-id>\"."
  (format nil "refs/githack/ledger/~A" tx-id))

(defun build-transaction-manifest (tx-id pending-writes ledger-git-repository)
  "Return TX-ID's own Transaction Manifest: an s-expression plist
recording TX-ID itself, the elected Ledger repository's own
pathname, and, for every participant in PENDING-WRITES (a list of
PENDING-WRITE), its own repository pathname, branch name, and
branch OLD-SHA (the CAS baseline every participant's own Phase 2/
crash-recovery roll-forward is checked against). Every path is
recorded via UIOP:NATIVE-NAMESTRING, so RUN-GITHACK-EXORCIST! can
later reconstitute a real pathname via UIOP:PARSE-NATIVE-NAMESTRING."
  (list :tx-id tx-id
        :ledger (uiop:native-namestring (get-pathname ledger-git-repository))
        :participants
        (mapcar (lambda (pw)
                  (list :repository (uiop:native-namestring (get-pathname (pending-write/git-repository pw)))
                        :branch (pending-write/branch-name pw)
                        :old-sha (pending-write/old-sha pw)))
                pending-writes)))

(defun format-transaction-manifest (manifest)
  "Return MANIFEST (a plist, as built by %BUILD-TRANSACTION-
MANIFEST) printed as a READable Lisp s-expression string, suitable
for use as a Git annotated tag's own message."
  (let ((*print-pretty* nil) (*print-readably* nil) (*print-circle* nil))
    (prin1-to-string manifest)))

(defun parse-transaction-manifest (text)
  "Inverse of FORMAT-TRANSACTION-MANIFEST: READ TEXT back into its
original Transaction Manifest plist. *READ-EVAL* is bound to NIL for
the duration, since TEXT ultimately comes from a Git object's own
stored content, which this process should never blindly EVAL."
  (let ((*read-eval* nil))
    (read-from-string text)))

(defun sanitize-tag-name-component (string)
  "Return STRING with every #\\/ replaced by #\\-, so it is safe to
embed in a Git tag object's own \"tag <name>\" header line (which,
unlike a ref path, is not expected to contain path separators)."
  (substitute #\- #\/ string))

(defun format-annotated-tag-content (commit-sha tag-name tagger-signature manifest-text)
  "Return the exact plain-text payload of a Git annotated-tag object
targeting COMMIT-SHA (a commit): an \"object\"/\"type\"/\"tag\"/
\"tagger\" header, a blank line, and finally MANIFEST-TEXT as the
tag's own message -- suitable input for %GIT-MKTAG."
  (format nil "object ~A~%type commit~%tag ~A~%tagger ~A ~D ~A~%~%~A~%"
          commit-sha tag-name tagger-signature (unix-time-now) +default-commit-timezone-offset+ manifest-text))

;;; ------------------------------------------------------------------
;;; Branch ref lock. Git's update-ref creates `refs/heads/<branch>.lock`
;;; with O_EXCL and refuses to move the branch while that file exists.
;;; Prepare creates it, writes the prepared SHA, and leaves it on
;;; disk through the ledger write. Publishing renames it onto the
;;; branch ref. Rollback deletes it.
;;; ------------------------------------------------------------------

(defun %git-dir-pathname (repository)
  "Return REPOSITORY as a directory pathname, so merging a relative
ref path onto it keeps every component of the Git directory."
  (uiop:ensure-directory-pathname repository))

(defun %branch-ref-pathname (repository branch-name)
  "Return the loose-ref pathname of REPOSITORY's branch BRANCH-NAME."
  (merge-pathnames (uiop:parse-unix-namestring (format nil "refs/heads/~A" branch-name))
                   (%git-dir-pathname repository)))

(defun %branch-ref-lock-pathname (repository branch-name)
  "Return Git's own lock pathname for REPOSITORY's branch BRANCH-NAME:
`refs/heads/<branch>.lock`."
  (merge-pathnames (uiop:parse-unix-namestring (format nil "refs/heads/~A.lock" branch-name))
                   (%git-dir-pathname repository)))

(defun %write-lock-sha (lock-pathname sha)
  "Write SHA and a single linefeed into LOCK-PATHNAME. The bytes are
written explicitly so a Windows stream cannot turn the linefeed
into a carriage return."
  (with-open-file (out lock-pathname :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :overwrite :if-does-not-exist :error)
    (write-sequence (sb-ext:string-to-octets (format nil "~A~%" sha) :external-format :ascii) out)
    (finish-output out)))

(defun %atomic-replace-file (source target)
  "Replace TARGET with SOURCE. SOURCE is consumed. On Windows this is
MoveFileEx with MOVEFILE_REPLACE_EXISTING and MOVEFILE_WRITE_THROUGH,
so a crash cannot leave the branch ref missing."
  #+os-windows
  (let* ((src (substitute #\/ #\\ (uiop:native-namestring source)))
         (dst (substitute #\/ #\\ (uiop:native-namestring target)))
         (script (format nil "Add-Type -Namespace GithackAtomic -Name Move -MemberDefinition '[DllImport(\"kernel32.dll\", SetLastError=true, CharSet=CharSet.Unicode)] public static extern bool MoveFileEx(string existing, string neu, int flags);'; if (-not [GithackAtomic.Move]::MoveFileEx('~A','~A', 9)) { exit 1 }"
                         src dst)))
    (multiple-value-bind (output error-output code)
        (uiop:run-program (list "powershell" "-NoProfile" "-NonInteractive" "-Command" script)
                          :output :string :error-output :string :ignore-error-status t)
      (unless (zerop code)
        (error 'distributed-transaction-error
               :format-control "Could not publish ~A onto ~A (~D): ~A~A"
               :format-arguments (list source target code output error-output)))))
  #-os-windows
  (uiop:rename-file-overwriting-target source target))

(defun %release-branch-ref-lock! (repository branch-name)
  "Delete REPOSITORY's branch lock for BRANCH-NAME if it exists.
Used when a Prepare is rolled back, or when the branch ref already
holds the prepared commit and the lock file is only a leftover."
  (ignore-errors (delete-file (%branch-ref-lock-pathname repository branch-name)))
  nil)

(defun %acquire-branch-ref-lock! (repository branch-name old-sha new-sha)
  "Create Git's `refs/heads/<branch>.lock` for BRANCH-NAME and write
NEW-SHA into it. After the file exists, the branch must still be at
OLD-SHA (NIL when the branch does not exist yet); otherwise the lock
is removed and CONCURRENT-MODIFICATION-ERROR is signalled. Polls
while some other update holds the lock, then signals
TRANSACTION-LOCK-TIMEOUT-ERROR."
  (let ((lock (%branch-ref-lock-pathname repository branch-name))
        (deadline (+ (get-internal-real-time)
                     (round (* +transaction-lock-timeout+ internal-time-units-per-second)))))
    (ensure-directories-exist lock)
    (let next ()
      (let ((stream (open lock :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists nil :if-does-not-exist :create)))
        (if (null stream)
            (progn
              (when (> (get-internal-real-time) deadline)
                (error 'transaction-lock-timeout-error :pathname lock))
              (sleep +transaction-lock-poll-interval+)
              (next))
            (handler-case
                (progn
                  (let ((current (git-show-ref-sha repository branch-name)))
                    (unless (equal current old-sha)
                      (error 'concurrent-modification-error
                             :repository repository :name branch-name
                             :expected-sha old-sha :new-sha new-sha)))
                  (write-sequence (sb-ext:string-to-octets (format nil "~A~%" new-sha) :external-format :ascii)
                                  stream)
                  (finish-output stream)
                  (close stream))
              (error (condition)
                (ignore-errors (close stream))
                (%release-branch-ref-lock! repository branch-name)
                (error condition))))))))

(defun %publish-locked-branch-ref! (repository branch-name new-sha)
  "Make BRANCH-NAME point at NEW-SHA by replacing the loose ref with
the lock file. If the branch already points at NEW-SHA, only a
leftover lock file is removed."
  (if (equal (git-show-ref-sha repository branch-name) new-sha)
      (%release-branch-ref-lock! repository branch-name)
      (let ((lock (%branch-ref-lock-pathname repository branch-name)))
        (unless (probe-file lock)
          (ensure-directories-exist lock)
          (with-open-file (out lock :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists nil :if-does-not-exist :create)
            (unless out
              (error 'distributed-transaction-error
                     :format-control "Branch ~A in ~A moved while its lock file was gone."
                     :format-arguments (list branch-name repository)))
            (write-sequence (sb-ext:string-to-octets (format nil "~A~%" new-sha) :external-format :ascii) out)
            (finish-output out)
            (close out)))
        (%write-lock-sha lock new-sha)
        (%atomic-replace-file lock (%branch-ref-pathname repository branch-name)))))

;;; ------------------------------------------------------------------
;;; Phase 1 (PREPARE) and its rollback.
;;; ------------------------------------------------------------------

(defun %prepare-participant! (pw tx-id manifest-text)
  "Perform Phase 1 (\"Prepare\") for PW (a PENDING-WRITE): create an
annotated tag (via %GIT-MKTAG) targeting PW's own already-persisted
NEW-COMMIT-SHA, with MANIFEST-TEXT as its message, then point
`refs/githack/prepare/<tx-id>/<encoded-branch>` at it. The branch
segment is %ENCODE-REF-PATH-SEGMENT of the raw branch name (`/` is
`%2F`), not the raw name. The raw name is only in MANIFEST-TEXT.
The tag object's own \"tag\" header is SANITIZE-TAG-NAME-COMPONENT
of that name (`/` becomes `-`). That header string is not an
identity: \"feature/foo\" and \"feature-foo\" sanitize to the same
header, and nothing looks a participant up by it. The ref must not
already exist -- a collision would mean TX-ID was somehow reused,
which GENERATE-TRANSACTION-ID's 128 bits of randomness makes
astronomically unlikely. Records the new prepare ref's path in PW's
own PREPARE-REF slot. Returns PW."
  (let* ((git-repository (pending-write/git-repository pw))
         (repository (get-pathname git-repository))
         (branch-name (pending-write/branch-name pw))
         (tag-name (format nil "githack-prepare-~A-~A" tx-id (sanitize-tag-name-component branch-name)))
         (tagger (or (get-committer git-repository) (get-author git-repository) "GitHack 2PC <githack@localhost>"))
         (tag-content (format-annotated-tag-content (pending-write/new-commit-sha pw) tag-name tagger manifest-text))
         (tag-sha (%git-mktag repository tag-content))
         (ref-path (prepare-ref-path tx-id branch-name)))
    (%acquire-branch-ref-lock! repository branch-name
                               (pending-write/old-sha pw) (pending-write/new-commit-sha pw))
    (handler-case
        (progn
          (%git-raw-update-ref! repository ref-path tag-sha :expected-sha nil)
          (setf (pending-write/prepare-ref pw) ref-path)
          pw)
      (error (condition)
        (%release-branch-ref-lock! repository branch-name)
        (error condition)))))

(defun %rollback-participant-prepare! (pw)
  "Undo %PREPARE-PARTICIPANT!'s effect on PW (a PENDING-WRITE) that
already succeeded: best-effort delete its own prepare ref (if any)
and its branch lock, leaving its already-persisted commit object as
harmless, unreachable Git garbage. Used only when some LATER
participant's own Phase 1 step fails, and by the Exorcist when the
ledger ref was never written. The branch lock has to go too, or the
branch stays frozen for every later update-ref."
  (let ((repository (get-pathname (pending-write/git-repository pw))))
    (when (pending-write/prepare-ref pw)
      (%git-raw-delete-ref! repository (pending-write/prepare-ref pw))
      (setf (pending-write/prepare-ref pw) nil))
    (%release-branch-ref-lock! repository (pending-write/branch-name pw))))

;;; ------------------------------------------------------------------
;;; Phase 2 (POINT OF NO RETURN & ROLL FORWARD).
;;; ------------------------------------------------------------------

(defun %write-ledger-commit-point! (ledger-git-repository tx-id)
  "Write TX-ID's own Point of No Return: a fresh, otherwise
meaningless Git blob (containing TX-ID itself, purely for a human
inspecting the object with `git cat-file` to have something to look
at) hashed into LEDGER-GIT-REPOSITORY's object database, and
`refs/githack/ledger/<tx-id>` pointed at it (requiring that ref not
already exist). The instant this ref exists, TX-ID's distributed
transaction is permanently committed, no matter what happens to this
Lisp process next -- see RUN-GITHACK-EXORCIST!. Returns the blob's
SHA."
  (let* ((repository (get-pathname ledger-git-repository))
         (blob-sha (git-hash-object repository "blob" (sb-ext:string-to-octets tx-id :external-format :utf-8)))
         (ref-path (ledger-ref-path tx-id)))
    (%git-raw-update-ref! repository ref-path blob-sha :expected-sha nil)
    blob-sha))

(defun %roll-forward-participant! (pw)
  "Publish PW's prepared commit by replacing its branch ref with the
lock file %PREPARE-PARTICIPANT! wrote, then delete PW's prepare ref.
An ordinary update-ref cannot move the branch until this replace
happens, because that lock file is Git's own
`refs/heads/<branch>.lock`."
  (let ((repository (get-pathname (pending-write/git-repository pw))))
    (%publish-locked-branch-ref! repository (pending-write/branch-name pw)
                                 (pending-write/new-commit-sha pw))
    (when (pending-write/prepare-ref pw)
      (%git-raw-delete-ref! repository (pending-write/prepare-ref pw)))))

;;; ------------------------------------------------------------------
;;; Smart-commit dispatch: 0 / 1 / >1 participants.
;;; ------------------------------------------------------------------

(defun %finish-single-repo-write! (pw)
  "The \"Fast Path\": PW (a PENDING-WRITE) is the ONLY participant in
its GITHACK-TRANSACTION, so no 2PC coordination is needed at all --
just advance its branch straight to its own prepared commit, via one
single atomic `git update-ref --stdin` call (rather than plain
GIT-UPDATE-REF!, purely so this path, too, honours the architecture
spec's own requirement to use `update-ref --stdin`; the two are
equally atomic for a single ref)."
  (let ((repository (get-pathname (pending-write/git-repository pw))))
    (%git-update-ref-stdin! repository
                            (list (list :update (format nil "refs/heads/~A" (pending-write/branch-name pw))
                                        (pending-write/new-commit-sha pw) (pending-write/old-sha pw))))))

(defun %finish-two-phase-commit! (tx-id pending-writes)
  "Drive the full distributed Two-Phase-Commit protocol for TX-ID
across PENDING-WRITES (a list of two or more PENDING-WRITE, in the
order their own GIT-TRANSACTIONs first committed): elect the first
as the Ledger, build one shared Transaction Manifest, Prepare every
participant in turn (rolling back and signalling DISTRIBUTED-
TRANSACTION-ERROR if any one Prepare step fails), write the Ledger's
own commit-point ref (the Point of No Return), and finally roll every
participant forward. Returns TX-ID."
  (let* ((ledger-git-repository (pending-write/git-repository (first pending-writes)))
         (manifest-text (format-transaction-manifest
                          (build-transaction-manifest tx-id pending-writes ledger-git-repository)))
         (prepared '()))
    (handler-case
        (dolist (pw pending-writes)
          (%prepare-participant! pw tx-id manifest-text)
          (push pw prepared))
      (error (condition)
        (dolist (pw prepared) (%rollback-participant-prepare! pw))
        (error 'distributed-transaction-error
               :format-control "Distributed transaction ~A: Phase 1 (Prepare) failed; rolled back ~D already-prepared participant(s). Original error: ~A"
               :format-arguments (list tx-id (length prepared) condition))))
    ;; Point of no return: from here on, TX-ID is permanently committed.
    (%write-ledger-commit-point! ledger-git-repository tx-id)
    (dolist (pw pending-writes)
      (%roll-forward-participant! pw))
    tx-id))

(defun %finish-githack-transaction! (txn)
  "Evaluate TXN's (a GITHACK-TRANSACTION) own PENDING-WRITES once its
WITH-GITHACK-TRANSACTION body has returned normally, and finish it
accordingly: no participants is a no-op, exactly one takes the Fast
Path (%FINISH-SINGLE-REPO-WRITE!), and more than one drives the full
2PC protocol (%FINISH-TWO-PHASE-COMMIT!). PENDING-WRITES accumulates
via PUSH, so it is reversed first to restore first-encountered order
(significant only for >1 participants, since the first is elected
the Ledger)."
  (let ((pending (reverse (%githack-transaction/pending-writes txn))))
    (cond
      ((null pending) nil)
      ((null (rest pending)) (%finish-single-repo-write! (first pending)))
      (t (%finish-two-phase-commit! (%githack-transaction/tx-id txn) pending)))))

;;; ------------------------------------------------------------------
;;; Public entry points.
;;; ------------------------------------------------------------------

(defun call-with-githack-transaction (thunk)
  "Invoke THUNK (a function of no arguments) with *CURRENT-
TRANSACTION* dynamically bound to a freshly created GITHACK-
TRANSACTION. See this file's own header comment for the full
protocol; in short: every ordinary, single-repository
CALL-WITH-TRANSACTION/CALL-WITH-GIT-TRANSACTION call THUNK makes
(against a :READ-WRITE repository, using any CONFLICT-RESOLUTION
other than :REBASE, which is not supported inside a distributed
transaction) enlists its own repository/branch as a pending write
instead of committing immediately. If THUNK returns normally, the
resulting pending writes are committed via the 0/1/>1-participant
no-op/Fast-Path/Two-Phase-Commit dispatch (%FINISH-GITHACK-
TRANSACTION!). If THUNK signals an error, nothing further happens --
every already-persisted-but-not-yet-ref-visible participant commit
is simply left as harmless Git garbage. Returns THUNK's own values."
  (let ((txn (%make-githack-transaction (generate-transaction-id))))
    (let ((*current-transaction* txn))
      (multiple-value-prog1
          (funcall thunk)
        (%finish-githack-transaction! txn)))))

(defmacro with-githack-transaction (() &body body)
  "Evaluate BODY with *CURRENT-TRANSACTION* dynamically bound to a
freshly created, distributed (potentially multi-repository)
transaction context. Equivalent to (CALL-WITH-GITHACK-TRANSACTION
(LAMBDA () ,@BODY)); see CALL-WITH-GITHACK-TRANSACTION for the full
protocol."
  `(call-with-githack-transaction (lambda () ,@body)))

(defun githack-transaction-tx-id (transaction)
  "Return TRANSACTION's (a GITHACK-TRANSACTION) own TX-ID string."
  (%githack-transaction/tx-id transaction))

;;; ------------------------------------------------------------------
;;; The Exorcist: crash recovery.
;;; ------------------------------------------------------------------

(defun %manifest-participant (manifest repository branch-name)
  "Return MANIFEST's participant plist for REPOSITORY and BRANCH-NAME,
or NIL. The key is the pair ENLIST-TRANSACTION-WRITE! coalesces on.
The repository half is compared as UIOP:NATIVE-NAMESTRING under
EQUAL, not by EQ of pathname objects; the branch half is STRING=.
Enlist keeps at most one pending write per pair (a second top-level
write to the same repository and branch replaces the new commit SHA
and preserves the original old SHA), so the match is unique. Branch
name alone is not a key."
  (let ((repository-key (uiop:native-namestring repository)))
    (find-if (lambda (participant)
               (and (equal repository-key (getf participant :repository))
                    (string= branch-name (getf participant :branch))))
             (getf manifest :participants))))

(defun %exorcise-stranded-ref! (repository tag-sha ref-path)
  "Resolve one stranded `refs/githack/prepare/<tx-id>/<branch-name>`
ref (REF-PATH, whose annotated tag object's SHA is TAG-SHA) found in
REPOSITORY: parse its own Manifest, ask its Ledger whether it was
ever permanently committed, and either roll it forward or delete it.
The roll-forward compare-and-swap baseline is that participant's own
old SHA, looked up by (%MANIFEST-PARTICIPANT MANIFEST REPOSITORY
BRANCH-NAME). There is at most one manifest entry per (repository,
branch), because ENLIST-TRANSACTION-WRITE! coalesces a second write
to the same pair. Do not look the participant up by branch name
alone: every participant on \"main\" would then share the first
entry's old SHA, and the later ones' compare-and-swap would refuse.
Returns (VALUES TX-ID BRANCH-NAME ACTION), ACTION being :COMMITTED or
:ROLLED-BACK. Signals DISTRIBUTED-TRANSACTION-ERROR if REF-PATH's own
tag cannot be read back into a well-formed Manifest, if no
participant matches REPOSITORY and BRANCH-NAME, or if its Ledger
repository cannot itself be reached."
  (let* ((prefix "refs/githack/prepare/")
         (suffix (subseq ref-path (length prefix)))
         (slash (position #\/ suffix))
         (tx-id (subseq suffix 0 slash))
         (branch-name (%decode-ref-path-segment (subseq suffix (1+ slash)))))
    (handler-case
        (let* ((tag-content (sb-ext:octets-to-string (git-cat-file repository tag-sha) :external-format :utf-8))
               (manifest-text (nth-value 1 (split-commit-header-and-message tag-content)))
               (manifest (parse-transaction-manifest manifest-text))
               (ledger-repository (uiop:parse-native-namestring (getf manifest :ledger)))
               (participant (%manifest-participant manifest repository branch-name)))
          (unless participant
            (error 'distributed-transaction-error
                   :format-control "No manifest participant for repository ~A branch ~S."
                   :format-arguments (list repository branch-name)))
          (let ((old-sha (getf participant :old-sha)))
            (if (%git-raw-show-ref ledger-repository (ledger-ref-path tx-id))
                (let* ((target-commit-sha (%git-rev-parse repository (format nil "~A^{commit}" ref-path)))
                       (current (git-show-ref-sha repository branch-name)))
                  (cond
                    ((equal current target-commit-sha)
                     (%release-branch-ref-lock! repository branch-name))
                    ((equal current old-sha)
                     (%publish-locked-branch-ref! repository branch-name target-commit-sha))
                    (t
                     (error 'distributed-transaction-error
                            :format-control "Branch ~A in ~A is at ~A, neither its prepared commit ~A nor its old SHA ~A, so the branch lock was lost before publish."
                            :format-arguments (list branch-name repository current target-commit-sha old-sha))))
                  (%git-raw-delete-ref! repository ref-path)
                  (values tx-id branch-name :committed))
                (progn
                  (%git-raw-delete-ref! repository ref-path)
                  (%release-branch-ref-lock! repository branch-name)
                  (values tx-id branch-name :rolled-back)))))
      (distributed-transaction-error (condition) (error condition))
      (error (condition)
        (error 'distributed-transaction-error
               :format-control "RUN-GITHACK-EXORCIST! could not resolve stranded ref ~S in ~A: ~A"
               :format-arguments (list ref-path repository condition))))))

(defun run-githack-exorcist! (repository)
  "Scan REPOSITORY (a pathname naming a Git directory) for stranded
`refs/githack/prepare/<tx-id>/<branch-name>` refs -- left behind by a
WITH-GITHACK-TRANSACTION whose Lisp process crashed somewhere between
Phase 1 (Prepare) and Phase 2 (Roll Forward) -- and resolve each one
via %EXORCISE-STRANDED-REF!: fast-forward and clean up if its own
Ledger shows it was already permanently committed, or simply clean up
if not. The fast-forward baseline is that repository's own old SHA
for that branch, not the old SHA of whichever manifest entry shares
the branch name. Safe to call on a repository with no stranded refs at all
(returns the empty list); safe to call repeatedly (each ref is
resolved and removed, so a second call finds nothing left to do).
Returns a list of (TX-ID BRANCH-NAME ACTION) for every stranded ref
resolved, in the order found."
  (mapcar (lambda (entry)
            (destructuring-bind (tag-sha object-type ref-path) entry
              (unless (string= object-type "tag")
                (error 'distributed-transaction-error
                       :format-control "RUN-GITHACK-EXORCIST! found a stranded ref ~S in ~A that is not an annotated tag (its object type is ~S) -- GitHack's own Two-Phase-Commit machinery never creates one any other way, so this ref was not created by GitHack."
                       :format-arguments (list ref-path repository object-type)))
              (multiple-value-bind (tx-id branch-name action) (%exorcise-stranded-ref! repository tag-sha ref-path)
                (list tx-id branch-name action))))
          (%git-for-each-ref repository "refs/githack/prepare/")))
