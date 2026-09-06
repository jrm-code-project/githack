;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Transparent, crash-proof distributed (multi-repository) Two-
;;; Phase-Commit (2PC) transactions on top of GitHack's existing,
;;; single-repository GIT-TRANSACTION/TRANSACTION machinery.
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
;;; %COMMIT-GIT-TRANSACTION-NOW (git-transaction.lisp), on noticing
;;; *CURRENT-TRANSACTION* is bound, stops just short of actually
;;; advancing its branch: it instead calls %ENLIST-TRANSACTION-WRITE!,
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
;;; `refs/githack/prepare/<tx-id>/<branch-name>`, at that tag. If any
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
;;; process crashes one line later. Every participant is then rolled
;;; forward: its own real branch ref is advanced to its prepared
;;; commit, and its own prepare ref is deleted, both in one single
;;; atomic `git update-ref --stdin` batch per participant (Git's own
;;; --stdin batches are all-or-nothing: if the branch's own
;;; compare-and-swap check somehow fails at this point, NEITHER
;;; update in that one participant's batch takes effect).
;;;
;;; CRASH RECOVERY ("The Exorcist"): if this Lisp process dies
;;; between Phase 1 and Phase 2 (or partway through Phase 2's own
;;; roll-forward loop), some participants are left with a stranded
;;; `refs/githack/prepare/<tx-id>/...` ref and no corresponding real
;;; branch update. RUN-GITHACK-EXORCIST, run against any single
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
;;; Every one of these operations relies solely on standard Git
;;; plumbing (`hash-object`, `cat-file`, `update-ref`, `update-ref
;;; --stdin`, `mktag`, `rev-parse`, `for-each-ref`) against ordinary
;;; refs and tag objects -- Git's own Merkle tree and refs are the
;;; entire source of truth; no external coordination service of any
;;; kind is used or required.

;;; ------------------------------------------------------------------
;;; Low-level, generic Git ref/tag plumbing, beyond GIT-BRANCH.LISP's
;;; own refs/heads-specific GIT-SHOW-REF-SHA/GIT-UPDATE-REF: arbitrary
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

(defun %git-raw-update-ref (repository ref-path sha &key (expected-sha :unconditional))
  "Like GIT-UPDATE-REF, but against an arbitrary REF-PATH rather
than being hardwired to \"refs/heads/<name>\". EXPECTED-SHA has the
same meaning as GIT-UPDATE-REF's own argument of the same name.
Signals CONCURRENT-MODIFICATION-ERROR (naming REF-PATH) if Git's own
compare-and-swap check fails. Returns SHA on success."
  (let ((args (append (list "update-ref" ref-path sha)
                       (unless (eq expected-sha :unconditional)
                         (list (or expected-sha ""))))))
    (multiple-value-bind (output error-output exit-code) (%git-run repository args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'concurrent-modification-error
               :repository repository :name ref-path
               :expected-sha (and (not (eq expected-sha :unconditional)) expected-sha)
               :new-sha sha :detail error-output))
      sha)))

(defun %git-raw-delete-ref (repository ref-path)
  "Best-effort, idempotent deletion of REF-PATH in REPOSITORY: does
not signal if REF-PATH does not exist (or has already been deleted
by someone else). Returns T if REF-PATH existed and was deleted, NIL
otherwise. Used only for cleaning up GitHack's own internal
`refs/githack/prepare/...` tracking refs, where no caller ever needs
to distinguish \"already gone\" from \"just deleted\"."
  (multiple-value-bind (output error-output exit-code) (%git-run repository (list "update-ref" "-d" ref-path))
    (declare (ignore output error-output))
    (zerop exit-code)))

(defun %git-update-ref-stdin (repository commands)
  "Execute COMMANDS -- a list of (:UPDATE REF NEW-SHA OLD-SHA) or
(:DELETE REF) entries, OLD-SHA being NIL to require REF not already
exist -- as a single atomic batch of Git ref updates against
REPOSITORY, via `git update-ref --stdin`. Per Git's own documented
behaviour, every command in one --stdin invocation is applied as a
single atomic transaction: if any one update's compare-and-swap
check fails, Git performs NONE of the updates in COMMANDS at all.
Returns T on success; signals CONCURRENT-MODIFICATION-ERROR
otherwise."
  (let ((input (with-output-to-string (s)
                 (dolist (command commands)
                   (ecase (first command)
                     (:update
                      (destructuring-bind (ref new old) (rest command)
                        (format s "update ~A ~A ~A~%" ref new (or old ""))))
                     (:delete
                      (destructuring-bind (ref) (rest command)
                        (format s "delete ~A~%" ref))))))))
    (multiple-value-bind (output error-output exit-code) (%git-run repository (list "update-ref" "--stdin") :input input)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'concurrent-modification-error
               :repository repository :name "(batch `update-ref --stdin`)"
               :expected-sha nil :new-sha nil :detail error-output))
      t)))

(defun %git-mktag (repository content)
  "Shell out to `git mktag` against REPOSITORY, feeding it CONTENT
\(the exact plain-text payload of a Git annotated-tag object -- see
%FORMAT-ANNOTATED-TAG-CONTENT) on its standard input, and return the
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

(defun %split-lines (string)
  "Return a list of STRING's own lines, split on #\\Newline, with no
trailing empty line for a STRING that itself ends in a newline."
  (loop with start = 0
        with length = (length string)
        while (< start length)
        collect (let ((newline (position #\Newline string :start start)))
                  (prog1 (subseq string start (or newline length))
                    (setf start (if newline (1+ newline) length))))))

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
        (loop for line in (%split-lines output)
              unless (zerop (length line))
                collect (let* ((sp1 (position #\Space line))
                               (sp2 (position #\Space line :start (1+ sp1))))
                          (list (subseq line 0 sp1) (subseq line (1+ sp1) sp2) (subseq line (1+ sp2)))))
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

(defun %prepare-ref-path (tx-id branch-name)
  "Return the Git ref path GitHack's own Two-Phase-Commit Phase 1
uses to track TX-ID's own prepared-but-uncommitted write against
BRANCH-NAME: \"refs/githack/prepare/<tx-id>/<branch-name>\"."
  (format nil "refs/githack/prepare/~A/~A" tx-id branch-name))

(defun %ledger-ref-path (tx-id)
  "Return the Git ref path GitHack's own Two-Phase-Commit Phase 2
uses as TX-ID's Point of No Return marker in its elected Ledger
repository: \"refs/githack/ledger/<tx-id>\"."
  (format nil "refs/githack/ledger/~A" tx-id))

(defun %build-transaction-manifest (tx-id pending-writes ledger-git-repository)
  "Return TX-ID's own Transaction Manifest: an s-expression plist
recording TX-ID itself, the elected Ledger repository's own
pathname, and, for every participant in PENDING-WRITES (a list of
PENDING-WRITE), its own repository pathname, branch name, and
branch OLD-SHA (the CAS baseline every participant's own Phase 2/
crash-recovery roll-forward is checked against). Every path is
recorded via UIOP:NATIVE-NAMESTRING, so RUN-GITHACK-EXORCIST can
later reconstitute a real pathname via UIOP:PARSE-NATIVE-NAMESTRING."
  (list :tx-id tx-id
        :ledger (uiop:native-namestring (get-pathname ledger-git-repository))
        :participants
        (mapcar (lambda (pw)
                  (list :repository (uiop:native-namestring (get-pathname (pending-write-git-repository pw)))
                        :branch (pending-write-branch-name pw)
                        :old-sha (pending-write-old-sha pw)))
                pending-writes)))

(defun %format-transaction-manifest (manifest)
  "Return MANIFEST (a plist, as built by %BUILD-TRANSACTION-
MANIFEST) printed as a READable Lisp s-expression string, suitable
for use as a Git annotated tag's own message."
  (let ((*print-pretty* nil) (*print-readably* nil) (*print-circle* nil))
    (prin1-to-string manifest)))

(defun %parse-transaction-manifest (text)
  "Inverse of %FORMAT-TRANSACTION-MANIFEST: READ TEXT back into its
original Transaction Manifest plist. *READ-EVAL* is bound to NIL for
the duration, since TEXT ultimately comes from a Git object's own
stored content, which this process should never blindly EVAL."
  (let ((*read-eval* nil))
    (read-from-string text)))

(defun %sanitize-tag-name-component (string)
  "Return STRING with every #\\/ replaced by #\\-, so it is safe to
embed in a Git tag object's own \"tag <name>\" header line (which,
unlike a ref path, is not expected to contain path separators)."
  (substitute #\- #\/ string))

(defun %format-annotated-tag-content (commit-sha tag-name tagger-signature manifest-text)
  "Return the exact plain-text payload of a Git annotated-tag object
targeting COMMIT-SHA (a commit): an \"object\"/\"type\"/\"tag\"/
\"tagger\" header, a blank line, and finally MANIFEST-TEXT as the
tag's own message -- suitable input for %GIT-MKTAG."
  (format nil "object ~A~%type commit~%tag ~A~%tagger ~A ~D ~A~%~%~A~%"
          commit-sha tag-name tagger-signature (%unix-time-now) +default-commit-timezone-offset+ manifest-text))

;;; ------------------------------------------------------------------
;;; Phase 1 (PREPARE) and its rollback.
;;; ------------------------------------------------------------------

(defun %prepare-participant! (pw tx-id manifest-text)
  "Perform Phase 1 (\"Prepare\") for PW (a PENDING-WRITE): create an
annotated tag (via %GIT-MKTAG) targeting PW's own already-persisted
NEW-COMMIT-SHA, with MANIFEST-TEXT as its message, then point
`refs/githack/prepare/<tx-id>/<branch-name>` at it (requiring that
ref not already exist -- a collision would mean TX-ID was somehow
reused, which %GENERATE-TRANSACTION-ID's 128 bits of randomness makes
astronomically unlikely). Records the new prepare ref's path in PW's
own PREPARE-REF slot. Returns PW."
  (let* ((git-repository (pending-write-git-repository pw))
         (repository (get-pathname git-repository))
         (branch-name (pending-write-branch-name pw))
         (tag-name (format nil "githack-prepare-~A-~A" tx-id (%sanitize-tag-name-component branch-name)))
         (tagger (or (get-committer git-repository) (get-author git-repository) "GitHack 2PC <githack@localhost>"))
         (tag-content (%format-annotated-tag-content (pending-write-new-commit-sha pw) tag-name tagger manifest-text))
         (tag-sha (%git-mktag repository tag-content))
         (ref-path (%prepare-ref-path tx-id branch-name)))
    (%git-raw-update-ref repository ref-path tag-sha :expected-sha nil)
    (setf (pending-write-prepare-ref pw) ref-path)
    pw))

(defun %rollback-participant-prepare! (pw)
  "Undo %PREPARE-PARTICIPANT!'s effect on PW (a PENDING-WRITE) that
already succeeded: best-effort delete its own prepare ref (if any),
leaving its already-persisted commit object as harmless, unreachable
Git garbage. Used only when some LATER participant's own Phase 1
step fails, to avoid leaving earlier participants' prepare refs
stranded for RUN-GITHACK-EXORCIST to have to clean up later."
  (when (pending-write-prepare-ref pw)
    (%git-raw-delete-ref (get-pathname (pending-write-git-repository pw)) (pending-write-prepare-ref pw))
    (setf (pending-write-prepare-ref pw) nil)))

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
Lisp process next -- see RUN-GITHACK-EXORCIST. Returns the blob's
SHA."
  (let* ((repository (get-pathname ledger-git-repository))
         (blob-sha (git-hash-object repository "blob" (sb-ext:string-to-octets tx-id :external-format :utf-8)))
         (ref-path (%ledger-ref-path tx-id)))
    (%git-raw-update-ref repository ref-path blob-sha :expected-sha nil)
    blob-sha))

(defun %roll-forward-participant! (pw)
  "Perform Phase 2's own per-participant \"Roll Forward\" for PW (a
PENDING-WRITE) whose Phase 1 Prepare step already succeeded: advance
its real `refs/heads/<branch-name>` to its own prepared NEW-COMMIT-
SHA (checked against OLD-SHA, exactly the same compare-and-swap
baseline an ordinary, non-distributed commit would have used) and
delete its own prepare ref, both via one single atomic
`git update-ref --stdin` batch, so no external observer can ever see
PW's branch newly advanced while its prepare ref still lingers, or
vice versa."
  (let ((repository (get-pathname (pending-write-git-repository pw))))
    (%git-update-ref-stdin repository
                            (list (list :update (format nil "refs/heads/~A" (pending-write-branch-name pw))
                                        (pending-write-new-commit-sha pw) (pending-write-old-sha pw))
                                  (list :delete (pending-write-prepare-ref pw))))))

;;; ------------------------------------------------------------------
;;; Smart-commit dispatch: 0 / 1 / >1 participants.
;;; ------------------------------------------------------------------

(defun %finish-single-repo-write! (pw)
  "The \"Fast Path\": PW (a PENDING-WRITE) is the ONLY participant in
its GITHACK-TRANSACTION, so no 2PC coordination is needed at all --
just advance its branch straight to its own prepared commit, via one
single atomic `git update-ref --stdin` call (rather than plain
GIT-UPDATE-REF, purely so this path, too, honours the architecture
spec's own requirement to use `update-ref --stdin`; the two are
equally atomic for a single ref)."
  (let ((repository (get-pathname (pending-write-git-repository pw))))
    (%git-update-ref-stdin repository
                            (list (list :update (format nil "refs/heads/~A" (pending-write-branch-name pw))
                                        (pending-write-new-commit-sha pw) (pending-write-old-sha pw))))))

(defun %finish-two-phase-commit! (tx-id pending-writes)
  "Drive the full distributed Two-Phase-Commit protocol for TX-ID
across PENDING-WRITES (a list of two or more PENDING-WRITE, in the
order their own GIT-TRANSACTIONs first committed): elect the first
as the Ledger, build one shared Transaction Manifest, Prepare every
participant in turn (rolling back and signalling DISTRIBUTED-
TRANSACTION-ERROR if any one Prepare step fails), write the Ledger's
own commit-point ref (the Point of No Return), and finally roll every
participant forward. Returns TX-ID."
  (let* ((ledger-git-repository (pending-write-git-repository (first pending-writes)))
         (manifest-text (%format-transaction-manifest
                          (%build-transaction-manifest tx-id pending-writes ledger-git-repository)))
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
  (let ((pending (reverse (%githack-transaction-pending-writes txn))))
    (cond
      ((null pending) nil)
      ((null (rest pending)) (%finish-single-repo-write! (first pending)))
      (t (%finish-two-phase-commit! (%githack-transaction-tx-id txn) pending)))))

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
  (let ((txn (%make-githack-transaction (%generate-transaction-id))))
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
  (%githack-transaction-tx-id transaction))

;;; ------------------------------------------------------------------
;;; The Exorcist: crash recovery.
;;; ------------------------------------------------------------------

(defun %exorcise-stranded-ref! (repository tag-sha ref-path)
  "Resolve one stranded `refs/githack/prepare/<tx-id>/<branch-name>`
ref (REF-PATH, whose annotated tag object's SHA is TAG-SHA) found in
REPOSITORY: parse its own Manifest, ask its Ledger whether it was
ever permanently committed, and either roll it forward or delete it.
Returns (VALUES TX-ID BRANCH-NAME ACTION), ACTION being :COMMITTED or
:ROLLED-BACK. Signals DISTRIBUTED-TRANSACTION-ERROR if REF-PATH's own
tag cannot be read back into a well-formed Manifest, or if its
Ledger repository cannot itself be reached."
  (let* ((prefix "refs/githack/prepare/")
         (suffix (subseq ref-path (length prefix)))
         (slash (position #\/ suffix))
         (tx-id (subseq suffix 0 slash))
         (branch-name (subseq suffix (1+ slash))))
    (handler-case
        (let* ((tag-content (sb-ext:octets-to-string (git-cat-file repository tag-sha) :external-format :utf-8))
               (manifest-text (nth-value 1 (%split-commit-header-and-message tag-content)))
               (manifest (%parse-transaction-manifest manifest-text))
               (ledger-repository (uiop:parse-native-namestring (getf manifest :ledger)))
               (participant (find branch-name (getf manifest :participants)
                                   :key (lambda (p) (getf p :branch)) :test #'string=))
               (old-sha (and participant (getf participant :old-sha))))
          (if (%git-raw-show-ref ledger-repository (%ledger-ref-path tx-id))
              (let ((target-commit-sha (%git-rev-parse repository (format nil "~A^{commit}" ref-path))))
                (%git-update-ref-stdin repository
                                       (list (list :update (format nil "refs/heads/~A" branch-name)
                                                   target-commit-sha old-sha)
                                             (list :delete ref-path)))
                (values tx-id branch-name :committed))
              (progn
                (%git-raw-delete-ref repository ref-path)
                (values tx-id branch-name :rolled-back))))
      (distributed-transaction-error (condition) (error condition))
      (error (condition)
        (error 'distributed-transaction-error
               :format-control "RUN-GITHACK-EXORCIST could not resolve stranded ref ~S in ~A: ~A"
               :format-arguments (list ref-path repository condition))))))

(defun run-githack-exorcist (repository)
  "Scan REPOSITORY (a pathname naming a Git directory) for stranded
`refs/githack/prepare/<tx-id>/<branch-name>` refs -- left behind by a
WITH-GITHACK-TRANSACTION whose Lisp process crashed somewhere between
Phase 1 (Prepare) and Phase 2 (Roll Forward) -- and resolve each one
via %EXORCISE-STRANDED-REF!: fast-forward and clean up if its own
Ledger shows it was already permanently committed, or simply clean up
if not. Safe to call on a repository with no stranded refs at all
(returns the empty list); safe to call repeatedly (each ref is
resolved and removed, so a second call finds nothing left to do).
Returns a list of (TX-ID BRANCH-NAME ACTION) for every stranded ref
resolved, in the order found."
  (loop for (tag-sha object-type ref-path) in (%git-for-each-ref repository "refs/githack/prepare/")
        unless (string= object-type "tag")
          do (error 'distributed-transaction-error
                     :format-control "RUN-GITHACK-EXORCIST found a stranded ref ~S in ~A that is not an annotated tag (its object type is ~S) -- GitHack's own Two-Phase-Commit machinery never creates one any other way, so this ref was not created by GitHack."
                     :format-arguments (list ref-path repository object-type))
        collect (multiple-value-bind (tx-id branch-name action) (%exorcise-stranded-ref! repository tag-sha ref-path)
                  (list tx-id branch-name action))))
