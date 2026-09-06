;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; GIT-TRANSACTION implements a strict transaction boundary atop
;;; the GIT-OBJECT proxy layer (GIT-BLOB / GIT-TREE / GIT-COMMIT) and
;;; GIT-BRANCH: CALL-WITH-GIT-TRANSACTION resolves a branch to its
;;; current head commit, invokes a user RECEIVER with a transient
;;; GIT-TRANSACTION and that head commit, and -- for a :READ-WRITE
;;; transaction whose RECEIVER returns a root GIT-OBJECT normally --
;;; flushes that root (and any of its unpersisted children, wrapping
;;; it first in an ATOMIC-WRAPPER-TREE via WRAP-ATOMIC-COMMIT-ROOT if
;;; it is a bare atom rather than a GIT-TREE), a new GIT-COMMIT built
;;; from the transaction's cascaded author/committer/message and
;;; parents, and finally advances the branch to point at it.
;;;
;;; If RECEIVER signals an error or otherwise exits abnormally,
;;; nothing is written: the transient Lisp objects are simply
;;; garbage, and the Git repository on disk is untouched. This is
;;; the transaction's automatic rollback.
;;;
;;; NOTE: this is deliberately named GIT-TRANSACTION, with entry
;;; points CALL-WITH-GIT-TRANSACTION / COMMIT-GIT-TRANSACTION /
;;; ABORT-GIT-TRANSACTION, rather than TRANSACTION /
;;; CALL-WITH-TRANSACTION / TRANSACTION/COMMIT / TRANSACTION/ABORT,
;;; because those names already belong to an older, unrelated
;;; prototype transaction system in githack.lisp (predating the
;;; GIT-OBJECT proxy layer) that is still exercised by tests.lisp.

;;; CONCURRENCY POLICY: multiple GIT-TRANSACTIONs against the *same*
;;; repository, whether from separate threads in this Lisp image or
;;; from entirely separate OS processes, are supported at the
;;; Git-ref level -- see CALL-WITH-GIT-TRANSACTION's
;;; CONFLICT-RESOLUTION argument (:ERROR / :RETRY / :LOCK / :REBASE),
;;; which
;;; governs how a branch's compare-and-swap ("Lost Update") race is
;;; detected and handled.
;;;
;;; What is NOT safe is sharing a single in-memory GIT-OBJECT/
;;; PERSISTENT-OBJECT proxy instance (or anything reachable from
;;; one, e.g. via a GIT-BRANCH's TARGET) across multiple threads.
;;; Every lazy-load path in this codebase -- %ENSURE-TREE-ENTRIES-
;;; LOADED, %ENSURE-BLOB-LOADED, and %ENSURE-COMMIT-LOADED in
;;; atomic-wrapper.lisp; PERSISTENT-VECTOR-REF's and PERSISTENT-
;;; ARRAY-REF's element caches in persistent-vector.lisp/persistent-
;;; array.lisp -- mutates a proxy's own slots with no locking
;;; whatsoever. Two threads racing to lazily load the *same* proxy
;;; instance can, at best, harmlessly redo idempotent work (re-fetch
;;; and re-decode the same Git object twice), or, at worst, observe
;;; or produce a partially-populated object if one thread reads a
;;; slot the other is mid-way through setting. Nothing in this
;;; codebase today spawns worker threads internally, so this is a
;;; theoretical risk rather than an observed bug, but any caller
;;; introducing concurrency of their own MUST ensure each thread
;;; either uses entirely separate proxy object graphs (e.g. by
;;; opening its own GIT-TRANSACTION and letting it re-resolve BRANCH
;;; from scratch, rather than sharing one already-resolved
;;; GIT-COMMIT/GIT-TRANSACTION across threads) or otherwise
;;; externally serializes all access to any proxy object it shares.

;;; *GIT-TRANSACTION* is the GIT-TRANSACTION currently in dynamic
;;; scope, bound by CALL-WITH-GIT-TRANSACTION around its call to
;;; RECEIVER. It is unbound at the top level -- referencing it
;;; outside the dynamic extent of a CALL-WITH-GIT-TRANSACTION call
;;; signals UNBOUND-VARIABLE.
(defvar *git-transaction*)

;; DEFVAR's own DOCUMENTATION argument cannot be supplied without
;; also supplying an INITIAL-VALUE (which would leave
;; *GIT-TRANSACTION* bound instead of unbound by default), so its
;; docstring is attached separately here via (SETF DOCUMENTATION).
(setf (documentation '*git-transaction* 'variable)
      "The GIT-TRANSACTION currently in dynamic scope, dynamically
bound by CALL-WITH-GIT-TRANSACTION to the GIT-TRANSACTION it
constructs, for the duration of its call to RECEIVER. Unbound at the
top level -- referencing it outside the dynamic extent of a
CALL-WITH-GIT-TRANSACTION call signals UNBOUND-VARIABLE.")

(defclass git-transaction ()
  ((git-repository
    :initarg :git-repository
    :reader get-git-repository
    :documentation "The GIT-REPOSITORY this transaction was opened against.")
   (mode
    :initarg :mode
    :reader get-mode
    :type (member :read-only :read-write)
    :documentation "Either :READ-ONLY or :READ-WRITE, as passed to CALL-WITH-GIT-TRANSACTION.")
   (target-branch
    :initarg :target-branch
    :reader get-target-branch
    :documentation "The GIT-BRANCH this transaction will advance on a successful :READ-WRITE commit.")
   (author
    :initarg :author
    :reader get-author
    :documentation "This transaction's cascaded commit author signature.")
   (committer
    :initarg :committer
    :reader get-committer
    :documentation "This transaction's cascaded commit committer signature.")
   (message
    :initarg :message
    :reader get-message
    :documentation "This transaction's cascaded commit message.")
   (parents
    :initarg :parents
    :reader get-parents
    :documentation "The list of GIT-COMMIT proxies the new commit will record as its parents.")
   (conflict-resolution
    :initarg :conflict-resolution
    :reader get-conflict-resolution
    :initform :error
    :type (member :error :retry :lock :rebase)
    :documentation
    "One of :ERROR, :RETRY, :LOCK, or :REBASE, as passed to
CALL-WITH-GIT-TRANSACTION, controlling how a 'Lost Update' conflict
-- some other writer having already advanced TARGET-BRANCH between
this transaction's own read and its commit -- is resolved. See
REBASE-FALLBACK for what :REBASE itself falls back to on a genuine,
unresolvable content conflict.")
   (rebase-fallback
    :initarg :rebase-fallback
    :reader get-rebase-fallback
    :initform :error
    :type (member :error :retry)
    :documentation
    "Only consulted when CONFLICT-RESOLUTION is :REBASE. One of
:ERROR or :RETRY, controlling what happens if GIT-MERGE-TREE ever
reports a genuine, unresolvable content conflict while replaying
this transaction's own candidate commit onto a concurrently-advanced
branch HEAD (rather than some merely-detected-but-still-mergeable
race): :ERROR (the default) signals MERGE-CONFLICT-ERROR directly;
:RETRY instead signals CONCURRENT-MODIFICATION-ERROR, discarding
this transaction's own computation entirely and letting :REBASE's
own outer retry loop (see CALL-WITH-GIT-TRANSACTION) re-run RECEIVER
from scratch against the branch's latest HEAD.")
   (expected-branch-sha
    :initarg :expected-branch-sha
    :reader get-expected-branch-sha
    :documentation
    "The SHA TARGET-BRANCH was observed to point at when this
transaction was opened, or NIL if the branch did not exist yet.
Passed through to UPDATE-BRANCH's own EXPECTED-SHA compare-and-swap
argument at commit time, so that a concurrent writer which already
advanced (or created) the branch out from under this transaction is
detected -- via CONCURRENT-MODIFICATION-ERROR -- instead of silently
overwritten.")
   (status
    :initarg :status
    :initform :active
    :accessor get-status
    :type (member :active :committed :aborted)
    :documentation "One of :ACTIVE, :COMMITTED, or :ABORTED.")
   (result
    :initarg :result
    :initform nil
    :accessor get-result
    :documentation
    "The GIT-COMMIT this transaction created upon a successful
:READ-WRITE commit, or NIL if it has not (yet) committed. Always NIL
for a NESTED transaction (one with a non-NIL GET-PARENT-TRANSACTION),
since a nested transaction never itself creates a GIT-COMMIT -- only
the outermost transaction in a nesting chain does, once, when it
finally exits.")
   (parent-transaction
    :initarg :parent-transaction
    :initform nil
    :reader get-parent-transaction
    :documentation
    "The enclosing GIT-TRANSACTION this transaction is nested
within -- i.e. the GIT-TRANSACTION *GIT-TRANSACTION* was already
bound to when CALL-WITH-GIT-TRANSACTION was invoked to create this
one -- or NIL if this is an outermost transaction, opened with
*GIT-TRANSACTION* unbound or bound to NIL.")
   (head-commit
    :initarg :head-commit
    :initform nil
    :reader get-head-commit
    :documentation
    "For an outermost transaction (GET-PARENT-TRANSACTION is NIL),
the GIT-COMMIT TARGET-BRANCH was resolved to when this transaction
was opened (or NIL for an as-yet-empty branch). Used only to lazily
compute GET-CURRENT-ROOT's initial value on first read -- via
RESOLVE-COMMIT-ROOT -- so that a RECEIVER which never itself nests a
further transaction never forces that (potentially Git-object-store-
touching) resolution at all. Always NIL for a nested transaction,
which instead inherits its initial GET-CURRENT-ROOT directly from
GET-PARENT-TRANSACTION's own.")
   (%current-root-cell
    :initarg :current-root
    :initform :unresolved
    :documentation
    "Private backing cell for GET-CURRENT-ROOT/(SETF GET-CURRENT-
ROOT); see those functions. The sentinel :UNRESOLVED marks an
outermost transaction whose GET-CURRENT-ROOT has not yet been lazily
computed from HEAD-COMMIT."))
  (:documentation
   "A transient transaction boundary over the GIT-OBJECT proxy
layer. See CALL-WITH-GIT-TRANSACTION, COMMIT-GIT-TRANSACTION, and
ABORT-GIT-TRANSACTION.

GIT-TRANSACTIONs may nest: if CALL-WITH-GIT-TRANSACTION is invoked
while *GIT-TRANSACTION* is already bound to an active GIT-TRANSACTION
(GET-PARENT-TRANSACTION-wise), the new GIT-TRANSACTION it constructs
records that enclosing transaction as its own GET-PARENT-TRANSACTION,
inherits its GET-CURRENT-ROOT as its own starting point rather than
re-resolving BRANCH from Git, and -- on a normal or explicit
:READ-WRITE commit -- copies its own final GET-CURRENT-ROOT back up
into its parent's, instead of ever creating a real GIT-COMMIT or
advancing TARGET-BRANCH itself. Only the single outermost transaction
in any such nesting chain (the one whose GET-PARENT-TRANSACTION is
NIL) ever does that. If a nested transaction is aborted instead --
whether via ABORT-GIT-TRANSACTION or an error unwinding out of
RECEIVER -- its parent's GET-CURRENT-ROOT is left completely
untouched, and any Blobs/Trees the nested transaction wrote to Git's
object database along the way are simply orphaned, to be reclaimed
later by Git's own garbage collection."))

(defun get-current-root (transaction)
  "Return TRANSACTION's (a GIT-TRANSACTION) own in-progress root
GIT-OBJECT, or NIL for an as-yet-empty branch. For an outermost
transaction, this is lazily computed -- and cached -- from
GET-HEAD-COMMIT via RESOLVE-COMMIT-ROOT the first time it is ever
read (so a RECEIVER that never nests a further transaction never
forces that resolution at all); for a nested transaction, it was
already supplied directly (inherited from its parent's own
GET-CURRENT-ROOT) when the transaction was constructed. See
GIT-TRANSACTION's class docstring for how this percolates up a chain
of nested transactions."
  (let ((cell (slot-value transaction '%current-root-cell)))
    (if (eq cell :unresolved)
        (setf (slot-value transaction '%current-root-cell)
              (and (get-head-commit transaction)
                   (resolve-commit-root (get-head-commit transaction))))
        cell)))

(defun (setf get-current-root) (new-value transaction)
  "Set TRANSACTION's (a GIT-TRANSACTION) own in-progress root
GIT-OBJECT; see GET-CURRENT-ROOT."
  (setf (slot-value transaction '%current-root-cell) new-value))

(setf (documentation 'get-git-repository 'function)
      "Return TRANSACTION's (a GIT-TRANSACTION) GIT-REPOSITORY --
the repository it was opened against.")
(setf (documentation 'get-target-branch 'function)
      "Return TRANSACTION's (a GIT-TRANSACTION) GIT-BRANCH -- the
branch it will advance on a successful :READ-WRITE commit.")
(setf (documentation 'get-status 'function)
      "Return TRANSACTION's (a GIT-TRANSACTION) current status: one
of :ACTIVE, :COMMITTED, or :ABORTED.")
(setf (documentation 'get-result 'function)
      "Return the GIT-COMMIT TRANSACTION (a GIT-TRANSACTION) created
upon a successful :READ-WRITE commit, or NIL if it has not (yet)
committed. Always NIL for a nested transaction; see
GET-PARENT-TRANSACTION.")
(setf (documentation 'get-parent-transaction 'function)
      "Return the GIT-TRANSACTION TRANSACTION is nested within, or
NIL if TRANSACTION is an outermost transaction.")
(setf (documentation 'get-head-commit 'function)
      "Return TRANSACTION's (a GIT-TRANSACTION) resolved head
GIT-COMMIT, or NIL for an as-yet-empty branch, for an outermost
transaction. Always NIL for a nested transaction; see
GET-CURRENT-ROOT.")

(defun %unix-time-now ()
  "Return the current time as an integer Unix epoch timestamp."
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

;;; %UNIQUE-TEMPORARY-PATHNAME and GIT-HASH-OBJECT now live in
;;; git-io.lisp, shared with PERSISTENT-CONS's own persistence logic.

(defun %persist-git-tree-object (tree)
  "Ensure TREE (a GIT-TREE) and every one of its ENTRIES not yet
persisted -- recursively -- has a SHA, writing each unpersisted
GIT-BLOB's PAYLOAD and each unpersisted GIT-TREE's serialized
ENTRIES to Git's object database via GIT-HASH-OBJECT. Returns TREE's
own SHA."
  (or (sha tree)
      (progn
        (dolist (entry (get-entries tree))
          (%persist-git-object (cdr entry)))
        (setf (sha tree)
              (git-hash-object (get-repository tree) "tree" (serialize-tree tree))))))

(defgeneric %persist-git-object-by-type (git-object)
  (:documentation
   "Persist GIT-OBJECT (which is known not to have a SHA yet) to
Git's object database according to its concrete type, and return the
resulting SHA. Broken out of %PERSIST-GIT-OBJECT so this dispatch is
its own generic function, with one DEFMETHOD per concrete type in
place of an ETYPECASE clause."))

(defmethod %persist-git-object-by-type ((git-object persistent-object))
  (serialize-persistent-object git-object))

(defmethod %persist-git-object-by-type ((git-object persistent-cons))
  (serialize-persistent-cons git-object))

(defmethod %persist-git-object-by-type ((git-object persistent-vector))
  (serialize-persistent-vector git-object))

(defmethod %persist-git-object-by-type ((git-object persistent-array))
  (serialize-persistent-array git-object))

(defmethod %persist-git-object-by-type ((git-object git-tree))
  (%persist-git-tree-object git-object))

(defmethod %persist-git-object-by-type ((git-object git-blob))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "blob"
                          (serialize-atom (get-payload git-object)))))

(defun %persist-git-object (git-object)
  "Ensure GIT-OBJECT (a GIT-BLOB, GIT-TREE, PERSISTENT-CONS,
PERSISTENT-VECTOR, PERSISTENT-ARRAY, or PERSISTENT-OBJECT) has a
SHA, persisting it (and, for a GIT-TREE, PERSISTENT-CONS,
PERSISTENT-VECTOR, PERSISTENT-ARRAY, or PERSISTENT-OBJECT, its
children) to Git's object database if it does not already. Returns
GIT-OBJECT's SHA. Objects that already have a SHA are assumed
already present in Git's object database and are left untouched."
  (or (sha git-object)
      (%persist-git-object-by-type git-object)))

(defun %persist-git-commit-object (commit)
  "Ensure COMMIT (a GIT-COMMIT) has a SHA, writing its serialized
form to Git's object database via GIT-HASH-OBJECT if it does not
already have one. Returns COMMIT's SHA. Assumes COMMIT's TREE and
PARENTS are already persisted (SERIALIZE-COMMIT signals an error
otherwise)."
  (or (sha commit)
      (setf (sha commit)
            (git-hash-object (get-repository commit) "commit"
                              (sb-ext:string-to-octets (serialize-commit commit) :external-format :utf-8)))))

(defun %commit-git-transaction-now (transaction root)
  "Perform the actual work of committing TRANSACTION with root
GIT-OBJECT ROOT: persist ROOT (and its modified children), wrapping
it first in an ATOMIC-WRAPPER-TREE via WRAP-ATOMIC-COMMIT-ROOT if it
is not itself a GIT-TREE (a bare GIT-BLOB has no directory structure
of its own for a Git commit to point at), create and persist a new
GIT-COMMIT from TRANSACTION's cascaded AUTHOR/COMMITTER/MESSAGE and
PARENTS, and advance TRANSACTION's TARGET-BRANCH to point at it.
Records the new commit in TRANSACTION's RESULT slot and returns it."
  (check-type root git-object)
  (%persist-git-object root)
  (let* ((repository (get-pathname (get-git-repository transaction)))
         (tree (if (typep root 'git-tree)
                   root
                   (wrap-atomic-commit-root repository root)))
         (commit (make-instance 'git-commit
                                 :repository repository
                                 :tree tree
                                 :parents (get-parents transaction)
                                 :author (get-author transaction)
                                 :committer (get-committer transaction)
                                 :timestamp (%unix-time-now)
                                 :message (get-message transaction)
                                 :loaded? t)))
    (%persist-git-commit-object commit)
    (setf (get-target (get-target-branch transaction)) commit)
    (update-branch (get-target-branch transaction)
                    :expected-sha (get-expected-branch-sha transaction))
    (setf (get-result transaction) commit)
    (setf (get-current-root transaction) root)
    commit))

(defun %commit-git-transaction-with-rebase (transaction root)
  "Commit TRANSACTION (an outermost, :READ-WRITE GIT-TRANSACTION
whose CONFLICT-RESOLUTION is :REBASE) with root GIT-OBJECT ROOT.

In the common case where no other writer has advanced TARGET-BRANCH
since TRANSACTION was opened, this behaves exactly like
%COMMIT-GIT-TRANSACTION-NOW: persist ROOT (wrapping it in an
ATOMIC-WRAPPER-TREE first if it is not itself a GIT-TREE), build and
persist a new GIT-COMMIT from TRANSACTION's cascaded defaults with
GET-HEAD-COMMIT as its sole parent, and advance the branch to point
at it.

If some other writer HAS already advanced TARGET-BRANCH (detected
via GIT-UPDATE-REF's own compare-and-swap check, exactly as it would
be for :ERROR/:RETRY/:LOCK), TRANSACTION's own already-computed ROOT
is not discarded: instead, its persisted candidate commit is replayed
onto the branch's new, real HEAD via %GIT-MERGE-TREE -- Git's own
native, working-tree-free three-way content merge, which locates the
original HEAD as their common ancestor automatically. If some other
writer wins the race yet again before this replayed commit itself
can be written, the whole replay is simply retried against that even
newer HEAD, for as long as necessary.

If %GIT-MERGE-TREE ever reports a genuine, unresolvable content
conflict (both this transaction and some concurrent writer touched
the exact same content) rather than a merely-detected-but-mergeable
race, TRANSACTION's own REBASE-FALLBACK decides what happens next:
:RETRY signals CONCURRENT-MODIFICATION-ERROR, so CALL-WITH-GIT-
TRANSACTION's own :REBASE retry loop discards this whole attempt and
re-runs RECEIVER from scratch against the branch's latest HEAD;
:ERROR instead signals MERGE-CONFLICT-ERROR directly, propagating
all the way out of CALL-WITH-GIT-TRANSACTION, exactly as an ordinary,
non-mergeable race would propagate under plain :ERROR mode.

Records the winning commit in TRANSACTION's RESULT slot and its
underlying (possibly rebased) tree in its CURRENT-ROOT, exactly as
%COMMIT-GIT-TRANSACTION-NOW does. Returns that commit."
  (check-type root git-object)
  (%persist-git-object root)
  (let* ((repository (get-pathname (get-git-repository transaction)))
         (tree (if (typep root 'git-tree)
                   root
                   (wrap-atomic-commit-root repository root)))
         (branch-name (get-name (get-target-branch transaction)))
         (base-sha (get-expected-branch-sha transaction))
         (candidate-commit (make-instance 'git-commit
                                           :repository repository
                                           :tree tree
                                           :parents (get-parents transaction)
                                           :author (get-author transaction)
                                           :committer (get-committer transaction)
                                           :timestamp (%unix-time-now)
                                           :message (get-message transaction)
                                           :loaded? t)))
    (%persist-git-commit-object candidate-commit)
    (flet ((finish (commit)
             (setf (get-target (get-target-branch transaction)) commit)
             (setf (get-result transaction) commit)
             (setf (get-current-root transaction) root)
             commit))
      (handler-case
          (progn
            (git-update-ref repository branch-name (sha candidate-commit) :expected-sha base-sha)
            (finish candidate-commit))
        (concurrent-modification-error ()
          ;; Someone else already won the race: replay our own
          ;; already-persisted candidate commit onto their new HEAD
          ;; instead of giving up, retrying against an ever-fresher
          ;; HEAD for as long as the branch keeps moving out from
          ;; under us.
          (loop
            (let ((current-head-sha (git-show-ref-sha repository branch-name)))
              (multiple-value-bind (merged-tree-sha conflict-detail)
                  (%git-merge-tree repository (sha candidate-commit) current-head-sha)
                (if merged-tree-sha
                    (let* ((rebased-commit
                             (make-instance
                              'git-commit
                              :repository repository
                              :tree (make-instance 'git-tree :repository repository :sha merged-tree-sha)
                              :parents (list (make-instance 'git-commit :repository repository :sha current-head-sha))
                              :author (get-author transaction)
                              :committer (get-committer transaction)
                              :timestamp (%unix-time-now)
                              :message (get-message transaction)
                              :loaded? t))
                           (raced-again nil))
                      (%persist-git-commit-object rebased-commit)
                      (handler-case
                          (git-update-ref repository branch-name (sha rebased-commit) :expected-sha current-head-sha)
                        (concurrent-modification-error () (setf raced-again t)))
                      (unless raced-again
                        (return (finish rebased-commit))))
                    (ecase (get-rebase-fallback transaction)
                      (:retry (error 'concurrent-modification-error
                                     :repository repository :name branch-name
                                     :expected-sha base-sha :new-sha current-head-sha
                                     :detail (format nil "Unresolvable rebase merge conflict; falling back to :RETRY.~@[~%~A~]"
                                                      conflict-detail)))
                      (:error (error 'merge-conflict-error
                                     :repository repository :name branch-name
                                     :base-sha base-sha
                                     :candidate-sha (sha candidate-commit)
                                     :current-head-sha current-head-sha
                                     :detail conflict-detail)))))))))))) 

(defun commit-git-transaction (transaction root)
  "Explicitly and immediately commit TRANSACTION with root GIT-OBJECT
ROOT.

If TRANSACTION has no GET-PARENT-TRANSACTION (it is an outermost
transaction): persist ROOT and its modified children (wrapping it in
an ATOMIC-WRAPPER-TREE first if ROOT is not itself a GIT-TREE),
create and persist a new GIT-COMMIT from TRANSACTION's cascaded
defaults, and advance its branch to point at that commit -- via
%COMMIT-GIT-TRANSACTION-NOW, or, if TRANSACTION's CONFLICT-RESOLUTION
is :REBASE, via %COMMIT-GIT-TRANSACTION-WITH-REBASE instead.

If TRANSACTION IS nested (GET-PARENT-TRANSACTION is non-NIL): no
GIT-COMMIT is created and no branch is touched. Instead, ROOT simply
becomes TRANSACTION's own GET-CURRENT-ROOT, which is then copied up
into GET-PARENT-TRANSACTION's own GET-CURRENT-ROOT once RECEIVER's
enclosing CALL-WITH-GIT-TRANSACTION call unwinds -- see
%CALL-WITH-NESTED-GIT-TRANSACTION.

Signals an error if TRANSACTION is not :READ-WRITE or is no longer
:ACTIVE. Immediately unwinds out of the enclosing
CALL-WITH-GIT-TRANSACTION's RECEIVER, so any code after this call
within RECEIVER never runs."
  (unless (eq (get-status transaction) :active)
    (error 'transaction-state-error
           :format-control "Transaction is not active (status is ~S)."
           :format-arguments (list (get-status transaction))))
  (unless (eq (get-mode transaction) :read-write)
    (error 'transaction-state-error
           :format-control "Cannot commit a :READ-ONLY transaction."))
  (if (get-parent-transaction transaction)
      (progn
        (check-type root git-object)
        (setf (get-current-root transaction) root))
      (if (eq (get-conflict-resolution transaction) :rebase)
          (%commit-git-transaction-with-rebase transaction root)
          (%commit-git-transaction-now transaction root)))
  (setf (get-status transaction) :committed)
  (throw 'git-transaction-exit transaction))

(defun abort-git-transaction (transaction)
  "Explicitly and immediately terminate TRANSACTION, discarding all
transient state without writing anything to Git. If TRANSACTION is
nested (GET-PARENT-TRANSACTION is non-NIL), its parent's own
GET-CURRENT-ROOT is left completely untouched -- none of TRANSACTION's
own writes ever percolate up to it. Immediately unwinds out of the
enclosing CALL-WITH-GIT-TRANSACTION's RECEIVER, so any code after
this call within RECEIVER never runs."
  (setf (get-status transaction) :aborted)
  (throw 'git-transaction-exit transaction))

(defun %call-with-git-transaction-attempt
    (repository mode branch-name final-author final-committer final-message parents
     receiver conflict-resolution rebase-fallback)
  "Perform exactly one attempt at opening and (for :READ-WRITE)
committing a GIT-TRANSACTION against REPOSITORY: resolve BRANCH-NAME
fresh (via RESOLVE-BRANCH) to its current head GIT-COMMIT, construct
a transient GIT-TRANSACTION recording that head's SHA as its own
EXPECTED-BRANCH-SHA, invoke RECEIVER, and, for a normal return from a
:READ-WRITE transaction, commit it -- via %COMMIT-GIT-TRANSACTION-NOW,
or, if CONFLICT-RESOLUTION is :REBASE, via %COMMIT-GIT-TRANSACTION-
WITH-REBASE instead (REBASE-FALLBACK is only ever consulted by that
latter path). May signal CONCURRENT-MODIFICATION-ERROR (propagated up
from UPDATE-BRANCH's/GIT-UPDATE-REF's own compare-and-swap check) if
some other writer already advanced BRANCH-NAME between this attempt's
read and its commit -- or, for :REBASE with REBASE-FALLBACK :ERROR, a
genuine, unresolvable content conflict may instead surface as
MERGE-CONFLICT-ERROR. Returns the resulting GIT-TRANSACTION."
  (let* ((target-branch (resolve-branch (get-pathname repository) branch-name :if-does-not-exist nil))
         (head-commit (get-target target-branch))
         ;; ORPHAN-COMMIT GUARANTEE: if BRANCH-NAME does not exist yet,
         ;; RESOLVE-BRANCH (called with :IF-DOES-NOT-EXIST NIL above)
         ;; returns a GIT-BRANCH whose TARGET is NIL, so HEAD-COMMIT is
         ;; NIL here. FINAL-PARENTS then defaults to the empty list --
         ;; SERIALIZE-COMMIT (git-commit.lisp) therefore emits no
         ;; "parent" header line at all, producing a genuine orphan root
         ;; commit -- and EXPECTED-BRANCH-SHA below is likewise NIL,
         ;; which UPDATE-BRANCH/GIT-UPDATE-REF (git-branch.lisp) pass to
         ;; `git update-ref` as an empty old-value argument, Git's own
         ;; convention (equivalent to the all-zeroes SHA) for "this ref
         ;; must not already exist" -- so a concurrently-created branch
         ;; is still caught as a CONCURRENT-MODIFICATION-ERROR rather
         ;; than silently clobbered.
         (final-parents (or parents (and head-commit (list head-commit))))
         (transaction (make-instance 'git-transaction
                                      :git-repository repository
                                      :mode mode
                                      :target-branch target-branch
                                      :author final-author
                                      :committer final-committer
                                      :message final-message
                                      :parents final-parents
                                      :conflict-resolution conflict-resolution
                                      :rebase-fallback rebase-fallback
                                      :expected-branch-sha (and head-commit (sha head-commit))
                                      :head-commit head-commit)))
    (let ((*git-transaction* transaction))
      (let ((root (catch 'git-transaction-exit
                    (funcall receiver transaction head-commit))))
        (when (eq (get-status transaction) :active)
          (when (eq mode :read-write)
            (if (eq conflict-resolution :rebase)
                (%commit-git-transaction-with-rebase transaction root)
                (%commit-git-transaction-now transaction root)))
          (setf (get-status transaction) :committed))))
    transaction))

(defun %synthesize-head-commit-for-root (repository root)
  "Return a transient GIT-COMMIT, never itself persisted, whose
logical root -- as RESOLVE-COMMIT-ROOT would recover it -- is ROOT,
or NIL if ROOT is NIL. Used by %CALL-WITH-NESTED-GIT-TRANSACTION to
feed a nested GIT-TRANSACTION's inherited, possibly not-yet-committed
GET-CURRENT-ROOT to RECEIVER as its HEAD-COMMIT argument, through
the very same RESOLVE-COMMIT-ROOT path an outermost transaction's
real head commit would use. ROOT is persisted first (via
%PERSIST-GIT-OBJECT) and wrapped in an ATOMIC-WRAPPER-TREE (via
WRAP-ATOMIC-COMMIT-ROOT) unless it is already a GIT-TREE, exactly as
%COMMIT-GIT-TRANSACTION-NOW would for a real commit -- any Blob/Tree
this writes to Git's object database is harmless: it becomes part of
a real commit if some enclosing transaction eventually commits for
real, or is simply orphaned Git garbage otherwise."
  (and root
       (let ((tree (if (typep root 'git-tree)
                        root
                        (progn (%persist-git-object root)
                               (wrap-atomic-commit-root repository root)))))
         (make-instance 'git-commit
                         :repository repository
                         :tree tree
                         :parents '()
                         :author ""
                         :committer ""
                         :timestamp 0
                         :message ""
                         :loaded? t))))

(defun %call-with-nested-git-transaction (parent mode receiver)
  "Perform a NESTED GIT-TRANSACTION, opened while PARENT (a
GIT-TRANSACTION) is already active in *GIT-TRANSACTION*: construct a
new GIT-TRANSACTION recording PARENT as its own GET-PARENT-
TRANSACTION and inheriting PARENT's GET-CURRENT-ROOT as its own
starting point (rather than resolving any branch fresh against Git),
cascading PARENT's own AUTHOR/COMMITTER/MESSAGE/PARENTS/TARGET-
BRANCH/CONFLICT-RESOLUTION, invoke RECEIVER with that new
GIT-TRANSACTION and a synthetic HEAD-COMMIT reflecting its inherited
root (see %SYNTHESIZE-HEAD-COMMIT-FOR-ROOT), and, if RECEIVER exits
normally or via an explicit COMMIT-GIT-TRANSACTION for a :READ-WRITE
transaction, copy the resulting root back up into PARENT's own
GET-CURRENT-ROOT -- without ever creating a real GIT-COMMIT or
touching any branch ref. If RECEIVER instead exits via an error or
an explicit ABORT-GIT-TRANSACTION, PARENT's GET-CURRENT-ROOT is left
completely untouched. Signals TRANSACTION-STATE-ERROR if MODE is
:READ-WRITE but PARENT is :READ-ONLY. Returns the nested
GIT-TRANSACTION."
  (when (and (eq mode :read-write) (eq (get-mode parent) :read-only))
    (error 'transaction-state-error
           :format-control "Cannot open a nested :READ-WRITE transaction inside an enclosing :READ-ONLY transaction."))
  (let* ((repository (get-pathname (get-git-repository parent)))
         (transaction (make-instance 'git-transaction
                                      :git-repository (get-git-repository parent)
                                      :mode mode
                                      :target-branch (get-target-branch parent)
                                      :author (get-author parent)
                                      :committer (get-committer parent)
                                      :message (get-message parent)
                                      :parents (get-parents parent)
                                      :conflict-resolution (get-conflict-resolution parent)
                                      :rebase-fallback (get-rebase-fallback parent)
                                      :expected-branch-sha (get-expected-branch-sha parent)
                                      :parent-transaction parent
                                      :current-root (get-current-root parent))))
    (let ((*git-transaction* transaction))
      (let ((root (catch 'git-transaction-exit
                    (funcall receiver transaction
                             (%synthesize-head-commit-for-root repository (get-current-root transaction))))))
        (when (eq (get-status transaction) :active)
          (when (eq mode :read-write)
            (setf (get-current-root transaction) root))
          (setf (get-status transaction) :committed))))
    (when (and (eq (get-status transaction) :committed) (eq mode :read-write))
      (setf (get-current-root parent) (get-current-root transaction)))
    transaction))

(defun call-with-git-transaction (repository mode &key branch author committer message parents receiver
                                                        (conflict-resolution :error) (rebase-fallback :error))
  "Open a GIT-TRANSACTION against REPOSITORY (a GIT-REPOSITORY),
cascading BRANCH/AUTHOR/COMMITTER/MESSAGE from REPOSITORY's own
defaults for any not explicitly supplied here. Resolves BRANCH to
its current head GIT-COMMIT (via RESOLVE-BRANCH and
INFLATE-GIT-PROXY) and, unless PARENTS is supplied, defaults PARENTS
to a list of just that head commit -- or to the empty list if BRANCH
does not exist yet (an empty repository, awaiting its initial
commit), in which case HEAD-COMMIT is NIL. Invokes (FUNCALL RECEIVER
TRANSACTION HEAD-COMMIT), with *GIT-TRANSACTION* dynamically bound
to TRANSACTION for the duration of that call. RECEIVER may call RESOLVE-COMMIT-ROOT on
HEAD-COMMIT to transparently retrieve its logical root object,
whether that root is a GIT-TREE or (having been auto-wrapped by a
prior commit) a bare atomic GIT-BLOB.

Signals INVALID-ARGUMENT-ERROR if REPOSITORY is not a GIT-REPOSITORY,
if MODE is not :READ-ONLY or :READ-WRITE, if RECEIVER is not a
callable function, or if the effective BRANCH name (after cascading
from REPOSITORY's own default) is not a non-empty string. Signals
TRANSACTION-STATE-ERROR if MODE is :READ-WRITE but REPOSITORY was
opened :READ-ONLY.

NESTING: if this call occurs while *GIT-TRANSACTION* is already
dynamically bound to an enclosing, still-active GIT-TRANSACTION --
i.e. from within the dynamic extent of another CALL-WITH-GIT-
TRANSACTION call against the same REPOSITORY -- this instead opens a
NESTED transaction: BRANCH is never re-resolved against Git at all,
and BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS/CONFLICT-RESOLUTION are
all ignored, cascaded from the enclosing transaction instead. The
nested transaction inherits the enclosing transaction's current root
state as RECEIVER's HEAD-COMMIT, and, on a normal or explicit
:READ-WRITE commit, copies its own final root back up into the
enclosing transaction's -- without ever creating a real GIT-COMMIT or
advancing any branch itself; only the single outermost transaction in
a nesting chain ever does that, once, when it finally exits. If the
nested transaction instead aborts (explicitly, or via an error
unwinding out of RECEIVER), the enclosing transaction's state is left
completely untouched, and any Blobs/Trees the nested transaction
wrote along the way are simply orphaned Git garbage. Signals
INVALID-ARGUMENT-ERROR if REPOSITORY does not match the enclosing
transaction's own repository. See %CALL-WITH-NESTED-GIT-TRANSACTION.

If RECEIVER returns normally, it must return a GIT-OBJECT
representing the desired new root state -- a GIT-TREE (or
PERSISTENT-CONS or other GIT-TREE subtype), committed directly, or a
bare atomic GIT-BLOB, transparently wrapped first in an
ATOMIC-WRAPPER-TREE by WRAP-ATOMIC-COMMIT-ROOT, since Git itself
requires every commit to point at a tree. When MODE is :READ-WRITE,
that root (and its modified children) is then automatically
persisted, a new commit created and persisted from it, and the
branch advanced -- exactly as COMMIT-GIT-TRANSACTION would (this
also creates BRANCH's ref for the first time, if it did not already
exist). If RECEIVER instead calls COMMIT-GIT-TRANSACTION or
ABORT-GIT-TRANSACTION itself, or signals an error, that explicit or
abnormal exit is honored instead and nothing further is written.

CONFLICT-RESOLUTION controls what happens if some other writer
already advanced (or created) BRANCH between this transaction's own
read of its head commit and its own commit -- Git's 'Lost Update'
problem -- detected via GIT-UPDATE-REF's own compare-and-swap check:
* :ERROR (the default) lets CONCURRENT-MODIFICATION-ERROR propagate
  out of this call immediately; nothing is written.
* :RETRY catches CONCURRENT-MODIFICATION-ERROR and re-attempts the
  entire transaction from scratch -- re-resolving BRANCH to its
  latest head and re-invoking RECEIVER against that fresh state --
  looping until it succeeds. Because RECEIVER may thus run more than
  once, it MUST be free of any side effect other than reading and
  returning GIT-OBJECTs/persistent proxies: no network calls, no
  file I/O outside of Git itself, and no mutation of any global or
  shared Lisp state, since GitHack cannot undo such a side effect if
  RECEIVER is silently re-run. (GitHack's own proxy/serialization
  pipeline already satisfies this: the only mutation it ever performs
  is a proxy's own lazy-load SHA/cache slot on the specific instance
  being read or written, which is safe, transparent memoization, not
  externally observable side-effecting state.)
* :LOCK instead acquires an exclusive, repository-wide OS-level lock
  (see WITH-REPOSITORY-TRANSACTION-LOCK) before even reading BRANCH's
  head commit, and holds it until the commit (or abort) is finalized,
  so no other :LOCK-mode transaction against the same repository can
  run concurrently, and this attempt should therefore never actually
  observe a real compare-and-swap conflict.
* :REBASE, like :ERROR/:RETRY/:LOCK, still detects the race via
  GIT-UPDATE-REF's own compare-and-swap check, but does not discard
  RECEIVER's own computation: it replays TRANSACTION's already-
  computed candidate commit onto the branch's new HEAD via
  %GIT-MERGE-TREE (Git's own native, working-tree-free three-way
  content merge), retrying that replay against an ever-fresher HEAD
  for as long as other writers keep winning the race, and only
  re-invoking RECEIVER from scratch (like :RETRY) or signaling an
  error (like :ERROR) if a genuine, unresolvable content conflict is
  ever found -- see REBASE-FALLBACK.

REBASE-FALLBACK is only consulted when CONFLICT-RESOLUTION is
:REBASE, and only once %GIT-MERGE-TREE reports a genuine,
unresolvable content conflict (as opposed to a merely-detected-but-
still-mergeable race): :ERROR (the default) signals MERGE-CONFLICT-
ERROR directly, so it propagates out of this call exactly as an
ordinary, non-mergeable :ERROR-mode race would; :RETRY instead
signals CONCURRENT-MODIFICATION-ERROR, discarding this transaction's
entire computation and re-running RECEIVER from scratch against the
branch's latest HEAD, exactly as plain :RETRY mode would for an
ordinary race.

Returns TRANSACTION."
  (check-type conflict-resolution (member :error :retry :lock :rebase))
  (check-type rebase-fallback (member :error :retry))
  (unless (typep repository 'git-repository)
    (error 'invalid-argument-error
           :format-control "REPOSITORY must be a GIT-REPOSITORY, not ~S."
           :format-arguments (list repository)))
  (unless (member mode '(:read-only :read-write))
    (error 'invalid-argument-error
           :format-control "MODE must be :READ-ONLY or :READ-WRITE, not ~S."
           :format-arguments (list mode)))
  (unless (or (functionp receiver) (and (symbolp receiver) receiver (fboundp receiver)))
    (error 'invalid-argument-error
           :format-control "RECEIVER must be a callable function, not ~S."
           :format-arguments (list receiver)))
  ;; NESTING: if *GIT-TRANSACTION* is already bound to an active
  ;; enclosing GIT-TRANSACTION, this call opens a NESTED transaction
  ;; instead of a fresh outermost one -- see
  ;; %CALL-WITH-NESTED-GIT-TRANSACTION's own docstring for the full
  ;; nesting semantics. BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS/
  ;; CONFLICT-RESOLUTION are all ignored in that case (a nested
  ;; transaction always cascades those from its parent instead, since
  ;; it never itself creates a GIT-COMMIT or advances any branch).
  (when (and (boundp '*git-transaction*) *git-transaction*)
    (let ((parent *git-transaction*))
      (unless (equal (get-pathname repository) (get-pathname (get-git-repository parent)))
        (error 'invalid-argument-error
               :format-control "A nested transaction's REPOSITORY (~S) must be the same as its enclosing transaction's (~S)."
               :format-arguments (list repository (get-git-repository parent))))
      (return-from call-with-git-transaction
        (%call-with-nested-git-transaction parent mode receiver))))
  (when (and (eq mode :read-write) (eq (get-mode repository) :read-only))
    (error 'transaction-state-error
           :format-control "Cannot open a :READ-WRITE transaction against a repository opened :READ-ONLY."))
  (let* ((branch-name (or branch (get-branch repository)))
         (final-author (or author (get-author repository)))
         (final-committer (or committer (get-committer repository) final-author))
         (final-message (or message (get-message repository))))
    (unless (and (stringp branch-name) (plusp (length branch-name)))
      (error 'invalid-argument-error
             :format-control "BRANCH must be a non-empty string, not ~S."
             :format-arguments (list branch-name)))
    (flet ((attempt ()
             (%call-with-git-transaction-attempt
              repository mode branch-name final-author final-committer final-message
              parents receiver conflict-resolution rebase-fallback)))
      (ecase conflict-resolution
        (:error (attempt))
        (:retry (loop
                  (handler-case
                      (return (attempt))
                    (concurrent-modification-error () nil))))
        (:lock (with-repository-transaction-lock ((get-pathname repository))
                 (attempt)))
        (:rebase (loop
                   (handler-case
                       (return (attempt))
                     (concurrent-modification-error () nil))))))))

(defmacro with-git-transaction ((transaction-var head-commit-var) (repository mode &key branch author committer message parents (conflict-resolution :error) (rebase-fallback :error)) &body body)
  "Macro wrapper around CALL-WITH-GIT-TRANSACTION: expands into a
call to CALL-WITH-GIT-TRANSACTION on REPOSITORY and MODE (evaluated
once each), passing BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS/
CONFLICT-RESOLUTION/REBASE-FALLBACK through unchanged, with
:RECEIVER bound to a closure over BODY. Within BODY, TRANSACTION-VAR
is bound to the transient GIT-TRANSACTION and HEAD-COMMIT-VAR to the
resolved head GIT-COMMIT, exactly as they would be passed to an
explicit RECEIVER function.

BODY's normal return value is subject to the same auto-commit
semantics as CALL-WITH-GIT-TRANSACTION's RECEIVER: for a :READ-WRITE
transaction, BODY must return a GIT-OBJECT (a GIT-TREE, or a bare
atomic GIT-BLOB to be auto-wrapped) representing the desired new
root, which is then automatically committed and the branch advanced,
unless BODY has already called COMMIT-GIT-TRANSACTION or
ABORT-GIT-TRANSACTION itself. See CALL-WITH-GIT-TRANSACTION's own
docstring for CONFLICT-RESOLUTION's four modes, REBASE-FALLBACK, and,
crucially, the purity requirement :RETRY (and :REBASE, on a genuine
merge conflict with REBASE-FALLBACK :RETRY) places on BODY. Returns
the GIT-TRANSACTION, exactly as CALL-WITH-GIT-TRANSACTION does."
  `(call-with-git-transaction ,repository ,mode
                               :branch ,branch
                               :author ,author
                               :committer ,committer
                               :message ,message
                               :parents ,parents
                               :conflict-resolution ,conflict-resolution
                               :rebase-fallback ,rebase-fallback
                               :receiver (lambda (,transaction-var ,head-commit-var)
                                           ,@body)))
