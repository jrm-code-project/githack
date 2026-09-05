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
    :type (member :error :retry :lock)
    :documentation
    "One of :ERROR, :RETRY, or :LOCK, as passed to
CALL-WITH-GIT-TRANSACTION, controlling how a 'Lost Update' conflict
-- some other writer having already advanced TARGET-BRANCH between
this transaction's own read and its commit -- is resolved.")
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
:READ-WRITE commit, or NIL if it has not (yet) committed."))
  (:documentation
   "A transient transaction boundary over the GIT-OBJECT proxy
layer. See CALL-WITH-GIT-TRANSACTION, COMMIT-GIT-TRANSACTION, and
ABORT-GIT-TRANSACTION."))

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

(defgeneric %persist-git-object-etypecase (git-object)
  (:documentation
   "Persist GIT-OBJECT (which is known not to have a SHA yet) to
Git's object database according to its concrete type, and return the
resulting SHA. Broken out of %PERSIST-GIT-OBJECT so this dispatch is
its own generic function, with one DEFMETHOD per concrete type in
place of an ETYPECASE clause."))

(defmethod %persist-git-object-etypecase ((git-object persistent-object))
  (serialize-persistent-object git-object))

(defmethod %persist-git-object-etypecase ((git-object persistent-cons))
  (serialize-persistent-cons git-object))

(defmethod %persist-git-object-etypecase ((git-object persistent-vector))
  (serialize-persistent-vector git-object))

(defmethod %persist-git-object-etypecase ((git-object persistent-array))
  (serialize-persistent-array git-object))

(defmethod %persist-git-object-etypecase ((git-object git-tree))
  (%persist-git-tree-object git-object))

(defmethod %persist-git-object-etypecase ((git-object git-blob))
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
      (%persist-git-object-etypecase git-object)))

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
    commit))

(defun commit-git-transaction (transaction root)
  "Explicitly and immediately commit TRANSACTION with root GIT-OBJECT
ROOT: persist ROOT and its modified children (wrapping it in an
ATOMIC-WRAPPER-TREE first if ROOT is not itself a GIT-TREE), create
and persist a new GIT-COMMIT from TRANSACTION's cascaded defaults,
and advance its branch to point at that commit. Signals an error if
TRANSACTION is not :READ-WRITE or is no longer :ACTIVE. Immediately
unwinds out of the enclosing CALL-WITH-GIT-TRANSACTION's RECEIVER,
so any code after this call within RECEIVER never runs."
  (unless (eq (get-status transaction) :active)
    (error 'transaction-state-error
           :format-control "Transaction is not active (status is ~S)."
           :format-arguments (list (get-status transaction))))
  (unless (eq (get-mode transaction) :read-write)
    (error 'transaction-state-error
           :format-control "Cannot commit a :READ-ONLY transaction."))
  (%commit-git-transaction-now transaction root)
  (setf (get-status transaction) :committed)
  (throw 'git-transaction-exit transaction))

(defun abort-git-transaction (transaction)
  "Explicitly and immediately terminate TRANSACTION, discarding all
transient state without writing anything to Git. Immediately unwinds
out of the enclosing CALL-WITH-GIT-TRANSACTION's RECEIVER, so any
code after this call within RECEIVER never runs."
  (setf (get-status transaction) :aborted)
  (throw 'git-transaction-exit transaction))

(defun %call-with-git-transaction-attempt
    (repository mode branch-name final-author final-committer final-message parents
     receiver conflict-resolution)
  "Perform exactly one attempt at opening and (for :READ-WRITE)
committing a GIT-TRANSACTION against REPOSITORY: resolve BRANCH-NAME
fresh (via RESOLVE-BRANCH) to its current head GIT-COMMIT, construct
a transient GIT-TRANSACTION recording that head's SHA as its own
EXPECTED-BRANCH-SHA, invoke RECEIVER, and, for a normal return from a
:READ-WRITE transaction, commit it. May signal
CONCURRENT-MODIFICATION-ERROR (propagated up from UPDATE-BRANCH's
own compare-and-swap check inside %COMMIT-GIT-TRANSACTION-NOW) if
some other writer already advanced BRANCH-NAME between this
attempt's read and its commit. Returns the resulting GIT-TRANSACTION."
  (let* ((target-branch (resolve-branch (get-pathname repository) branch-name :if-does-not-exist nil))
         (head-commit (get-target target-branch))
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
                                      :expected-branch-sha (and head-commit (sha head-commit)))))
    (let ((*git-transaction* transaction))
      (let ((root (catch 'git-transaction-exit
                    (funcall receiver transaction head-commit))))
        (when (eq (get-status transaction) :active)
          (when (eq mode :read-write)
            (%commit-git-transaction-now transaction root))
          (setf (get-status transaction) :committed))))
    transaction))

(defun call-with-git-transaction (repository mode &key branch author committer message parents receiver (conflict-resolution :error))
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

Returns TRANSACTION."
  (check-type conflict-resolution (member :error :retry :lock))
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
              parents receiver conflict-resolution)))
      (ecase conflict-resolution
        (:error (attempt))
        (:retry (loop
                  (handler-case
                      (return (attempt))
                    (concurrent-modification-error () nil))))
        (:lock (with-repository-transaction-lock ((get-pathname repository))
                 (attempt)))))))

(defmacro with-git-transaction ((transaction-var head-commit-var) (repository mode &key branch author committer message parents (conflict-resolution :error)) &body body)
  "Macro wrapper around CALL-WITH-GIT-TRANSACTION: expands into a
call to CALL-WITH-GIT-TRANSACTION on REPOSITORY and MODE (evaluated
once each), passing BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS/
CONFLICT-RESOLUTION through unchanged, with :RECEIVER bound to a
closure over BODY. Within BODY, TRANSACTION-VAR is bound to the
transient GIT-TRANSACTION and HEAD-COMMIT-VAR to the resolved head
GIT-COMMIT, exactly as they would be passed to an explicit RECEIVER
function.

BODY's normal return value is subject to the same auto-commit
semantics as CALL-WITH-GIT-TRANSACTION's RECEIVER: for a :READ-WRITE
transaction, BODY must return a GIT-OBJECT (a GIT-TREE, or a bare
atomic GIT-BLOB to be auto-wrapped) representing the desired new
root, which is then automatically committed and the branch advanced,
unless BODY has already called COMMIT-GIT-TRANSACTION or
ABORT-GIT-TRANSACTION itself. See CALL-WITH-GIT-TRANSACTION's own
docstring for CONFLICT-RESOLUTION's three modes and, crucially, the
purity requirement :RETRY places on BODY. Returns the GIT-TRANSACTION,
exactly as CALL-WITH-GIT-TRANSACTION does."
  `(call-with-git-transaction ,repository ,mode
                               :branch ,branch
                               :author ,author
                               :committer ,committer
                               :message ,message
                               :parents ,parents
                               :conflict-resolution ,conflict-resolution
                               :receiver (lambda (,transaction-var ,head-commit-var)
                                           ,@body)))
