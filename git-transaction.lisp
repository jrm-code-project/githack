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

(defun %persist-git-object (git-object)
  "Ensure GIT-OBJECT (a GIT-BLOB, GIT-TREE, PERSISTENT-CONS,
PERSISTENT-VECTOR, or PERSISTENT-ARRAY) has a SHA, persisting it
(and, for a GIT-TREE, PERSISTENT-CONS, PERSISTENT-VECTOR, or
PERSISTENT-ARRAY, its children) to Git's object database if it does
not already. Returns GIT-OBJECT's SHA. Objects that already have a
SHA are assumed already present in Git's object database and are
left untouched."
  (or (sha git-object)
      (etypecase git-object
        (persistent-cons (serialize-persistent-cons git-object))
        (persistent-vector (serialize-persistent-vector git-object))
        (persistent-array (serialize-persistent-array git-object))
        (git-tree (%persist-git-tree-object git-object))
        (git-blob
         (setf (sha git-object)
               (git-hash-object (get-repository git-object) "blob"
                                 (serialize-atom (get-payload git-object))))))))

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
    (update-branch (get-target-branch transaction))
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
    (error "Transaction is not active (status is ~S)." (get-status transaction)))
  (unless (eq (get-mode transaction) :read-write)
    (error "Cannot commit a :READ-ONLY transaction."))
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

(defun call-with-git-transaction (repository mode &key branch author committer message parents receiver)
  "Open a GIT-TRANSACTION against REPOSITORY (a GIT-REPOSITORY),
cascading BRANCH/AUTHOR/COMMITTER/MESSAGE from REPOSITORY's own
defaults for any not explicitly supplied here. Resolves BRANCH to
its current head GIT-COMMIT (via RESOLVE-BRANCH and
INFLATE-GIT-PROXY) and, unless PARENTS is supplied, defaults PARENTS
to a list of just that head commit -- or to the empty list if BRANCH
does not exist yet (an empty repository, awaiting its initial
commit), in which case HEAD-COMMIT is NIL. Invokes (FUNCALL RECEIVER
TRANSACTION HEAD-COMMIT). RECEIVER may call RESOLVE-COMMIT-ROOT on
HEAD-COMMIT to transparently retrieve its logical root object,
whether that root is a GIT-TREE or (having been auto-wrapped by a
prior commit) a bare atomic GIT-BLOB.

Signals an error if MODE is :READ-WRITE but REPOSITORY was opened
:READ-ONLY.

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
Returns TRANSACTION."
  (when (and (eq mode :read-write) (eq (get-mode repository) :read-only))
    (error "Cannot open a :READ-WRITE transaction against a repository opened :READ-ONLY."))
  (let* ((branch-name (or branch (get-branch repository)))
         (final-author (or author (get-author repository)))
         (final-committer (or committer (get-committer repository) final-author))
         (final-message (or message (get-message repository)))
         (target-branch (resolve-branch (get-pathname repository) branch-name :if-does-not-exist nil))
         (head-commit (get-target target-branch))
         (final-parents (or parents (and head-commit (list head-commit))))
         (transaction (make-instance 'git-transaction
                                      :git-repository repository
                                      :mode mode
                                      :target-branch target-branch
                                      :author final-author
                                      :committer final-committer
                                      :message final-message
                                      :parents final-parents)))
    (let ((root (catch 'git-transaction-exit
                  (funcall receiver transaction head-commit))))
      (when (eq (get-status transaction) :active)
        (when (eq mode :read-write)
          (%commit-git-transaction-now transaction root))
        (setf (get-status transaction) :committed)))
    transaction))

(defmacro with-git-transaction ((transaction-var head-commit-var) (repository mode &key branch author committer message parents) &body body)
  "Macro wrapper around CALL-WITH-GIT-TRANSACTION: expands into a
call to CALL-WITH-GIT-TRANSACTION on REPOSITORY and MODE (evaluated
once each), passing BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS through
unchanged, with :RECEIVER bound to a closure over BODY. Within BODY,
TRANSACTION-VAR is bound to the transient GIT-TRANSACTION and
HEAD-COMMIT-VAR to the resolved head GIT-COMMIT, exactly as they
would be passed to an explicit RECEIVER function.

BODY's normal return value is subject to the same auto-commit
semantics as CALL-WITH-GIT-TRANSACTION's RECEIVER: for a :READ-WRITE
transaction, BODY must return a GIT-OBJECT (a GIT-TREE, or a bare
atomic GIT-BLOB to be auto-wrapped) representing the desired new
root, which is then automatically committed and the branch advanced,
unless BODY has already called COMMIT-GIT-TRANSACTION or
ABORT-GIT-TRANSACTION itself. Returns the GIT-TRANSACTION, exactly
as CALL-WITH-GIT-TRANSACTION does."
  `(call-with-git-transaction ,repository ,mode
                               :branch ,branch
                               :author ,author
                               :committer ,committer
                               :message ,message
                               :parents ,parents
                               :receiver (lambda (,transaction-var ,head-commit-var)
                                           ,@body)))
