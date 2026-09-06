;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; GITHACK-ERROR and its subtypes let callers programmatically
;;; distinguish categories of GitHack's own failures (as opposed to
;;; conditions signaled by underlying libraries, or by Common Lisp
;;; itself) without resorting to parsing error-message text. Loaded
;;; very early (depending on nothing but PACKAGE) so every other file
;;; may signal these conditions without introducing a load-order
;;; cycle.
;;;
;;; Most subtypes below carry no slots of their own and are simply
;;; signaled via (ERROR 'SUBTYPE :FORMAT-CONTROL "..." :FORMAT-ARGUMENTS
;;; (LIST ...)), inheriting SIMPLE-ERROR's own REPORT method -- so
;;; they read identically, from a caller's point of view, to the bare
;;; CL:ERROR calls they replace, except for now being a
;;; programmatically distinguishable condition class. A few
;;; (BRANCH-NOT-FOUND-ERROR, CONCURRENT-MODIFICATION-ERROR in
;;; git-branch.lisp, TRANSACTION-LOCK-TIMEOUT-ERROR in
;;; transaction-lock.lisp) carry their own structured slots and a
;;; custom REPORT method instead.

(define-condition githack-error (simple-error)
  ()
  (:documentation
   "Base condition for every error GitHack itself signals, as
opposed to conditions signaled by underlying libraries or by Common
Lisp itself. Subclasses let callers programmatically distinguish
categories of GitHack failure instead of pattern-matching error
message text. Like SIMPLE-ERROR (which this inherits from), accepts
:FORMAT-CONTROL/:FORMAT-ARGUMENTS initargs for building a
human-readable REPORT."))

(define-condition malformed-git-object-error (githack-error)
  ()
  (:documentation
   "Signaled when parsing/deserializing a Git object's raw bytes or
text -- a tree's packed binary entries, a commit's plain-text
headers, a blob's atom envelope, or the \".meta\" blob of a
persistent structure -- finds unexpected or invalid structure: a
corrupt object, or one that was never written by GitHack's own
serialization code in the first place."))

(define-condition unpersisted-object-error (githack-error)
  ()
  (:documentation
   "Signaled when an operation requires a GIT-OBJECT to already be
persisted (i.e. to already have a SHA), or a persistent structure's
own required internal state to already be complete (e.g. a
PERSISTENT-CONS's PERSISTENT-CAR, or a PERSISTENT-ARRAY's DIMENSIONS/
DATA), before it can be serialized -- but it is not."))

(define-condition transaction-state-error (githack-error)
  ()
  (:documentation
   "Signaled when an operation on a GIT-TRANSACTION -- or the
:READ-ONLY/:READ-WRITE mode of the GIT-REPOSITORY it was opened
against -- requires a state (e.g. :ACTIVE, :READ-WRITE) other than
its actual current one."))

(define-condition invalid-argument-error (githack-error)
  ()
  (:documentation
   "Signaled when a public entry point is called with an argument of
the wrong type, shape, or value -- an unsupported atom type, invalid
array dimensions, an out-of-range index or subscript, an
unnameable hash-table TEST function, and so on -- as opposed to any
problem with previously-persisted Git data (see
MALFORMED-GIT-OBJECT-ERROR) or an object's own persistence state
(see UNPERSISTED-OBJECT-ERROR)."))

(define-condition distributed-transaction-error (githack-error)
  ()
  (:documentation
   "Signaled by WITH-GITHACK-TRANSACTION/CALL-WITH-GITHACK-
TRANSACTION's own Two-Phase-Commit machinery (distributed-
transaction.lisp) when a distributed, multi-repository transaction's
own Phase 1 (\"Prepare\") fails against some participating
repository (e.g. `git mktag` rejects a malformed tag, or a
`refs/githack/prepare/<tx-id>/<branch-name>` ref already
unexpectedly exists) -- as opposed to CONCURRENT-MODIFICATION-ERROR,
which means a single repository's own ordinary branch
compare-and-swap failed. Also signaled by RUN-GITHACK-EXORCIST if a
stranded PREPARE ref's own annotated tag cannot be parsed back into
a Transaction Manifest, or names a Ledger repository that cannot
itself be reached."))

(define-condition branch-not-found-error (githack-error)
  ((repository :initarg :repository :reader get-repository)
   (name :initarg :name :reader get-name))
  (:report
   (lambda (condition stream)
     (format stream "No branch named ~S in repository ~A."
             (get-name condition) (get-repository condition))))
  (:documentation
   "Signaled by RESOLVE-BRANCH when no branch named NAME exists in
REPOSITORY and :IF-DOES-NOT-EXIST is :ERROR (the default)."))

(define-condition merge-conflict-error (githack-error)
  ((repository :initarg :repository :reader get-repository)
   (name :initarg :name :reader get-name)
   (base-sha :initarg :base-sha :reader get-base-sha)
   (candidate-sha :initarg :candidate-sha :reader get-candidate-sha)
   (current-head-sha :initarg :current-head-sha :reader get-current-head-sha)
   (detail :initarg :detail :initform nil :reader get-detail))
  (:report
   (lambda (condition stream)
     (format stream "Rebase merge conflict on branch ~S in ~A: could not cleanly replay commit ~A (started from ~A) onto the concurrently-advanced HEAD ~A.~@[~%~A~]"
             (get-name condition)
             (get-repository condition)
             (get-candidate-sha condition)
             (get-base-sha condition)
             (get-current-head-sha condition)
             (get-detail condition))))
  (:documentation
   "Signaled by CALL-WITH-GIT-TRANSACTION's (and, transitively,
CALL-WITH-TRANSACTION's) :REBASE CONFLICT-RESOLUTION strategy when
GIT-MERGE-TREE finds a genuine, unresolvable content conflict while
attempting to replay a transaction's own already-computed candidate
commit onto a branch HEAD some other writer has concurrently
advanced, and :REBASE-FALLBACK is :ERROR (rather than :RETRY, which
instead signals CONCURRENT-MODIFICATION-ERROR to trigger a full
re-run of the transaction from scratch). Distinct from CONCURRENT-
MODIFICATION-ERROR: that condition means a race was merely detected
(Git's own ref compare-and-swap failed); this one means a race was
detected AND an attempt to resolve it via a real three-way content
merge itself failed, because the two writers touched the exact same
content."))

(setf (documentation 'get-base-sha 'function)
      "Return the SHA CONDITION (a MERGE-CONFLICT-ERROR) recorded as
the branch HEAD its transaction originally started from, before any
concurrent writer advanced it.")
(setf (documentation 'get-candidate-sha 'function)
      "Return the SHA of the already-persisted candidate GIT-COMMIT
CONDITION's (a MERGE-CONFLICT-ERROR) transaction computed and tried,
unsuccessfully, to replay onto a concurrently-advanced branch HEAD.")
(setf (documentation 'get-current-head-sha 'function)
      "Return the SHA of the branch HEAD CONDITION's (a
MERGE-CONFLICT-ERROR) transaction was attempting to replay its own
candidate commit onto when GIT-MERGE-TREE reported a genuine,
unresolvable content conflict.")

(define-condition git-not-found-error (githack-error)
  ()
  (:documentation
   "Signaled by %ENSURE-GIT-AVAILABLE (called by CALL-WITH-REPOSITORY)
when no working `git` executable can be found and run on PATH --
e.g. because `git` is not installed, PATH is misconfigured, or an
unusual shell environment prevents subprocess creation entirely --
so this fails fast with a clear diagnostic up front, rather than
some arbitrary later GIT-IO.LISP call failing deep in a stack with a
raw UIOP:SUBPROCESS-ERROR or OS-level ENOENT."))
