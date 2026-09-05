;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; CALL-WITH-TRANSACTION is GitHack's user-facing transaction
;;; boundary, wrapping CALL-WITH-GIT-TRANSACTION so that application
;;; code never touches SHAs, GIT-BLOBs, GIT-TREEs, or GIT-COMMITs
;;; directly.
;;;
;;; The read phase (before RECEIVER runs) resolves the branch's
;;; current head commit -- via CALL-WITH-GIT-TRANSACTION itself --
;;; and reduces it to a single plain Lisp value: NIL if the branch
;;; does not exist yet (an empty repository awaiting its initial
;;; commit); the decoded Lisp atom, if RESOLVE-COMMIT-ROOT's logical
;;; root turns out to be a bare GIT-BLOB (whether or not it needed
;;; unwrapping from an ATOMIC-WRAPPER-TREE, which is invisible at
;;; this level); or otherwise the persistent proxy object itself (a
;;; GIT-TREE, PERSISTENT-CONS, or other persistent structure) for any
;;; compound root, unchanged.
;;;
;;; The write phase (after RECEIVER returns normally) takes RECEIVER's
;;; single plain Lisp value in turn and coerces it back into a
;;; GIT-OBJECT: unchanged, if it is already one (a persistent proxy
;;; RECEIVER simply mutated and returned), or else wrapped in a
;;; brand-new, unpersisted GIT-BLOB otherwise. That GIT-OBJECT is then
;;; handed back as the root to CALL-WITH-GIT-TRANSACTION, whose own
;;; auto-commit logic persists it (auto-wrapping a bare atom in an
;;; ATOMIC-WRAPPER-TREE as needed), creates the new commit, and
;;; advances the branch -- exactly as it would for any other caller.

;;; %ENSURE-BLOB-LOADED now lives in atomic-wrapper.lisp, alongside
;;; %ENSURE-TREE-ENTRIES-LOADED/%ENSURE-COMMIT-LOADED, so that
;;; PERSISTENT-VECTOR's own lazy accessor can share it too.

(defun %transaction-read-value (head-commit)
  "Return the plain Lisp value CALL-WITH-TRANSACTION's RECEIVER
should see for HEAD-COMMIT: NIL if HEAD-COMMIT is itself NIL (an
empty branch, awaiting its initial commit); the decoded atom held in
its root GIT-BLOB, if RESOLVE-COMMIT-ROOT's root is a bare atom
(loading it first via %ENSURE-BLOB-LOADED if necessary); or
otherwise RESOLVE-COMMIT-ROOT's root GIT-OBJECT itself (a GIT-TREE,
PERSISTENT-CONS, or other persistent proxy), unchanged."
  (and head-commit
       (let ((root (resolve-commit-root head-commit)))
         (if (typep root 'git-blob)
             (get-payload (%ensure-blob-loaded root))
             root))))

(defun %transaction-write-value (repository value)
  "Coerce VALUE -- the plain Lisp value CALL-WITH-TRANSACTION's
RECEIVER returned -- into a GIT-OBJECT suitable to hand back to
CALL-WITH-GIT-TRANSACTION as its new commit root: VALUE unchanged if
it is already a GIT-OBJECT (a GIT-TREE, PERSISTENT-CONS, or other
persistent proxy RECEIVER mutated and returned), or else a
brand-new, unpersisted GIT-BLOB wrapping VALUE as its PAYLOAD, for
any other atomic Lisp value (an INTEGER, STRING, SYMBOL, and so on;
see SERIALIZE-ATOM). REPOSITORY is the raw pathname to construct
that new GIT-BLOB against."
  (if (typep value 'git-object)
      value
      (make-instance 'git-blob :repository repository :payload value)))

(defun call-with-transaction (repository mode &key branch author committer message parents receiver)
  "The user-facing counterpart to CALL-WITH-GIT-TRANSACTION: opens a
transaction against REPOSITORY (a GIT-REPOSITORY) exactly as
CALL-WITH-GIT-TRANSACTION does, cascading BRANCH/AUTHOR/COMMITTER/
MESSAGE/PARENTS the same way, but invokes (FUNCALL RECEIVER VALUE)
with a single plain Lisp value in place of a raw GIT-COMMIT -- see
%TRANSACTION-READ-VALUE -- and expects RECEIVER to return a single
plain Lisp value in turn, representing the new desired root state,
which is automatically coerced back into a GIT-OBJECT and persisted
-- see %TRANSACTION-WRITE-VALUE. RECEIVER must never touch SHAs,
GIT-BLOBs, GIT-TREEs, or GIT-COMMITs directly.

As with CALL-WITH-GIT-TRANSACTION, RECEIVER may instead signal an
error to trigger an abnormal exit (nothing is written), and that is
honored exactly as it would be for an explicit :RECEIVER passed to
CALL-WITH-GIT-TRANSACTION directly. Callers needing COMMIT-GIT-
TRANSACTION/ABORT-GIT-TRANSACTION's finer explicit control should
use CALL-WITH-GIT-TRANSACTION directly instead of this wrapper.

Returns the GIT-TRANSACTION, exactly as CALL-WITH-GIT-TRANSACTION
does."
  (call-with-git-transaction
   repository mode
   :branch branch
   :author author
   :committer committer
   :message message
   :parents parents
   :receiver (lambda (transaction head-commit)
               (declare (ignore transaction))
               (%transaction-write-value
                (get-pathname repository)
                (funcall receiver (%transaction-read-value head-commit))))))

(defmacro with-transaction ((value-var) (repository mode &key branch author committer message parents) &body body)
  "Macro wrapper around CALL-WITH-TRANSACTION: expands into a call to
CALL-WITH-TRANSACTION on REPOSITORY and MODE (evaluated once each),
passing BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS through unchanged,
with :RECEIVER bound to a closure over BODY. Within BODY, VALUE-VAR
is bound to the plain Lisp value CALL-WITH-TRANSACTION's RECEIVER
would receive (NIL for an empty branch awaiting its initial commit).
BODY must return a single plain Lisp value representing the new
desired root state; see CALL-WITH-TRANSACTION."
  `(call-with-transaction ,repository ,mode
                           :branch ,branch
                           :author ,author
                           :committer ,committer
                           :message ,message
                           :parents ,parents
                           :receiver (lambda (,value-var) ,@body)))
