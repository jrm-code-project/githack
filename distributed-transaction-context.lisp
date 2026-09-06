;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Early, dependency-free state for GitHack's distributed
;;; (multi-repository) Two-Phase-Commit transaction layer -- see
;;; WITH-GITHACK-TRANSACTION and friends in distributed-
;;; transaction.lisp, the very last file this system loads, for the
;;; actual 2PC protocol logic (Git plumbing, Phase 1/Phase 2, and
;;; crash recovery). This tiny file is split out on its own and
;;; loaded right after CONDITIONS, for the same reason CONDITIONS
;;; itself is loaded early: so every other file -- in particular
;;; PERSISTENT-STANDARD-CLASS.LISP's write-side slot hook, and
;;; GIT-TRANSACTION.LISP's own single-repository commit logic -- can
;;; refer to *CURRENT-TRANSACTION* and enlist a participating
;;; repository without introducing a load-order cycle with the much
;;; heavier distributed-transaction.lisp file, which itself depends
;;; on GIT-IO/GIT-BRANCH/GIT-REPOSITORY/GIT-TRANSACTION.

(defvar *current-transaction* nil
  "The GITHACK-TRANSACTION (a distributed, potentially
multi-repository Two-Phase-Commit transaction) currently in dynamic
scope, bound by WITH-GITHACK-TRANSACTION/CALL-WITH-GITHACK-
TRANSACTION for the duration of its body, or NIL if no distributed
transaction is currently active (the ordinary, single-repository
case, which is by far the common one). Unlike *GIT-TRANSACTION*
(which is left UNBOUND, not NIL, outside its own dynamic extent, and
only ever tracks a single repository's own transaction),
*CURRENT-TRANSACTION* defaults to NIL at the top level precisely so
every ordinary, non-distributed GIT-TRANSACTION/TRANSACTION commit
can cheaply check it with a simple boolean test with no BOUNDP
dance required.")

(defstruct (pending-write
            (:constructor %make-pending-write (git-repository branch-name old-sha new-commit-sha)))
  "One participating repository's own prepared-but-not-yet-ref-
visible write within a GITHACK-TRANSACTION: the GIT-REPOSITORY it
belongs to, its BRANCH-NAME, the branch's OLD-SHA (its SHA when this
distributed transaction's first write to this repository/branch
began, or NIL if the branch did not exist yet -- see
GIT-TRANSACTION's own EXPECTED-BRANCH-SHA), and the latest
NEW-COMMIT-SHA %COMMIT-GIT-TRANSACTION-NOW computed for it (already
persisted to Git's object database, but not yet reachable from any
ref). If the same (repository . branch) is written to more than once
while one GITHACK-TRANSACTION is active, only the single latest
NEW-COMMIT-SHA survives -- OLD-SHA is NOT updated, so the final 2PC
commit or fast-path update is still checked against the branch's
*original* SHA. See %ENLIST-TRANSACTION-WRITE! in git-
transaction.lisp: a distributed transaction is scoped, for
correctness, to at most one committed write per (repository .
branch) -- see its docstring for the reasoning. PREPARE-REF is
filled in only once the 2PC path's own Phase 1 has created this
participant's own `refs/githack/prepare/<tx-id>/<branch-name>` ref;
NIL beforehand, and for the single-repository fast path, which never
needs one at all."
  git-repository
  branch-name
  old-sha
  new-commit-sha
  (prepare-ref nil))

(defstruct (githack-transaction
            (:constructor %make-githack-transaction (tx-id))
            (:conc-name %githack-transaction-))
  "A distributed, potentially multi-repository transaction context;
see WITH-GITHACK-TRANSACTION/CALL-WITH-GITHACK-TRANSACTION.

TX-ID is a freshly generated, globally-unique hexadecimal string
identifying this transaction, used to namespace its own
`refs/githack/prepare/<tx-id>/<branch-name>` and
`refs/githack/ledger/<tx-id>` refs so concurrent, unrelated
distributed transactions never collide with one another.

PENDING-WRITES is a list of PENDING-WRITE, one per distinct
\(repository . branch) that any GIT-TRANSACTION has committed
against while this distributed transaction was bound to
*CURRENT-TRANSACTION*, in the order first encountered. This is
GitHack's actual PARTICIPATING-REPOS bookkeeping (per the
architecture spec's terminology) -- it is what
%FINISH-GITHACK-TRANSACTION! (distributed-transaction.lisp) consults
to decide between the 0/1/>1-repository no-op/fast-path/2PC paths,
and it is populated by GIT-TRANSACTION.LISP's own
%COMMIT-GIT-TRANSACTION-NOW, the single choke point every ordinary
\(non-:REBASE) GIT-TRANSACTION commit -- and so, transitively, every
PERSISTENT-OBJECT/PERSISTENT-VECTOR/PERSISTENT-HASH-TABLE mutation's
eventual commit -- already passes through.

TOUCHED-PATHNAMES is a supplementary, purely informational list of
bare repository pathnames observed by PERSISTENT-STANDARD-CLASS's
own write-side SLOT-VALUE-USING-CLASS hook: every PERSISTENT-OBJECT
slot write (whether from ordinary construction or explicit mutation)
while this transaction is bound notes its own backing repository
pathname here, for introspection/debugging. This list plays NO role
in deciding 2PC participation, since a bare pathname alone (unlike a
full GIT-REPOSITORY) carries no branch/author/committer/message to
actually commit with -- PENDING-WRITES, above, is the only
authoritative bookkeeping 2PC itself relies on."
  tx-id
  (pending-writes '())
  (touched-pathnames '()))

(defun %generate-transaction-id ()
  "Return a freshly generated, unique 32-hexadecimal-digit
transaction identifier string, grouped like a UUID's canonical
textual form (8-4-4-4-12) purely for readability -- this is not a
validated RFC-4122 version-4 UUID (no version/variant bits are
forced), just a very large random number, which is all
GITHACK-TRANSACTION's own TX-ID actually needs: global uniqueness
(for namespacing `refs/githack/prepare/<tx-id>/...` and
`refs/githack/ledger/<tx-id>` refs so unrelated concurrent
distributed transactions never collide), not standards conformance.
SBCL/Quicklisp provide no UUID library this system already depends
on, so this mirrors %UNIQUE-TEMPORARY-PATHNAME's own
random-digits-in-some-radix technique (git-io.lisp), just with more
bits and grouped for readability."
  (let ((digits (format nil "~(~16,32,'0R~)" (random (expt 16 32) (make-random-state t)))))
    (format nil "~A-~A-~A-~A-~A"
            (subseq digits 0 8) (subseq digits 8 12) (subseq digits 12 16)
            (subseq digits 16 20) (subseq digits 20 32))))
