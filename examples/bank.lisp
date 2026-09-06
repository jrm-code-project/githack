;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; GITHACK-EXAMPLE-BANK: a second self-hosting example application,
;;; alongside GITHACK-EXAMPLE-LIBRARY, demonstrating two ADVANCED
;;; GitHack features that a single flat CRUD example (like the
;;; library) never needs to exercise:
;;;
;;;   1. NESTED TRANSACTIONS -- TRANSFER composes two independent,
;;;      already-transactional operations (WITHDRAW and DEPOSIT) into
;;;      one atomic outer commit, simply by calling them from within
;;;      an enclosing WITH-TRANSACTION. Neither WITHDRAW nor DEPOSIT
;;;      needs to know it is being nested: GitHack detects, via the
;;;      dynamically bound *TRANSACTION*/*GIT-TRANSACTION*, that a
;;;      transaction is already active against this repository, and
;;;      silently turns each inner WITH-TRANSACTION into a NESTED
;;;      transaction instead of an independent top-level commit --
;;;      see GIT-TRANSACTION.LISP's CALL-WITH-GIT-TRANSACTION
;;;      docstring, "NESTING:" paragraph. A nested transaction never
;;;      itself creates a real Git commit or advances any branch; it
;;;      only merges its own final state back up into its immediate
;;;      parent's in-flight state (GET-CURRENT-ROOT), and only the
;;;      single outermost transaction in the chain -- here, TRANSFER's
;;;      own WITH-TRANSACTION -- ever creates the one real commit that
;;;      makes any of it durable. If WITHDRAW signals an error (e.g.
;;;      insufficient funds), the Lisp stack unwinds out of both
;;;      nested transactions and the outer one: nothing is committed,
;;;      not even DEPOSIT's own half of the transfer, and any Blobs/
;;;      Trees written along the way are simply orphaned Git garbage
;;;      for `git gc` to reclaim later -- no manual rollback code
;;;      anywhere in this file.
;;;
;;;   2. ADVANCED CONCURRENCY HANDLING -- every write here accepts an
;;;      explicit :CONFLICT-RESOLUTION (and, for :REBASE, a
;;;      :REBASE-FALLBACK) keyword, so callers can pick the strategy
;;;      that fits their own contention pattern instead of always
;;;      taking the default :RETRY:
;;;        * :RETRY (the default here, as in the library example) is
;;;          fine when transactions are cheap to recompute and races
;;;          are rare.
;;;        * :REBASE is the better choice under WORKLOADS-INDUCE-
;;;          real concurrent traffic: because each ACCOUNT lives in
;;;          its own bucket/subtree of the BANK's PERSISTENT-HASH-
;;;          TABLE catalog, two concurrent transactions touching
;;;          DIFFERENT accounts touch different Git subtrees, so
;;;          `git merge-tree` can merge them cleanly without ever
;;;          discarding either transaction's own work -- see
;;;          CONCURRENCY-DEMO.LISP, which proves this with real
;;;          concurrent OS threads. Only truly conflicting concurrent
;;;          writes to the very SAME account ever fall back (via
;;;          :REBASE-FALLBACK) to :RETRY or :ERROR.
;;;        * :LOCK is appropriate for SETTLE-INTEREST, a batch
;;;          operation that touches every account in the bank at
;;;          once: true mutual exclusion is worth the throughput cost
;;;          of serializing against other :LOCK-mode transactions,
;;;          since a batch this wide would otherwise conflict with
;;;          almost any other concurrent writer.
;;;
;;; To run:
;;;   (asdf:load-system :githack/example)
;;;   (githack-example-bank::populate-example-bank)
;;; Then see examples/concurrency-demo.lisp for a runnable
;;; demonstration of all three CONFLICT-RESOLUTION strategies above
;;; racing real OS threads against this same branch.

(defpackage "GITHACK-EXAMPLE-BANK"
  (:use "COMMON-LISP")
  (:import-from "GITHACK"
                "DEFINE-PERSISTENT-STRUCT"
                "PERSISTENT-HASH-TABLE"
                "PHASH-MAKE"
                "PHASH-GET"
                "PHASH-PUT"
                "PHASH-MAP"
                "DESERIALIZE-PERSISTENT-OBJECT"
                "WITH-REPOSITORY"
                "WITH-TRANSACTION"
                "*TRANSACTION*"
                "GET-CURRENT-ROOT")
  (:export "GET-GITHACK-REPO-PATH"
           "ENSURE-BANK-INITIALIZED"
           "OPEN-ACCOUNT"
           "GET-BALANCE"
           "DEPOSIT"
           "WITHDRAW"
           "TRANSFER"
           "SETTLE-INTEREST"
           "LIST-ACCOUNTS"
           "POPULATE-EXAMPLE-BANK"
           "INSUFFICIENT-FUNDS-ERROR"
           "INSUFFICIENT-FUNDS-ERROR-ACCOUNT-ID"
           "INSUFFICIENT-FUNDS-ERROR-REQUESTED-AMOUNT"
           "INSUFFICIENT-FUNDS-ERROR-AVAILABLE-BALANCE"))

(in-package "GITHACK-EXAMPLE-BANK")

(defparameter +bank-branch+ "database-example-bank"
  "The orphan branch every GITHACK-EXAMPLE-BANK transaction reads
from and writes to. As with GITHACK-EXAMPLE-LIBRARY's
+LIBRARY-BRANCH+, this is deliberately never \"main\", so this
example's data can never share commit ancestry with GitHack's own
source history, nor with the library example's own
\"database-example\" branch -- each example gets its own independent
orphan timeline sharing only the underlying object database.")

(defparameter +bank-signature+ "GitHack Example Bank <bank@githack.local>"
  "The AUTHOR/COMMITTER signature every GITHACK-EXAMPLE-BANK
transaction commits under.")

(define-condition insufficient-funds-error (error)
  ((account-id :initarg :account-id :reader insufficient-funds-error-account-id)
   (requested-amount :initarg :requested-amount :reader insufficient-funds-error-requested-amount)
   (available-balance :initarg :available-balance :reader insufficient-funds-error-available-balance))
  (:report (lambda (condition stream)
             (format stream "Account ~A has insufficient funds: requested ~D cents, only ~D cents available."
                     (insufficient-funds-error-account-id condition)
                     (insufficient-funds-error-requested-amount condition)
                     (insufficient-funds-error-available-balance condition))))
  (:documentation
   "Signaled by WITHDRAW (and, transitively, by TRANSFER, whenever
its own nested WITHDRAW signals this) when ACCOUNT-ID's current
balance is smaller than REQUESTED-AMOUNT. Deliberately a plain
Lisp ERROR, not any GitHack condition: from GitHack's own
transaction machinery's point of view this is just an ordinary
error unwinding out of RECEIVER, which is all it needs to guarantee
nothing is committed -- see this file's own header comment."))

(define-persistent-struct account
  (id "")
  (owner "")
  (balance 0))

(define-persistent-struct bank
  (accounts nil))

(defun get-githack-repo-path ()
  "Return the pathname of GitHack's own `.git` directory, exactly as
GITHACK-EXAMPLE-LIBRARY::GET-GITHACK-REPO-PATH does -- see that
function's own docstring for the full rationale."
  (merge-pathnames ".git/" (asdf:system-source-directory :githack)))

(defun %resolve-persistent-object (value)
  "Return a live, fully-typed CLOS instance for VALUE, exactly as
GITHACK-EXAMPLE-LIBRARY::%RESOLVE-PERSISTENT-OBJECT does -- see that
function's own docstring for the full rationale."
  (and value (deserialize-persistent-object value)))

(defun ensure-bank-initialized ()
  "Idempotently ensure a BANK (with an empty ACCOUNTS catalog) exists
as the root object of +BANK-BRANCH+'s head commit, creating one --
via a genuine orphan root commit, since this branch will not yet
exist the very first time this is called -- if it does not already.
Uses :RETRY conflict resolution, so this is safe to call concurrently
from multiple threads/processes; does nothing but return the
existing BANK if one is already there."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +bank-branch+
                                  :author +bank-signature+
                                  :message "Initialize the example bank."
                                  :conflict-resolution :retry)
        (or (%resolve-persistent-object value)
            (make-instance 'bank
                           :repository repository-path
                           :accounts (phash-make :repository repository-path :test 'equal)))))))

(defun %get-bank (value account-id verb)
  "Shared helper: coerce VALUE (a WITH-TRANSACTION receiver's own raw
argument) into a live BANK instance, or signal an informative error
naming ACCOUNT-ID and VERB (a string describing the attempted
operation, e.g. \"deposit into\") if the bank has never been
initialized yet."
  (or (%resolve-persistent-object value)
      (error "Cannot ~A account ~S: the bank has not been initialized yet. Call ENSURE-BANK-INITIALIZED first."
             verb account-id)))

(defun %get-account (bank account-id verb)
  "Shared helper: return the live ACCOUNT named ACCOUNT-ID in BANK's
own ACCOUNTS catalog, or signal an informative error naming
ACCOUNT-ID and VERB if no such account exists."
  (multiple-value-bind (raw-account found?) (phash-get account-id (bank-accounts bank))
    (unless found?
      (error "Cannot ~A account ~S: no such account in the bank's catalog." verb account-id))
    (%resolve-persistent-object raw-account)))

(defun open-account (account-id owner-name initial-balance &key (conflict-resolution :retry))
  "Add a new ACCOUNT (ACCOUNT-ID/OWNER-NAME/INITIAL-BALANCE, an
integer number of cents) to the BANK's ACCOUNTS catalog on
+BANK-BRANCH+, as a single real Git transaction. Signals an error if
the bank has never been initialized (see ENSURE-BANK-INITIALIZED), or
if ACCOUNT-ID is already present. CONFLICT-RESOLUTION defaults to
:RETRY; see this file's own header comment for when :REBASE or :LOCK
might instead be more appropriate."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +bank-branch+
                                  :author +bank-signature+
                                  :message (format nil "Open account ~A for ~A with an initial balance of ~D cents."
                                                    account-id owner-name initial-balance)
                                  :conflict-resolution conflict-resolution)
        (let ((bank (%get-bank value account-id "open")))
          (multiple-value-bind (existing found?) (phash-get account-id (bank-accounts bank))
            (declare (ignore existing))
            (when found?
              (error "Cannot open account ~S: an account with that ID already exists." account-id)))
          (let* ((account (make-instance 'account
                                         :repository repository-path
                                         :id account-id
                                         :owner owner-name
                                         :balance initial-balance))
                 (accounts (phash-put account-id account (bank-accounts bank))))
            (make-instance 'bank :repository repository-path :accounts accounts)))))))

(defun get-balance (account-id)
  "Return the current BALANCE (an integer number of cents) of the
ACCOUNT named ACCOUNT-ID in +BANK-BRANCH+'s BANK catalog. Signals an
error if the bank has never been initialized, or if ACCOUNT-ID is not
present. Reads via a :READ-ONLY transaction, so this never writes a
commit -- calling it repeatedly, even concurrently with writers, is
always safe."
  (let ((repository-path (get-githack-repo-path))
        (balance nil))
    (with-repository (repository) (repository-path :mode :read-only)
      (with-transaction (value) (repository :read-only :branch +bank-branch+)
        (let* ((bank (%get-bank value account-id "read"))
               (account (%get-account bank account-id "read")))
          (setf balance (account-balance account)))
        value))
    balance))

(defun %adjust-balance (account-id delta message-verb &key (conflict-resolution :retry) (rebase-fallback :error))
  "Shared implementation of DEPOSIT/WITHDRAW: replace the ACCOUNT
named ACCOUNT-ID in +BANK-BRANCH+'s BANK catalog with an otherwise-
identical copy whose BALANCE has DELTA added to it (DELTA may be
negative, for a withdrawal), as a single Git transaction --
independent if called at the top level, or NESTED into whatever
transaction is already active in *TRANSACTION*/*GIT-TRANSACTION* if
called from within one (see TRANSFER, and this file's own header
comment). Signals INSUFFICIENT-FUNDS-ERROR if the resulting balance
would be negative; signals a plain ERROR if the bank has never been
initialized, or if ACCOUNT-ID is not present. MESSAGE-VERB (\"deposit
into\"/\"withdraw from\") only affects the commit message an
outermost caller sees -- a nested call's own MESSAGE is always
ignored in favor of its parent's, per GitHack's own nesting rules."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +bank-branch+
                                  :author +bank-signature+
                                  :message (format nil "~:(~A~) ~D cents ~A account ~A."
                                                    message-verb (abs delta) message-verb account-id)
                                  :conflict-resolution conflict-resolution
                                  :rebase-fallback rebase-fallback)
        (let* ((bank (%get-bank value account-id message-verb))
               (account (%get-account bank account-id message-verb))
               (new-balance (+ (account-balance account) delta)))
          (when (minusp new-balance)
            (error 'insufficient-funds-error
                   :account-id account-id
                   :requested-amount (- delta)
                   :available-balance (account-balance account)))
          (let* ((updated-account (make-instance 'account
                                                 :repository repository-path
                                                 :id (account-id account)
                                                 :owner (account-owner account)
                                                 :balance new-balance))
                 (accounts (phash-put account-id updated-account (bank-accounts bank))))
            (make-instance 'bank :repository repository-path :accounts accounts)))))))

(defun deposit (account-id amount &key (conflict-resolution :retry) (rebase-fallback :error))
  "Add AMOUNT (a positive integer number of cents) to ACCOUNT-ID's
current balance on +BANK-BRANCH+, as a single Git transaction --
independent if called at the top level, or NESTED if called from
within another already-active transaction (e.g. from TRANSFER). See
%ADJUST-BALANCE for the full behavior, and this file's own header
comment for CONFLICT-RESOLUTION/REBASE-FALLBACK's advanced usage."
  (%adjust-balance account-id amount "deposit into" :conflict-resolution conflict-resolution :rebase-fallback rebase-fallback))

(defun withdraw (account-id amount &key (conflict-resolution :retry) (rebase-fallback :error))
  "Subtract AMOUNT (a positive integer number of cents) from
ACCOUNT-ID's current balance on +BANK-BRANCH+, as a single Git
transaction -- independent if called at the top level, or NESTED if
called from within another already-active transaction (e.g. from
TRANSFER). Signals INSUFFICIENT-FUNDS-ERROR, aborting the transaction
(and any enclosing one) without writing anything, if AMOUNT exceeds
the account's current balance. See %ADJUST-BALANCE for the full
behavior, and this file's own header comment for CONFLICT-
RESOLUTION/REBASE-FALLBACK's advanced usage."
  (%adjust-balance account-id (- amount) "withdraw from" :conflict-resolution conflict-resolution :rebase-fallback rebase-fallback))

(defun transfer (from-account-id to-account-id amount &key (conflict-resolution :retry) (rebase-fallback :error))
  "Atomically move AMOUNT (a positive integer number of cents) from
account FROM-ACCOUNT-ID to account TO-ACCOUNT-ID on +BANK-BRANCH+, as
a SINGLE real Git commit -- ADVANCED FEATURE: NESTED TRANSACTIONS.

This function's own body opens exactly one outermost WITH-TRANSACTION
and then simply calls WITHDRAW and DEPOSIT as ordinary functions.
Because each of those in turn opens its own WITH-TRANSACTION while
this outer one is already active in *TRANSACTION*/*GIT-TRANSACTION*,
GitHack automatically treats both as NESTED transactions instead of
independent top-level commits: neither one re-resolves +BANK-BRANCH+
against Git, neither one creates a real Git commit or advances the
branch, and each one's own final state is merged upward into this
outer transaction's own in-flight GET-CURRENT-ROOT the moment it
returns, in the order WITHDRAW-then-DEPOSIT -- so DEPOSIT already
sees WITHDRAW's own debit reflected in the BANK it reads, even though
neither has committed anything real yet. Only this outer WITH-
TRANSACTION, once both nested calls have returned normally, actually
persists the combined result and advances +BANK-BRANCH+ -- which is
why this function's own BODY ends by returning (GET-CURRENT-ROOT
*TRANSACTION*): that slot already holds the fully-merged BANK state
DEPOSIT's own nested transaction left behind, and using it directly
(rather than trying to recompute or re-fetch that state some other
way) is exactly what GitHack's nested-transaction machinery is for.

If WITHDRAW signals INSUFFICIENT-FUNDS-ERROR (or ACCOUNT-ID does not
exist, or the bank was never initialized), the Lisp stack unwinds out
of the nested WITHDRAW call, out of this function's own outer
WITH-TRANSACTION body, and out of this function entirely: DEPOSIT
never runs, nothing is committed, and any Blobs/Trees either nested
transaction already wrote to Git's object database are simply
orphaned garbage for `git gc` to reclaim -- there is no explicit
rollback code anywhere in this function, by design; see this file's
own header comment (\"Rollbacks and Garbage Collection\").

CONFLICT-RESOLUTION/REBASE-FALLBACK govern only this OUTER
transaction; the nested WITHDRAW/DEPOSIT calls always cascade those
same settings from their parent instead (per GitHack's own nesting
rules), so they need not be -- and, per GIT-TRANSACTION.LISP's own
NESTING semantics, cannot meaningfully be -- passed down separately."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +bank-branch+
                                  :author +bank-signature+
                                  :message (format nil "Transfer ~D cents from ~A to ~A." amount from-account-id to-account-id)
                                  :conflict-resolution conflict-resolution
                                  :rebase-fallback rebase-fallback)
        (declare (ignore value))
        (withdraw from-account-id amount)
        (deposit to-account-id amount)
        (get-current-root *transaction*)))))

(defun settle-interest (rate &key (conflict-resolution :lock))
  "Apply interest at RATE (e.g. 1/100 for one percent) to every
account currently in the BANK's catalog on +BANK-BRANCH+, as a SINGE
real Git commit -- ADVANCED FEATURE: :LOCK CONCURRENCY. Each
account's own interest is computed and applied via a NESTED DEPOSIT
call (see TRANSFER's own docstring for exactly how nesting composes
here), so the whole batch either commits together or, on any error,
is discarded together.

CONFLICT-RESOLUTION defaults to :LOCK rather than :RETRY here quite
deliberately: this operation touches every single account in the
bank at once, so under :RETRY or :REBASE it would conflict with
almost any other concurrent writer -- any single concurrent DEPOSIT/
WITHDRAW/TRANSFER anywhere in the bank touches a bucket this batch
also touches -- making progress unlikely under real contention.
:LOCK instead acquires this repository's exclusive OS-level lock (see
TRANSACTION-LOCK.LISP) up front, so this batch always runs to
completion in one pass, at the cost of blocking out any other
:LOCK-mode transaction against this same repository until it does;
see CONCURRENCY-DEMO.LISP for a demonstration with real concurrent OS
threads."
  (let ((repository-path (get-githack-repo-path))
        (account-ids '()))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +bank-branch+
                                  :author +bank-signature+
                                  :message (format nil "Settle interest at rate ~A across the entire bank." rate)
                                  :conflict-resolution conflict-resolution)
        (let ((bank (%get-bank value :all "settle interest across")))
          (phash-map (lambda (account-id raw-account)
                       (declare (ignore raw-account))
                       (push account-id account-ids))
                     (bank-accounts bank)))
        (dolist (account-id account-ids)
          (let ((interest (round (* rate (get-balance account-id)))))
            (when (plusp interest)
              (deposit account-id interest))))
        (get-current-root *transaction*)))))

(defun list-accounts ()
  "Return a fresh list of every ACCOUNT currently in +BANK-BRANCH+'s
BANK catalog, each a live, fully-typed ACCOUNT instance, in an
unspecified order. Returns NIL if the bank has never been initialized
or its catalog is empty. Reads via a :READ-ONLY transaction, so this
never writes a commit."
  (let ((repository-path (get-githack-repo-path))
        (accounts '()))
    (with-repository (repository) (repository-path :mode :read-only)
      (with-transaction (value) (repository :read-only :branch +bank-branch+)
        (let ((bank (%resolve-persistent-object value)))
          (when bank
            (phash-map (lambda (account-id raw-account)
                         (declare (ignore account-id))
                         (push (%resolve-persistent-object raw-account) accounts))
                       (bank-accounts bank))))
        value))
    accounts))

(defun populate-example-bank ()
  "Idempotently ensure the example bank exists and has three starter
accounts (\"alice\", \"bob\", \"carol\"), each opened only if not
already present. Convenient entry point for a first interactive run;
see CONCURRENCY-DEMO.LISP for a subsequent demonstration of nested
transactions and every CONFLICT-RESOLUTION strategy against this same
data."
  (ensure-bank-initialized)
  (dolist (spec '(("alice" "Alice Anderson" 10000)
                  ("bob" "Bob Brown" 5000)
                  ("carol" "Carol Chen" 7500)))
    (destructuring-bind (id owner initial-balance) spec
      (handler-case (open-account id owner initial-balance)
        (error () nil))))
  (list-accounts))
