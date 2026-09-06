;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests for GitHack's distributed (multi-repository)
;;; Two-Phase-Commit transaction layer (distributed-transaction.lisp
;;; / distributed-transaction-context.lisp): WITH-GITHACK-
;;; TRANSACTION's own 0/1/>1-participant no-op/Fast-Path/2PC
;;; dispatch, Phase 1 (Prepare) failure/rollback, and RUN-GITHACK-
;;; EXORCIST's crash recovery for both the "already committed" and
;;; "never committed" cases. Like END-TO-END-SUITE, every test here
;;; genuinely shells out to real `git` executables against real,
;;; temporary bare repositories (via WITH-TEMPORARY-GIT-REPOSITORY)
;;; -- no GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/
;;; GIT-UPDATE-REF fake is ever installed here, since the whole
;;; point of this feature is its use of genuine Git plumbing
;;; (`mktag`, `update-ref --stdin`, `for-each-ref`, `rev-parse`).

(def-suite distributed-transaction-suite
  :in githack-suite
  :description "End-to-end tests for GitHack's distributed (multi-repository) Two-Phase-Commit transaction layer, against real temporary bare Git repositories.")

(in-suite distributed-transaction-suite)

(defparameter +dtx-author+ "Distributed Test <dtx@githack.local>")

(defun %dtx-write! (repository branch value)
  "Open an ordinary, single-repository, real :READ-WRITE transaction
against REPOSITORY/BRANCH (both real, via WITH-REPOSITORY/WITH-
TRANSACTION) and commit VALUE as its new root. Whether this actually
advances BRANCH immediately, or is instead silently deferred as a
pending write against some enclosing WITH-GITHACK-TRANSACTION,
depends entirely on whether *CURRENT-TRANSACTION* happens to be
bound at the time -- exactly the transparency this whole feature is
about."
  (with-repository (repo) (repository :branch branch :author +dtx-author+ :committer +dtx-author+
                                       :message "dtx" :mode :read-write)
    (with-transaction (v) (repo :read-write)
      (declare (ignore v))
      value)))

(defun %dtx-read (repository branch)
  "Return the current plain Lisp value of REPOSITORY/BRANCH's own
root, via a real, read-only WITH-REPOSITORY/WITH-TRANSACTION round
trip, or NIL if BRANCH does not exist yet."
  (let ((result nil))
    (with-repository (repo) (repository :branch branch :mode :read-only)
      (with-transaction (v) (repo :read-only)
        (setf result v)
        v))
    result))

(test zero-participant-githack-transaction-is-a-no-op
  "A WITH-GITHACK-TRANSACTION body that opens no ordinary
transaction against any repository at all commits nothing and
signals nothing."
  (with-temporary-git-repository (repository)
    (finishes
      (with-githack-transaction ()
        (+ 1 2)))
    (is (null (git-show-ref-sha repository "main")))))

(test one-participant-githack-transaction-takes-the-fast-path
  "A WITH-GITHACK-TRANSACTION body that writes to exactly one
repository/branch commits it directly via the Fast Path (no Ledger,
no prepare/ledger refs of any kind), and the branch ends up holding
the expected value."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (%dtx-write! repository "main" "solo-value"))
    (is (equal "solo-value" (%dtx-read repository "main")))
    (is (null (%git-for-each-ref repository "refs/githack/")))))

(test two-participant-githack-transaction-commits-both-via-2pc
  "A WITH-GITHACK-TRANSACTION body that writes to two distinct
repositories drives the full Two-Phase-Commit protocol: both
branches end up advanced to their own expected values, and no
`refs/githack/prepare/...` tracking ref is left stranded in either
repository afterward (the Ledger's own `refs/githack/ledger/<tx-id>`
ref, by contrast, is permanent -- see its own docstring -- and so is
expected to remain)."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-githack-transaction ()
        (%dtx-write! repository-1 "main" "value-one")
        (%dtx-write! repository-2 "main" "value-two"))
      (is (equal "value-one" (%dtx-read repository-1 "main")))
      (is (equal "value-two" (%dtx-read repository-2 "main")))
      (is (null (remove-if (lambda (entry) (search "refs/githack/ledger/" (third entry)))
                            (%git-for-each-ref repository-1 "refs/githack/"))))
      (is (null (%git-for-each-ref repository-2 "refs/githack/"))))))

(test three-participant-githack-transaction-commits-all-three-via-2pc
  "A WITH-GITHACK-TRANSACTION body spanning three distinct
repositories advances all three branches, exercising Phase 1/Phase 2
across more than the minimal two-participant case."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-temporary-git-repository (repository-3)
        (with-githack-transaction ()
          (%dtx-write! repository-1 "main" "one")
          (%dtx-write! repository-2 "main" "two")
          (%dtx-write! repository-3 "main" "three"))
        (is (equal "one" (%dtx-read repository-1 "main")))
        (is (equal "two" (%dtx-read repository-2 "main")))
        (is (equal "three" (%dtx-read repository-3 "main")))))))

(test githack-transaction-error-in-body-leaves-every-participant-untouched
  "If a WITH-GITHACK-TRANSACTION body signals an error after already
writing to two distinct repositories, neither repository's branch is
ever advanced (nothing reaches Phase 1 at all -- %FINISH-GITHACK-
TRANSACTION! is simply never called), and the error propagates to
the caller unchanged."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (signals simple-error
        (with-githack-transaction ()
          (%dtx-write! repository-1 "main" "should-not-stick-1")
          (%dtx-write! repository-2 "main" "should-not-stick-2")
          (error "simulated failure in body")))
      (is (null (git-show-ref-sha repository-1 "main")))
      (is (null (git-show-ref-sha repository-2 "main")))
      (is (null (%git-for-each-ref repository-1 "refs/githack/")))
      (is (null (%git-for-each-ref repository-2 "refs/githack/"))))))

(test githack-transaction-second-two-phase-commit-does-not-collide-with-the-first
  "Two entirely separate, sequential WITH-GITHACK-TRANSACTIONs, each
spanning the same two repositories, both succeed and each leaves the
branches at its own final value -- confirms TX-ID-namespaced
prepare/ledger refs from the first transaction never interfere with
the second."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-githack-transaction ()
        (%dtx-write! repository-1 "main" "first-round-1")
        (%dtx-write! repository-2 "main" "first-round-2"))
      (with-githack-transaction ()
        (%dtx-write! repository-1 "main" "second-round-1")
        (%dtx-write! repository-2 "main" "second-round-2"))
      (is (equal "second-round-1" (%dtx-read repository-1 "main")))
      (is (equal "second-round-2" (%dtx-read repository-2 "main"))))))

(test phase-1-prepare-failure-rolls-back-already-prepared-participants
  "If Phase 1's own Prepare step fails against a later participant
(simulated here by a stranded, colliding prepare ref already
occupying that participant's own `refs/githack/prepare/<tx-id>/
<branch-name>` path before Phase 1 even begins), DISTRIBUTED-
TRANSACTION-ERROR is signaled, the earlier participant's own already-
created prepare ref is deleted again (rolled back), and neither
branch is ever advanced."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (let ((tx-id (%generate-transaction-id)))
        ;; Pre-occupy repository-2's own prepare ref path for TX-ID with
        ;; a bogus blob, so %PREPARE-PARTICIPANT!'s own %GIT-RAW-UPDATE-REF
        ;; (which requires the ref not already exist) fails for it.
        (let ((bogus-sha (git-hash-object repository-2 "blob" (sb-ext:string-to-octets "bogus" :external-format :utf-8))))
          (%git-raw-update-ref repository-2 (%prepare-ref-path tx-id "main") bogus-sha :expected-sha nil))
        (let ((txn (%make-githack-transaction tx-id)))
          (let ((*current-transaction* txn))
            (%dtx-write! repository-1 "main" "will-be-rolled-back-1")
            (%dtx-write! repository-2 "main" "will-be-rolled-back-2"))
          (signals distributed-transaction-error
            (%finish-githack-transaction! txn)))
        ;; repository-1's own prepare ref (created before repository-2's
        ;; own Prepare step failed) must have been rolled back again.
        (is (null (remove-if-not (lambda (entry) (search "prepare" (third entry)))
                                  (%git-for-each-ref repository-1 "refs/githack/"))))
        (is (null (git-show-ref-sha repository-1 "main")))
        (is (null (git-show-ref-sha repository-2 "main")))))))

(test exorcist-rolls-back-a-stranded-prepare-ref-with-no-ledger
  "RUN-GITHACK-EXORCIST, run against a repository holding a stranded
`refs/githack/prepare/<tx-id>/<branch-name>` ref whose own Ledger
repository never received its `refs/githack/ledger/<tx-id>` marker
(simulating a crash between Phase 1 and Phase 2's own Point-of-No-
Return), simply deletes the stranded ref and leaves the branch
untouched."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (%generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (%dtx-write! repository "main" "never-committed"))
      (let* ((pw (first (%githack-transaction-pending-writes txn)))
             (manifest-text (%format-transaction-manifest
                              (%build-transaction-manifest tx-id (list pw) (pending-write-git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text))
      ;; Crash simulated here: the Ledger ref (in this same repository,
      ;; since it is the sole, and so its own elected, participant) is
      ;; never written.
      (let ((results (run-githack-exorcist repository)))
        (is (equal (list (list tx-id "main" :rolled-back)) results)))
      (is (null (git-show-ref-sha repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/"))))))

(test exorcist-rolls-forward-a-stranded-prepare-ref-with-a-written-ledger
  "RUN-GITHACK-EXORCIST, run against a repository holding a stranded
`refs/githack/prepare/<tx-id>/<branch-name>` ref whose own Ledger
repository DID already receive its `refs/githack/ledger/<tx-id>`
marker (simulating a crash after Phase 2's own Point-of-No-Return
but before its roll-forward loop reached this participant), fast-
forwards the branch to the prepared commit and deletes the stranded
ref."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (%generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (%dtx-write! repository "main" "should-be-committed"))
      (let* ((pw (first (%githack-transaction-pending-writes txn)))
             (manifest-text (%format-transaction-manifest
                              (%build-transaction-manifest tx-id (list pw) (pending-write-git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text)
        ;; Point of no return reached, then crash simulated before roll-forward.
        (%write-ledger-commit-point! (pending-write-git-repository pw) tx-id))
      (let ((results (run-githack-exorcist repository)))
        (is (equal (list (list tx-id "main" :committed)) results)))
      (is (equal "should-be-committed" (%dtx-read repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/"))))))

(test exorcist-is-a-no-op-for-a-repository-with-no-stranded-refs
  "RUN-GITHACK-EXORCIST against a repository with no stranded
`refs/githack/prepare/...` refs at all (the overwhelmingly common
case) returns the empty list and touches nothing."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (%dtx-write! repository "main" "ordinary-value"))
    (is (null (run-githack-exorcist repository)))
    (is (equal "ordinary-value" (%dtx-read repository "main")))))

(test exorcist-is-idempotent-once-a-stranded-ref-is-already-resolved
  "Calling RUN-GITHACK-EXORCIST a second time, immediately after it
already resolved a stranded ref, finds nothing left to do."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (%generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (%dtx-write! repository "main" "resolved-once"))
      (let* ((pw (first (%githack-transaction-pending-writes txn)))
             (manifest-text (%format-transaction-manifest
                              (%build-transaction-manifest tx-id (list pw) (pending-write-git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text)
        (%write-ledger-commit-point! (pending-write-git-repository pw) tx-id))
      (run-githack-exorcist repository)
      (is (null (run-githack-exorcist repository))))))
