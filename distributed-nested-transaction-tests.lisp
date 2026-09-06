;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests exercising GitHack's distributed (multi-
;;; repository) Two-Phase-Commit layer (distributed-transaction.lisp)
;;; composed with its ordinary, single-repository nested-transaction
;;; support (GIT-TRANSACTION's own automatic nesting, exercised alone
;;; by END-TO-END-NESTED-TRANSACTIONS-COMMIT-ABORT-AND-RETRY in
;;; end-to-end-tests.lisp) -- i.e. real, non-mocked coverage of
;;; exactly the composition %ENLIST-TRANSACTION-WRITE!'s own
;;; docstring recommends: "nested GIT-TRANSACTIONs inside one single
;;; outer WITH-TRANSACTION/CALL-WITH-TRANSACTION call for that
;;; repository... reserve separate, top-level WITH-TRANSACTION calls
;;; under one WITH-GITHACK-TRANSACTION for genuinely distinct
;;; repositories."
;;;
;;; Every test here genuinely shells out to real `git` executables
;;; against real, temporary bare repositories (via
;;; WITH-TEMPORARY-GIT-REPOSITORY, exactly as DISTRIBUTED-
;;; TRANSACTION-SUITE and END-TO-END-SUITE both do) -- no
;;; GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/
;;; GIT-UPDATE-REF fake is ever installed.

(def-suite distributed-nested-transaction-suite
  :in githack-suite
  :description "End-to-end tests exercising GitHack's distributed Two-Phase-Commit layer composed with its own ordinary, single-repository nested-transaction support, against real temporary bare Git repositories.")

(in-suite distributed-nested-transaction-suite)

(defun %dntx-current-value ()
  "Return *TRANSACTION*'s own current, fully-percolated root value
as a plain Lisp value -- shorthand for (GET-PAYLOAD (GET-CURRENT-ROOT
*TRANSACTION*)), used throughout this file exactly as END-TO-END-
NESTED-TRANSACTIONS-COMMIT-ABORT-AND-RETRY (end-to-end-tests.lisp)
uses it inline, to observe a nested commit's or abort's effect on its
immediately enclosing transaction without yet returning from it."
  (get-payload (get-current-root *transaction*)))

(test nested-git-transactions-compose-correctly-within-one-participant-of-a-distributed-transaction
  "One participant (REPOSITORY-1) of a two-repository distributed
transaction internally composes an ordinary nested-transaction commit
(+10) and an ordinary nested-transaction explicit abort (+1000,
discarded), exactly as a single-repository transaction would on its
own -- while REPOSITORY-2, the distributed transaction's other
participant, is written to directly, with no nesting at all. Confirms
both that nested single-repository composition still works correctly
underneath a distributed transaction's own deferred-commit machinery,
and that BOTH participants still end up committed together via one
real 2PC round trip, with no `refs/githack/prepare/...` ref left
stranded in either repository afterward."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (%dtx-write! repository-1 "main" 100)
      (with-githack-transaction ()
        (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx" :mode :read-write)
          (with-transaction (v) (repo1 :read-write)
            (is (eql 100 v))
            ;; Nested A: +10, committed normally (percolates up, no
            ;; GIT-COMMIT of its own).
            (with-transaction (a) (repo1 :read-write) (+ a 10))
            (is (eql 110 (%dntx-current-value)))
            ;; Nested B: attempts +1000 but explicitly aborts instead
            ;; of returning -- must leave the percolated state
            ;; completely untouched (still 110, not 1110).
            (with-transaction (b) (repo1 :read-write) (abort-git-transaction *transaction*))
            (is (eql 110 (%dntx-current-value)))
            (%dntx-current-value)))
        (%dtx-write! repository-2 "main" "solo-participant"))
      (is (eql 110 (%dtx-read repository-1 "main")))
      (is (equal "solo-participant" (%dtx-read repository-2 "main")))
      (is (null (%git-for-each-ref repository-1 "refs/githack/prepare/")))
      (is (null (%git-for-each-ref repository-2 "refs/githack/prepare/"))))))

(test two-level-nested-transactions-percolate-through-a-distributed-participant
  "Mirrors END-TO-END-NESTED-TRANSACTIONS-COMMIT-ABORT-AND-RETRY's own
two-level (nested-of-nested) C/D percolation case, but with C itself
running as one participant of a three-repository distributed
transaction: nested transaction D (inside nested transaction C,
itself inside REPOSITORY-1's own outer transaction) commits +5; C
then reads its own now-percolated root back out and returns it,
carrying D's contribution up two levels into REPOSITORY-1's outer
transaction, which in turn is one of three participants 2PC commits
together."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-temporary-git-repository (repository-3)
        (%dtx-write! repository-1 "main" 100)
        (with-githack-transaction ()
          (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                   :message "dntx" :mode :read-write)
            (with-transaction (v) (repo1 :read-write)
              (is (eql 100 v))
              (with-transaction (c) (repo1 :read-write)
                (is (eql 100 c))
                (with-transaction (d) (repo1 :read-write)
                  (is (eql 100 d))
                  (+ d 5))
                (%dntx-current-value))
              (is (eql 105 (%dntx-current-value)))
              (%dntx-current-value)))
          (%dtx-write! repository-2 "main" "participant-two")
          (%dtx-write! repository-3 "main" "participant-three"))
        (is (eql 105 (%dtx-read repository-1 "main")))
        (is (equal "participant-two" (%dtx-read repository-2 "main")))
        (is (equal "participant-three" (%dtx-read repository-3 "main")))))))

(test independent-nested-single-repository-composition-in-two-separate-participants-at-once
  "Both participants of a two-repository distributed transaction
independently use ordinary nested-transaction composition of their
own (REPOSITORY-1 nests a +10 commit; REPOSITORY-2 nests a *3
commit), confirming nested-transaction percolation is entirely local
to each participant's own outer transaction and does not interfere
with a sibling participant's own nested composition, before both are
committed together via one real 2PC round trip."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (%dtx-write! repository-1 "main" 10)
      (%dtx-write! repository-2 "main" 7)
      (with-githack-transaction ()
        (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx" :mode :read-write)
          (with-transaction (v) (repo1 :read-write)
            (with-transaction (a) (repo1 :read-write) (+ a 10))
            (%dntx-current-value)))
        (with-repository (repo2) (repository-2 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx" :mode :read-write)
          (with-transaction (v) (repo2 :read-write)
            (with-transaction (b) (repo2 :read-write) (* b 3))
            (%dntx-current-value))))
      (is (eql 20 (%dtx-read repository-1 "main")))
      (is (eql 21 (%dtx-read repository-2 "main"))))))

(test nested-transaction-abort-in-one-participant-does-not-prevent-the-distributed-transaction-from-committing
  "An explicit ABORT-GIT-TRANSACTION on a nested transaction inside
REPOSITORY-1 discards only that nested contribution -- it is not
itself an error, so it neither aborts REPOSITORY-1's own outer
transaction nor the enclosing distributed transaction as a whole:
both REPOSITORY-1 and REPOSITORY-2 still end up committed via one
successful 2PC round trip."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (%dtx-write! repository-1 "main" "unchanged")
      (with-githack-transaction ()
        (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx" :mode :read-write)
          (with-transaction (v) (repo1 :read-write)
            (with-transaction (a) (repo1 :read-write)
              (declare (ignore a))
              (abort-git-transaction *transaction*))
            v))
        (%dtx-write! repository-2 "main" "committed-fine"))
      (is (equal "unchanged" (%dtx-read repository-1 "main")))
      (is (equal "committed-fine" (%dtx-read repository-2 "main")))
      (is (null (%git-for-each-ref repository-1 "refs/githack/prepare/")))
      (is (null (%git-for-each-ref repository-2 "refs/githack/prepare/"))))))

(test genuine-error-inside-a-nested-transaction-aborts-the-entire-distributed-transaction
  "Unlike an explicit ABORT-GIT-TRANSACTION (see the previous test),
a genuine error signaled inside a nested transaction unwinds not just
that nested transaction and its own enclosing REPOSITORY-1 outer
transaction, but the entire enclosing WITH-GITHACK-TRANSACTION as
well: %FINISH-GITHACK-TRANSACTION! is never reached, so NEITHER
participant is committed, even though REPOSITORY-2 had already been
written to (and successfully enlisted) before the error."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (signals simple-error
        (with-githack-transaction ()
          (%dtx-write! repository-2 "main" "should-not-stick")
          (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                   :message "dntx" :mode :read-write)
            (with-transaction (v) (repo1 :read-write)
              (declare (ignore v))
              (with-transaction (a) (repo1 :read-write)
                (declare (ignore a))
                (error "simulated failure inside a nested transaction"))
              100))))
      (is (null (git-show-ref-sha repository-1 "main")))
      (is (null (git-show-ref-sha repository-2 "main")))
      (is (null (%git-for-each-ref repository-1 "refs/githack/")))
      (is (null (%git-for-each-ref repository-2 "refs/githack/"))))))

(test two-sequential-top-level-writes-to-the-same-participant-coalesce-to-a-single-pending-write
  "Two separate, sequential (NOT nested -- each its own top-level
WITH-REPOSITORY/WITH-TRANSACTION call) writes against the very same
REPOSITORY-1/\"main\" within one distributed transaction coalesce, per
%ENLIST-TRANSACTION-WRITE!'s own documented simplification, into a
single PENDING-WRITE: only the LATEST NEW-COMMIT-SHA survives, but
OLD-SHA is preserved from the very FIRST write's own original branch
head (not the first write's own -- never ref-visible -- new SHA), so
the eventual 2PC/Fast-Path commit's own compare-and-swap is still
checked against the branch's real, original state. Also confirms the
second call's own BODY does not observe the first call's in-flight
write (it resolves its own starting value from the branch's real,
unmoved ref, so it sees the ORIGINAL value, not the first call's
result)."
  (with-temporary-git-repository (repository-1)
    (%dtx-write! repository-1 "main" "original")
    (let ((original-sha (git-show-ref-sha repository-1 "main")))
      (with-githack-transaction ()
        (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx-first" :mode :read-write)
          (with-transaction (v) (repo1 :read-write)
            (is (equal "original" v))
            "first-write"))
        (with-repository (repo1) (repository-1 :branch "main" :author +dtx-author+ :committer +dtx-author+
                                                 :message "dntx-second" :mode :read-write)
          (with-transaction (v) (repo1 :read-write)
            ;; Must still see the ORIGINAL value: the first call's
            ;; own write was never made ref-visible.
            (is (equal "original" v))
            "second-write"))
        (let ((pending (%githack-transaction-pending-writes *current-transaction*)))
          (is (= 1 (length pending)))
          (is (equal original-sha (pending-write-old-sha (first pending))))))
      (is (equal "second-write" (%dtx-read repository-1 "main"))))))

(test nested-githack-transaction-commits-independently-of-its-own-enclosing-transactions-later-outcome
  "A WITH-GITHACK-TRANSACTION nested directly inside another WITH-
GITHACK-TRANSACTION's own body is its own, wholly independent
distributed transaction -- with its own freshly generated TX-ID, and
its own %FINISH-GITHACK-TRANSACTION! call that runs (and, for its own
two participants, drives a full nested 2PC round trip to completion)
entirely within the inner LET's own dynamic extent, before control
ever returns to the outer transaction's body. Demonstrates the
resulting, documented consequence: if the OUTER transaction's own
body later signals an error, the already-fully-committed INNER
transaction is NOT rolled back -- only the outer's own separate
participant (written to after the inner transaction already
returned) is left uncommitted."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-temporary-git-repository (repository-3)
        (signals simple-error
          (with-githack-transaction ()
            ;; Inner, nested distributed transaction: its own
            ;; complete 2PC round trip across repository-1/
            ;; repository-2 runs to completion right here.
            (with-githack-transaction ()
              (%dtx-write! repository-1 "main" "inner-one")
              (%dtx-write! repository-2 "main" "inner-two"))
            ;; Back in the OUTER transaction: write to a third,
            ;; disjoint repository, then blow up before the outer's
            ;; own %FINISH-GITHACK-TRANSACTION! is ever reached.
            (%dtx-write! repository-3 "main" "outer-should-not-stick")
            (error "simulated failure in the outer transaction, after the inner one already committed")))
        ;; The inner transaction's own two participants are
        ;; permanently committed, regardless of the outer's own fate.
        (is (equal "inner-one" (%dtx-read repository-1 "main")))
        (is (equal "inner-two" (%dtx-read repository-2 "main")))
        ;; The outer transaction's own sole participant never committed.
        (is (null (git-show-ref-sha repository-3 "main")))
        (is (null (%git-for-each-ref repository-3 "refs/githack/")))))))
