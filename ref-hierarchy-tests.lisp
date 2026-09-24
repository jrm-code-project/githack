;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; A branch name may contain "/", and Git stores that as nested
;;; directories under refs/heads/. "feature/foo" is therefore a real
;;; branch. "feature" and "feature/foo" cannot both exist: one needs
;;; a file where the other needs a directory. That refusal used to be
;;; reported as CONCURRENT-MODIFICATION-ERROR, which :RETRY and
;;; :REBASE catch and re-attempt with no limit. These tests pin both
;;; halves: a slash-bearing name round-trips, and a prefix collision
;;; is REF-HIERARCHY-CONFLICT-ERROR, including under :RETRY.

(def-suite ref-hierarchy-suite
  :in githack-suite
  :description "Slash-bearing branch names, and the file-versus-directory collision they can produce.")

(in-suite ref-hierarchy-suite)

(defparameter +hierarchy-author+ "Hierarchy Test <hierarchy@githack.local>")

(defun hierarchy-write! (repository branch value &key (conflict-resolution :error))
  "Commit VALUE as the root of BRANCH in REPOSITORY."
  (with-repository (repo) (repository :branch branch :author +hierarchy-author+
                                       :message "hierarchy" :mode :read-write)
    (with-transaction (v) (repo :read-write :conflict-resolution conflict-resolution)
      (declare (ignore v))
      value)))

(defun hierarchy-read (repository branch)
  "Return the plain root value of BRANCH in REPOSITORY, or NIL when
BRANCH does not exist yet."
  (let ((got nil))
    (with-repository (repo) (repository :branch branch :mode :read-only)
      (with-transaction (v) (repo :read-only)
        (setf got v)))
    got))

(test slash-branch-round-trips-and-accepts-a-second-commit
  "A branch whose name contains \"/\" is a normal branch: the value
committed to it reads back, a second commit advances that same
branch, and a sibling slash-name in the same repository (neither is
a path-prefix of the other) commits independently."
  (with-temporary-git-repository (repository)
    (hierarchy-write! repository "feature/foo" "first")
    (is (equal "first" (hierarchy-read repository "feature/foo")))
    (hierarchy-write! repository "feature/foo" "second")
    (is (equal "second" (hierarchy-read repository "feature/foo")))
    (hierarchy-write! repository "topic/bar" "sibling")
    (is (equal "sibling" (hierarchy-read repository "topic/bar")))
    (is (equal "second" (hierarchy-read repository "feature/foo")))))

(test slash-branch-fast-path-round-trips
  "The single-participant fast path (WITH-GITHACK-TRANSACTION around
one repository) advances a slash-bearing branch, including a second
commit whose compare-and-swap old value is that branch's real head."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (hierarchy-write! repository "feature/foo" "fast-1"))
    (with-githack-transaction ()
      (hierarchy-write! repository "feature/foo" "fast-2"))
    (is (equal "fast-2" (hierarchy-read repository "feature/foo")))))

(test sibling-slash-branches-commit-together-in-one-distributed-transaction
  "Two slash-bearing branches that share a directory but neither of
which is a prefix of the other (\"rel/1\" and \"rel/2\") can be
prepared and committed by one distributed transaction. Their prepare
refs are files inside one directory, which Git allows."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (hierarchy-write! repository "rel/1" "one")
      (hierarchy-write! repository "rel/2" "two"))
    (is (equal "one" (hierarchy-read repository "rel/1")))
    (is (equal "two" (hierarchy-read repository "rel/2")))
    (is (null (%git-for-each-ref repository "refs/githack/prepare/")))))

(test prefix-of-an-existing-slash-branch-signals-ref-hierarchy-conflict
  "Creating \"feature\" when \"feature/foo\" already exists signals
REF-HIERARCHY-CONFLICT-ERROR naming the longer ref, and leaves the
existing branch untouched. This is the directory-blocks-file
direction."
  (with-temporary-git-repository (repository)
    (hierarchy-write! repository "feature/foo" "kept")
    (let ((condition nil))
      (handler-case
          (hierarchy-write! repository "feature" "nope")
        (ref-hierarchy-conflict-error (c) (setf condition c)))
      (is (typep condition 'ref-hierarchy-conflict-error))
      (is (string= "feature" (get-name condition)))
      (is (string= "refs/heads/feature/foo" (get-blocking-ref condition))))
    (is (equal "kept" (hierarchy-read repository "feature/foo")))
    (is (null (git-show-ref-sha repository "feature")))))

(test slash-branch-under-an-existing-branch-signals-ref-hierarchy-conflict
  "Creating \"feature/foo\" when \"feature\" already exists signals
REF-HIERARCHY-CONFLICT-ERROR naming the shorter ref. This is the
file-blocks-directory direction."
  (with-temporary-git-repository (repository)
    (hierarchy-write! repository "feature" "kept")
    (let ((condition nil))
      (handler-case
          (hierarchy-write! repository "feature/foo" "nope")
        (ref-hierarchy-conflict-error (c) (setf condition c)))
      (is (typep condition 'ref-hierarchy-conflict-error))
      (is (string= "feature/foo" (get-name condition)))
      (is (string= "refs/heads/feature" (get-blocking-ref condition))))
    (is (equal "kept" (hierarchy-read repository "feature")))
    (is (null (git-show-ref-sha repository "feature/foo")))))

(test retry-and-rebase-propagate-a-ref-hierarchy-conflict
  ":RETRY and :REBASE must not re-attempt a hierarchy collision.
Each mode propagates REF-HIERARCHY-CONFLICT-ERROR on the first
attempt. A timeout stands in for the old infinite retry loop, so a
regression fails this test instead of hanging it."
  (dolist (mode '(:retry :rebase))
    (with-temporary-git-repository (repository)
      (hierarchy-write! repository "feature/foo" "kept")
      (let ((saw nil))
        (handler-case
            (sb-ext:with-timeout 5
              (hierarchy-write! repository "feature" "nope" :conflict-resolution mode))
          (ref-hierarchy-conflict-error (c) (setf saw c))
          (sb-ext:timeout () (setf saw :timed-out)))
        (is (typep saw 'ref-hierarchy-conflict-error)
            "Mode ~S retried or failed differently: ~S" mode saw)
        (when (typep saw 'ref-hierarchy-conflict-error)
          (is (string= "feature" (get-name saw)))
          (is (string= "refs/heads/feature/foo" (get-blocking-ref saw)))))
      (is (equal "kept" (hierarchy-read repository "feature/foo"))))))

(test compare-and-swap-miss-on-a-slash-branch-stays-a-concurrent-modification
  "A real lost update against a slash-bearing branch is still
CONCURRENT-MODIFICATION-ERROR. Requiring the ref to be absent when
it already points at a commit is a compare-and-swap miss, not a
hierarchy collision, so :RETRY still has something to catch."
  (with-temporary-git-repository (repository)
    (hierarchy-write! repository "feature/foo" "kept")
    (let ((sha (git-show-ref-sha repository "feature/foo")))
      (signals concurrent-modification-error
        (git-update-ref! repository "feature/foo" sha :expected-sha nil)))
    (is (equal "kept" (hierarchy-read repository "feature/foo")))))

(test distributed-prepare-of-a-prefix-pair-rolls-back-without-advancing-either-branch
  "One distributed transaction that prepares both \"feature/foo\" and
\"feature\" in the same repository fails in Phase 1, reports the
hierarchy collision (wrapped as DISTRIBUTED-TRANSACTION-ERROR, the
way every other Prepare failure is), deletes the prepare ref it
already created, and advances neither branch."
  (with-temporary-git-repository (repository)
    (let ((condition nil))
      (handler-case
          (with-githack-transaction ()
            (hierarchy-write! repository "feature/foo" "child")
            (hierarchy-write! repository "feature" "parent"))
        (distributed-transaction-error (c) (setf condition c)))
      (is (typep condition 'distributed-transaction-error))
      (let ((report (princ-to-string condition)))
        (is (search "already occupies that path" report))
        (is (search "refs/githack/prepare/" report))
        (is (search "feature/foo" report))))
    (is (null (git-show-ref-sha repository "feature")))
    (is (null (git-show-ref-sha repository "feature/foo")))
    (is (null (%git-for-each-ref repository "refs/githack/prepare/")))))
