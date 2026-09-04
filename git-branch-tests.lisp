;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-branch-suite
  :in githack-suite
  :description "Tests for the GIT-BRANCH proxy, RESOLVE-BRANCH, and UPDATE-BRANCH.")

(in-suite git-branch-suite)

(defparameter +commit-sha+ "dddddddddddddddddddddddddddddddddddddddd"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a persisted GIT-COMMIT in tests.")

(test git-branch-instantiates
  "GIT-BRANCH is a concrete class (not a GIT-OBJECT subclass) whose
slots read back as constructed."
  (let* ((commit (make-instance 'git-commit :sha +commit-sha+ :repository :dummy-repo))
         (branch (make-instance 'git-branch :repository :dummy-repo :name "main" :target commit)))
    (is (not (typep branch 'git-object)))
    (is (eq :dummy-repo (get-repository branch)))
    (is (string= "main" (get-name branch)))
    (is (eq commit (get-target branch)))))

(test resolve-branch-inflates-target-commit
  "RESOLVE-BRANCH looks up the branch's current SHA via
GIT-SHOW-REF-SHA and returns a GIT-BRANCH whose TARGET is a
lazily-loaded GIT-COMMIT proxy for that SHA."
  (with-fake-git-show-ref-sha ((list (cons (cons :dummy-repo "main") +commit-sha+)))
    (with-fake-git-type ((list (cons +commit-sha+ "commit")))
      (let ((branch (resolve-branch :dummy-repo "main")))
        (is (string= "main" (get-name branch)))
        (is (eq :dummy-repo (get-repository branch)))
        (is (typep (get-target branch) 'git-commit))
        (is (string= +commit-sha+ (get-sha (get-target branch))))
        (is (null (get-loaded? (get-target branch))))))))

(test resolve-branch-signals-error-for-unknown-branch
  "RESOLVE-BRANCH signals an error when GIT-SHOW-REF-SHA reports no
SHA for the requested branch name."
  (with-fake-git-show-ref-sha ('())
    (signals error (resolve-branch :dummy-repo "no-such-branch"))))

(test update-branch-forwards-target-sha-to-git-update-ref
  "UPDATE-BRANCH calls GIT-UPDATE-REF with BRANCH's repository, name,
and its TARGET commit's SHA, and returns BRANCH itself."
  (let* ((commit (make-instance 'git-commit :sha +commit-sha+ :repository :dummy-repo))
         (branch (make-instance 'git-branch :repository :dummy-repo :name "main" :target commit))
         (calls '()))
    (with-recording-git-update-ref (calls)
      (let ((result (update-branch branch)))
        (is (eq branch result))
        (is (equal (list (list :dummy-repo "main" +commit-sha+)) calls))))))

(test update-branch-signals-error-for-unpersisted-target
  "UPDATE-BRANCH cannot advance a branch whose TARGET commit has no
SHA yet."
  (let* ((unsaved-commit (make-instance 'git-commit :repository :dummy-repo))
         (branch (make-instance 'git-branch :repository :dummy-repo :name "main" :target unsaved-commit))
         (calls '()))
    (with-recording-git-update-ref (calls)
      (signals error (update-branch branch))
      (is (null calls)))))
