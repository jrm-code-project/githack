;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite transaction-lock-suite
  :in githack-suite
  :description "Tests for WITH-REPOSITORY-TRANSACTION-LOCK, GitHack's OS-level lock-file mechanism backing CALL-WITH-GIT-TRANSACTION's :CONFLICT-RESOLUTION :LOCK mode.")

(in-suite transaction-lock-suite)

(test with-repository-transaction-lock-runs-body-once-and-cleans-up
  "WITH-REPOSITORY-TRANSACTION-LOCK runs BODY exactly once, returns
its value, and removes its own lock file afterward."
  (with-temporary-git-repository (repository)
    (let ((ran nil))
      (is (= 42
             (with-repository-transaction-lock (repository)
               (setf ran t)
               42)))
      (is (eq t ran))
      (is (not (probe-file (%transaction-lock-pathname repository)))))))

(test with-repository-transaction-lock-cleans-up-on-abnormal-exit
  "WITH-REPOSITORY-TRANSACTION-LOCK removes its own lock file even if
BODY signals an error."
  (with-temporary-git-repository (repository)
    (signals error
      (with-repository-transaction-lock (repository)
        (error "boom")))
    (is (not (probe-file (%transaction-lock-pathname repository))))))

(test with-repository-transaction-lock-excludes-a-second-concurrent-attempt
  "While one WITH-REPOSITORY-TRANSACTION-LOCK holds a repository's
lock, a second attempt to acquire it (via the low-level
%ACQUIRE-REPOSITORY-TRANSACTION-LOCK, since a genuinely concurrent
second thread is unnecessary to prove mutual exclusion) fails to
acquire it while the file still exists, i.e. GIT-DIR's lock file
really is held exclusively."
  (with-temporary-git-repository (repository)
    (with-repository-transaction-lock (repository)
      (is (probe-file (%transaction-lock-pathname repository)))
      (is (null (open (%transaction-lock-pathname repository)
                       :direction :output :if-exists nil :if-does-not-exist :create))))))
