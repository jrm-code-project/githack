;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-commit-suite
  :in githack-suite
  :description "Tests for the GIT-COMMIT proxy, SERIALIZE-COMMIT, and DESERIALIZE-COMMIT.")

(in-suite git-commit-suite)

(defparameter +root-tree-sha+ "cccccccccccccccccccccccccccccccccccccccc"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a persisted GIT-TREE in tests.")

(defparameter +parent-1-sha+ "1111111111111111111111111111111111111111"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a persisted GIT-COMMIT parent in tests.")

(defparameter +parent-2-sha+ "2222222222222222222222222222222222222222"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a second persisted GIT-COMMIT parent in tests.")

(defun %make-test-commit (&key tree parents (author "The Boss <boss@githack.local>")
                                (committer "The Boss <boss@githack.local>")
                                (timestamp 1700000000) (message "first commit"))
  (make-instance 'git-commit :repository :dummy-repo
                              :tree tree
                              :parents parents
                              :author author
                              :committer committer
                              :timestamp timestamp
                              :message message))

(test serialize-commit-signals-error-for-unpersisted-tree
  "SERIALIZE-COMMIT cannot encode a commit whose TREE has no SHA yet."
  (let* ((unsaved-tree (make-instance 'git-tree :repository :dummy-repo))
         (commit (%make-test-commit :tree unsaved-tree)))
    (signals error (serialize-commit commit))))

(test serialize-commit-signals-error-for-unpersisted-parent
  "SERIALIZE-COMMIT cannot encode a commit whose one of PARENTS has no SHA yet."
  (let* ((tree (make-instance 'git-tree :sha +root-tree-sha+ :repository :dummy-repo))
         (unsaved-parent (make-instance 'git-commit :repository :dummy-repo))
         (commit (%make-test-commit :tree tree :parents (list unsaved-parent))))
    (signals error (serialize-commit commit))))

(test serialize-commit-with-no-parents
  "SERIALIZE-COMMIT formats a root (parentless) commit as a
\"tree\" line, an \"author\" line, a \"committer\" line, a blank
line, and the message, with no \"parent\" lines at all."
  (let* ((tree (make-instance 'git-tree :sha +root-tree-sha+ :repository :dummy-repo))
         (commit (%make-test-commit :tree tree)))
    (is (string= (format nil "tree ~A~%author The Boss <boss@githack.local> 1700000000 +0000~%committer The Boss <boss@githack.local> 1700000000 +0000~%~%first commit"
                         +root-tree-sha+)
                 (serialize-commit commit)))))

(test serialize-commit-with-multiple-parents
  "SERIALIZE-COMMIT emits one \"parent\" line per entry in PARENTS,
in order, between the \"tree\" line and the \"author\" line."
  (let* ((tree (make-instance 'git-tree :sha +root-tree-sha+ :repository :dummy-repo))
         (parent-1 (make-instance 'git-commit :sha +parent-1-sha+ :repository :dummy-repo))
         (parent-2 (make-instance 'git-commit :sha +parent-2-sha+ :repository :dummy-repo))
         (commit (%make-test-commit :tree tree :parents (list parent-1 parent-2))))
    (is (string= (format nil "tree ~A~%parent ~A~%parent ~A~%author The Boss <boss@githack.local> 1700000000 +0000~%committer The Boss <boss@githack.local> 1700000000 +0000~%~%first commit"
                         +root-tree-sha+ +parent-1-sha+ +parent-2-sha+)
                 (serialize-commit commit)))))

(test deserialize-commit-populates-slots-with-no-parents
  "DESERIALIZE-COMMIT parses a root commit's text and populates a
GIT-COMMIT's TREE, AUTHOR, COMMITTER, TIMESTAMP, and MESSAGE slots,
leaving PARENTS empty."
  (let* ((text (format nil "tree ~A~%author The Boss <boss@githack.local> 1700000000 +0000~%committer The Boss <boss@githack.local> 1700000000 +0000~%~%first commit"
                       +root-tree-sha+))
         (commit (make-instance 'git-commit :repository :dummy-repo)))
    (with-fake-git-type ((list (cons +root-tree-sha+ "tree")))
      (let ((result (deserialize-commit commit text)))
        (is (eq commit result))
        (is (eq t (get-loaded? commit)))
        (is (typep (get-tree commit) 'git-tree))
        (is (string= +root-tree-sha+ (sha (get-tree commit))))
        (is (null (get-parents commit)))
        (is (string= "The Boss <boss@githack.local>" (get-author commit)))
        (is (string= "The Boss <boss@githack.local>" (get-committer commit)))
        (is (= 1700000000 (get-timestamp commit)))
        (is (string= "first commit" (get-message commit)))))))

(test deserialize-commit-populates-parents-in-order
  "DESERIALIZE-COMMIT collects multiple \"parent\" header lines, in
the order they appear, as lazily-loaded GIT-COMMIT proxies."
  (let* ((text (format nil "tree ~A~%parent ~A~%parent ~A~%author The Boss <boss@githack.local> 1700000000 +0000~%committer The Boss <boss@githack.local> 1700000000 +0000~%~%merge commit"
                       +root-tree-sha+ +parent-1-sha+ +parent-2-sha+))
         (commit (make-instance 'git-commit :repository :dummy-repo)))
    (with-fake-git-type ((list (cons +root-tree-sha+ "tree")
                               (cons +parent-1-sha+ "commit")
                               (cons +parent-2-sha+ "commit")))
      (deserialize-commit commit text)
      (is (= 2 (length (get-parents commit))))
      (destructuring-bind (first-parent second-parent) (get-parents commit)
        (is (typep first-parent 'git-commit))
        (is (string= +parent-1-sha+ (sha first-parent)))
        (is (typep second-parent 'git-commit))
        (is (string= +parent-2-sha+ (sha second-parent)))))))

(test deserialize-commit-preserves-multiline-message
  "DESERIALIZE-COMMIT's MESSAGE includes everything after the blank
line separating headers from message, including embedded newlines."
  (let* ((text (format nil "tree ~A~%author The Boss <boss@githack.local> 1700000000 +0000~%committer The Boss <boss@githack.local> 1700000000 +0000~%~%Summary line~%~%Body paragraph."
                       +root-tree-sha+))
         (commit (make-instance 'git-commit :repository :dummy-repo)))
    (with-fake-git-type ((list (cons +root-tree-sha+ "tree")))
      (deserialize-commit commit text)
      (is (string= (format nil "Summary line~%~%Body paragraph.") (get-message commit))))))

(test serialize-deserialize-commit-round-trips
  "Serializing a GIT-COMMIT and then deserializing the resulting text
into a fresh GIT-COMMIT reconstructs equivalent slot values."
  (let* ((tree (make-instance 'git-tree :sha +root-tree-sha+ :repository :dummy-repo))
         (parent (make-instance 'git-commit :sha +parent-1-sha+ :repository :dummy-repo))
         (original (%make-test-commit :tree tree :parents (list parent)))
         (text (serialize-commit original))
         (reloaded (make-instance 'git-commit :repository :dummy-repo)))
    (with-fake-git-type ((list (cons +root-tree-sha+ "tree")
                               (cons +parent-1-sha+ "commit")))
      (deserialize-commit reloaded text)
      (is (string= +root-tree-sha+ (sha (get-tree reloaded))))
      (is (= 1 (length (get-parents reloaded))))
      (is (string= +parent-1-sha+ (sha (first (get-parents reloaded)))))
      (is (string= (get-author original) (get-author reloaded)))
      (is (string= (get-committer original) (get-committer reloaded)))
      (is (= (get-timestamp original) (get-timestamp reloaded)))
      (is (string= (get-message original) (get-message reloaded))))))
