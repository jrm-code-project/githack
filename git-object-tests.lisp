;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-object-suite
  :in githack-suite
  :description "Tests for the git-object proxy class hierarchy and INFLATE-GIT-PROXY.")

(in-suite git-object-suite)

(test git-object-is-abstract
  "GIT-OBJECT itself must never be directly instantiable."
  (signals error (make-instance 'git-object)))

(test get-sha-defaults-to-nil
  "A newly created proxy with no :SHA supplied has a NIL SHA and is unloaded."
  (let ((blob (make-instance 'git-blob :repository :dummy-repo)))
    (is (null (get-sha blob)))
    (is (null (get-loaded? blob)))))

(test git-blob-instantiates
  "GIT-BLOB is a concrete GIT-OBJECT subclass whose slots read back as constructed."
  (let ((blob (make-instance 'git-blob :sha "abc123" :repository :dummy-repo)))
    (is (typep blob 'git-object))
    (is (typep blob 'git-blob))
    (is (string= (get-sha blob) "abc123"))
    (is (eq (get-repository blob) :dummy-repo))
    (is (null (get-loaded? blob)))))

(test git-tree-instantiates
  "GIT-TREE is a concrete GIT-OBJECT subclass whose SHA reads back as constructed."
  (let ((tree (make-instance 'git-tree :sha "def456" :repository :dummy-repo)))
    (is (typep tree 'git-object))
    (is (typep tree 'git-tree))
    (is (string= (get-sha tree) "def456"))))

(test git-commit-instantiates
  "GIT-COMMIT is a concrete GIT-OBJECT subclass whose SHA reads back as constructed."
  (let ((commit (make-instance 'git-commit :sha "ghi789" :repository :dummy-repo)))
    (is (typep commit 'git-object))
    (is (typep commit 'git-commit))
    (is (string= (get-sha commit) "ghi789"))))

(test get-loaded?-can-be-set-explicitly
  "LOADED? can be supplied at construction time via the :loaded? initarg."
  (let ((blob (make-instance 'git-blob :sha "abc123" :repository :dummy-repo
                                        :loaded? t)))
    (is (eq t (get-loaded? blob)))))

(test inflate-git-proxy-dispatches-blob
  "INFLATE-GIT-PROXY returns a GIT-BLOB when GIT-TYPE reports \"blob\"."
  (with-fake-git-type ((list (cons "b1" "blob")))
    (let ((object (inflate-git-proxy :dummy-repo "b1")))
      (is (typep object 'git-blob))
      (is (string= (get-sha object) "b1"))
      (is (eq (get-repository object) :dummy-repo)))))

(test inflate-git-proxy-dispatches-tree
  "INFLATE-GIT-PROXY returns a GIT-TREE when GIT-TYPE reports \"tree\"."
  (with-fake-git-type ((list (cons "t1" "tree")))
    (is (typep (inflate-git-proxy :dummy-repo "t1") 'git-tree))))

(test inflate-git-proxy-dispatches-commit
  "INFLATE-GIT-PROXY returns a GIT-COMMIT when GIT-TYPE reports \"commit\"."
  (with-fake-git-type ((list (cons "c1" "commit")))
    (is (typep (inflate-git-proxy :dummy-repo "c1") 'git-commit))))

(test inflate-git-proxy-signals-on-unknown-type
  "INFLATE-GIT-PROXY signals an error for a GIT-TYPE result it does not recognize."
  (with-fake-git-type ((list (cons "x1" "tag")))
    (signals error (inflate-git-proxy :dummy-repo "x1"))))

(test inflate-git-proxy-requires-string-sha
  "INFLATE-GIT-PROXY signals an error when SHA is not a string."
  (with-fake-git-type ((list (cons "b1" "blob")))
    (signals error (inflate-git-proxy :dummy-repo 12345))))
