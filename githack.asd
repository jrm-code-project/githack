(defsystem githack
  :description "Run Git from within Lisp."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (alexandria cffi fold named-let table)
  :in-order-to ((test-op (test-op "githack/test")))
  :components ((:file "package")
               (:file "githack" :depends-on ("package"))
               (:file "git-object" :depends-on ("package"))
               (:file "git-blob" :depends-on ("git-object" "package"))
               (:file "git-tree" :depends-on ("git-object" "package"))
               (:file "git-commit" :depends-on ("git-object" "package"))
               (:file "git-branch" :depends-on ("git-object" "package"))
               (:file "git-repository" :depends-on ("package"))
               (:file "git-transaction"
                :depends-on ("git-object" "git-tree" "git-commit" "git-branch" "git-repository" "package"))
               (:file "persistent-wttree" :depends-on ("githack" "package"))
               (:file "persistent-vector"
                :depends-on ("githack" "package" "persistent-wttree"))
               (:file "persistent-hash-table"
                :depends-on
                ("githack" "package" "persistent-vector"
                 "persistent-wttree"))
               (:file "identifier"
                :depends-on
                ("githack" "package" "persistent-vector"))
               (:file "canonical"
                :depends-on
                ("githack" "package" "persistent-hash-table"
                "persistent-vector"))
               (:file "cid-object"
                :depends-on
                ("canonical" "githack" "identifier" "package"
                "persistent-wttree"))
               (:file "cid-set"
                :depends-on ("githack" "package" "persistent-wttree"))
               (:file "cid-detail-table"
                :depends-on
                ("githack" "package" "persistent-hash-table"
                 "persistent-vector"))
               (:file "mapper"
                :depends-on
                ("cid-object" "cid-set" "githack" "identifier"
                 "package" "persistent-hash-table"
                 "persistent-vector"))
               (:file "integer-mapper"
                :depends-on
                ("githack" "identifier" "mapper" "package"
                 "persistent-vector"))
               (:file "versioned-value"
                :depends-on
                ("cid-set" "githack" "package"
                 "persistent-vector"))
               (:file "cvi"
                :depends-on
                ("cid-set" "githack" "package"
                "persistent-vector" "versioned-value"))
               (:file "cvfile"
                :depends-on
                ("cid-set" "cvi" "githack" "package"
                "persistent-vector" "versioned-value"))
               (:file "cid-master-table"
                :depends-on
                ("cid-detail-table" "cid-object" "cid-set"
                 "githack" "mapper" "package"
                 "persistent-vector" "versioned-value"))
               (:file "repository"
                :depends-on
                ("canonical" "cid-master-table" "cid-object" "cid-set"
                 "githack" "identifier" "mapper" "package"
                 "persistent-hash-table" "persistent-vector"
                 "versioned-value"))
               (:file "distributed-object"
                :depends-on
                ("githack" "identifier" "mapper" "package"
                 "repository"))
               (:file "txn"
                :depends-on
                ("cid-master-table" "cid-object" "cid-set" "cvi"
                 "distributed-object" "githack" "package" "repository"
                 "versioned-value"))
               (:file "versioned-object"
                :depends-on
                ("cid-set" "cvi" "cvfile" "githack" "package"
                 "repository" "txn" "versioned-value"))))

(defsystem "githack/test"
  :description "FiveAM test suite for GitHack."
  :depends-on ("githack" "fiveam")
  :components ((:file "test-package")
               (:file "test-helpers" :depends-on ("test-package"))
               (:file "git-object-tests" :depends-on ("test-package" "test-helpers"))
               (:file "atom-serialization-tests" :depends-on ("test-package"))
               (:file "git-tree-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-commit-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-branch-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-repository-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-transaction-tests" :depends-on ("test-package" "test-helpers")))
  :perform (test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "GITHACK-TEST" "RUN-GITHACK-TESTS")
               (error "GITHACK/TEST: one or more tests failed."))))
