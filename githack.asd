(defsystem githack
  :description "Run Git from within Lisp."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :version "0.1"
  :license "MIT"
  :in-order-to ((test-op (test-op "githack/test")))
  :depends-on ("alexandria" "fold" "function" "named-let" "series")
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "git-object" :depends-on ("conditions" "package"))
               (:file "git-io" :depends-on ("git-object" "conditions" "package"))
               (:file "git-blob" :depends-on ("git-object" "conditions" "package"))
               (:file "git-tree" :depends-on ("git-object" "conditions" "package"))
               (:file "git-commit" :depends-on ("git-object" "conditions" "package"))
               (:file "git-branch" :depends-on ("git-object" "conditions" "package"))
               (:file "git-repository" :depends-on ("conditions" "package"))
               (:file "persistent-cons"
                :depends-on ("git-object" "git-blob" "git-tree" "git-io" "conditions" "package"))
               (:file "atomic-wrapper"
                :depends-on ("git-object" "git-blob" "git-tree" "git-commit" "git-io" "conditions" "package"))
               (:file "persistent-vector"
                :depends-on ("git-object" "git-blob" "git-tree" "git-io" "atomic-wrapper" "persistent-cons" "conditions" "package"))
               (:file "persistent-array"
                :depends-on ("git-object" "git-blob" "git-tree" "git-io" "atomic-wrapper" "persistent-vector" "conditions" "package"))
               (:file "persistent-standard-class"
                :depends-on ("git-object" "git-blob" "git-tree" "git-io" "atomic-wrapper"
                              "persistent-cons" "persistent-vector" "persistent-array" "conditions" "package"))
               (:file "persistent-struct"
                :depends-on ("git-object" "git-blob" "git-tree" "persistent-standard-class" "package"))
               (:file "persistent-hash-table"
                :depends-on ("git-object" "git-blob" "git-tree" "git-io" "atomic-wrapper"
                              "persistent-cons" "persistent-vector" "persistent-struct" "conditions" "package"))
               (:file "transaction-lock" :depends-on ("conditions" "package"))
               (:file "git-transaction"
                :depends-on ("git-object" "git-tree" "git-commit" "git-branch" "git-repository"
                              "git-io" "persistent-cons" "persistent-vector" "persistent-array"
                              "persistent-standard-class" "atomic-wrapper" "transaction-lock" "conditions" "package"))
               (:file "transaction"
                :depends-on ("git-object" "git-blob" "git-io" "atomic-wrapper" "git-transaction" "git-repository" "package"))))

(defsystem "githack/test"
  :description "FiveAM test suite for GitHack."
  :depends-on ("githack" "fiveam")
  :components ((:file "test-package")
               (:file "test-helpers" :depends-on ("test-package"))
               (:file "git-object-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-io-tests" :depends-on ("test-package" "test-helpers"))
               (:file "documentation-tests" :depends-on ("test-package"))
               (:file "atom-serialization-tests" :depends-on ("test-package"))
               (:file "git-tree-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-commit-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-branch-tests" :depends-on ("test-package" "test-helpers"))
               (:file "transaction-lock-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-repository-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-cons-tests" :depends-on ("test-package" "test-helpers"))
               (:file "atomic-wrapper-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-vector-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-array-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-standard-class-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-struct-tests" :depends-on ("test-package" "test-helpers"))
               (:file "persistent-hash-table-tests" :depends-on ("test-package" "test-helpers"))
               (:file "git-transaction-tests" :depends-on ("test-package" "test-helpers"))
               (:file "transaction-tests" :depends-on ("test-package" "test-helpers"))
               (:file "end-to-end-tests"
                :depends-on ("test-package" "test-helpers" "persistent-standard-class-tests"
                             "persistent-struct-tests")))
  :perform (test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "GITHACK-TEST" "RUN-GITHACK-TESTS")
               (error "GITHACK/TEST: one or more tests failed."))))
