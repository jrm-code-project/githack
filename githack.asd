(defsystem githack
  :description "Run Git from within Lisp."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :version "0.1"
  :license "MIT"
  :in-order-to ((test-op (test-op "githack/test")))
  :depends-on ("alexandria" "fold" "function" "named-let" "series")
  :components ((:file "package")
               (:file "git-object" :depends-on ("package"))
               (:file "git-blob" :depends-on ("git-object" "package"))
               (:file "git-tree" :depends-on ("git-object" "package"))
               (:file "git-commit" :depends-on ("git-object" "package"))
               (:file "git-branch" :depends-on ("git-object" "package"))
               (:file "git-repository" :depends-on ("package"))
               (:file "git-transaction"
                :depends-on ("git-object" "git-tree" "git-commit" "git-branch" "git-repository" "package"))))

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
