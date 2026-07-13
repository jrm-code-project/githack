(in-package "CL-USER")

(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)

(use-package :githack)

(defparameter *test-repo-path*
  (merge-pathnames (format nil "githack-test-repo-~A/" (get-universal-time))
                   (uiop:default-temporary-directory)))

(defun run-githack-tests ()
  (format t "Starting GitHack Transaction Tests...~%")

  ;; Delete test directory if it exists to ensure a clean slate
  (ignore-errors
   (uiop:delete-directory-tree *test-repo-path* :validate t :if-does-not-exist :ignore))
  (ensure-directories-exist *test-repo-path*)
  (uiop:run-program (list "git" "init" "--bare" (uiop:native-namestring *test-repo-path*)))

  (let ((githack::*repository-pathname* *test-repo-path*))
    ;; Test 1: Normal commit
    (format t "Test 1: Normal transaction exit commits changes... ")
    (let ((tx (call-with-transaction "Alice" "First Commit"
                (lambda (tx)
                  (transaction-put tx :a 1)
                  (transaction-put tx :b "hello")))))
      (assert (eq (transaction-status tx) :committed))
      (assert (= (transaction-get tx :a) 1))
      (assert (string= (transaction-get tx :b) "hello"))
      (format t "PASS~%"))

    ;; Test 2: Master branch reference set correctly and subsequent transaction reads previous state
    (format t "Test 2: Master branch reference and state persistence... ")
    (let ((master-sha (githack::get-master-branch-sha)))
      (assert (not (null master-sha)))
      (assert (= (length master-sha) 40))

      ;; Start a new transaction to read/modify the state
      (let ((tx2 (call-with-transaction "Bob" "Second Commit"
                   (lambda (tx)
                     (assert (= (transaction-get tx :a) 1))
                     (assert (string= (transaction-get tx :b) "hello"))
                     (transaction-put tx :a 42)
                     (transaction-put tx :c '(x y z))))))
        (assert (eq (transaction-status tx2) :committed))
        (assert (= (transaction-get tx2 :a) 42))
        (assert (equal (transaction-get tx2 :c) '(x y z)))

        ;; Verify master branch moved to a new commit
        (let ((new-master-sha (githack::get-master-branch-sha)))
          (assert (not (string= master-sha new-master-sha)))
          (format t "PASS~%"))))

    ;; Test 3: Transaction abort on standard Lisp error / throw
    (format t "Test 3: Abort on error/throw... ")
    (let ((master-sha (githack::get-master-branch-sha))
          (aborted-tx nil))
      (handler-case
          (call-with-transaction "Charlie" "Failed Commit"
            (lambda (tx)
              (setf aborted-tx tx)
              (transaction-put tx :a 999)
              (error "Simulated error!")))
        (error () nil))
      ;; Check that transaction is aborted
      (assert (eq (transaction-status aborted-tx) :aborted))
      ;; Check that master branch has NOT moved
      (assert (string= master-sha (githack::get-master-branch-sha)))
      (format t "PASS~%"))

    ;; Test 4: Explicit transaction/abort inside receiver
    (format t "Test 4: Explicit transaction/abort... ")
    (let ((master-sha (githack::get-master-branch-sha))
          (aborted-tx (call-with-transaction "Dave" "Explicit Abort"
                        (lambda (tx)
                          (transaction-put tx :a 999)
                          (transaction/abort tx)))))
      (assert (eq (transaction-status aborted-tx) :aborted))
      (assert (string= master-sha (githack::get-master-branch-sha)))
      (format t "PASS~%"))

    ;; Test 5: Explicit transaction/commit inside receiver
    (format t "Test 5: Explicit transaction/commit... ")
    (let ((master-sha (githack::get-master-branch-sha))
          (committed-tx (call-with-transaction "Eve" "Explicit Commit"
                          (lambda (tx)
                            (transaction-put tx :a 100)
                            (transaction/commit tx)
                            ;; Any code after this should not execute, because commit throws
                            (transaction-put tx :a 200)))))
      (assert (eq (transaction-status committed-tx) :committed))
      (assert (= (transaction-get committed-tx :a) 100))
      (assert (not (string= master-sha (githack::get-master-branch-sha))))
      (format t "PASS~%"))

    ;; Test 6: Deserializing branch state from branch tip
    (format t "Test 6: Deserializing branch state from branch tip... ")
    (let ((state (deserialize-branch-state "master")))
      (assert (not (null state)))
      (assert (= (cdr (assoc :a state)) 100))
      (format t "PASS~%"))

    ;; Test 7: Persistent CONS cells are Git trees and round-trip Lisp lists
    (format t "Test 7: Persistent CONS cells... ")
    (let* ((null-sha (persistent-null))
           (cell-sha (persistent-cons '(nested value) null-sha))
           (same-cell-sha
             (persistent-cons '(nested value) null-sha))
           (list-sha (list->persistent-cons '(1 "two" (:three 3))))
           (same-list-sha (list->persistent-cons '(1 "two" (:three 3)))))
      (assert (= (length null-sha) 40))
      (assert (= (length cell-sha) 40))
      (assert (equal (persistent-car cell-sha) '(nested value)))
      (assert (string= (persistent-cdr cell-sha) null-sha))
      (assert (string= cell-sha same-cell-sha))
      (assert (string= list-sha same-list-sha))
      (assert (equal (persistent-cons->list list-sha)
                     '(1 "two" (:three 3))))
      (assert (null (persistent-cons->list null-sha)))
      (assert (string= (list->persistent-cons nil) null-sha))
      (assert
       (handler-case
           (progn (list->persistent-cons '(improper . list)) nil)
         (error () t)))
      (assert
       (handler-case
           (progn (persistent-car null-sha) nil)
         (error () t)))
      (githack::with-repository ()
        (assert
         (equal
          (mapcar #'first
                  (githack::read-tree
                   (githack::current-repository) cell-sha))
          '("car" "cdr")))
        (assert
         (null
          (githack::read-tree
           (githack::current-repository) null-sha))))
      (assert (string= "tree"
                       (string-trim
                        '(#\Space #\Newline #\Return)
                        (uiop:run-program
                         (list "git"
                               (format nil "--git-dir=~A"
                                       (uiop:native-namestring
                                        *test-repo-path*))
                               "cat-file" "-t" cell-sha)
                         :output :string))))
      (format t "PASS~%"))

    ;; Test 8: Persistent weight-balanced trees retain old roots and reload by SHA
    (format t "Test 8: Persistent weight-balanced trees... ")
    (let ((table (make-persistent-wttree-table)))
      (loop for key from 1 to 50
            do (table:table/insert! table key (* key key)))
      (let* ((original-sha (persistent-wttree-sha table))
             (updated (table:table/insert table 51 (* 51 51)))
             (trimmed (table:table/remove updated 1 25 51))
             (reloaded
               (make-persistent-wttree-table
                :representation original-sha))
             (immutable
               (make-persistent-immutable-wttree
                :representation original-sha))
             (initialized-immutable
               (make-persistent-immutable-wttree
                :initial-contents '((3 . three) (1 . one) (2 . two))))
             (union
               (table:table/union
                initialized-immutable
                (make-persistent-wttree-table
                 :initial-contents '((3 . replacement) (4 . four))))))
        (assert (= (table:table/size table) 50))
        (assert (= (table:table/size updated) 51))
        (assert (= (table:table/size trimmed) 48))
        (assert (= (table:table/lookup reloaded 17) 289))
        (assert (equal (table:table/keys reloaded)
                       (loop for key from 1 to 50 collect key)))
        (assert (null (table:table/lookup trimmed 25)))
        (multiple-value-bind (minimum-key minimum-value)
            (table:table/minimum reloaded)
          (assert (= minimum-key 1))
          (assert (= minimum-value 1)))
        (multiple-value-bind (maximum-key maximum-value)
            (table:table/maximum reloaded)
          (assert (= maximum-key 50))
          (assert (= maximum-value 2500)))
        (assert (= (table:table/size
                    (table:table/split-lt reloaded 11))
                   10))
        (assert (= (table:table/size
                    (table:table/split-gt reloaded 40))
                   10))
        (assert (= (table:table/lookup
                    (table:table/insert immutable 51 2601) 51)
                   2601))
        (assert (equal (table:table->alist initialized-immutable)
                       '((1 . one) (2 . two) (3 . three))))
        (assert (equal (table:table->alist union)
                       '((1 . one) (2 . two)
                         (3 . replacement) (4 . four))))
        (assert
         (handler-case
             (progn (table:table/insert! immutable 51 2601) nil)
           (error () t)))
        (let ((root (table:representation reloaded)))
          (assert (typep root 'persistent-node))
          (assert (= (persistent-node-size root) 50))
          (assert (= (persistent-node-value
                      (persistent-node-find #'< root 17))
                     289))
          (labels ((balanced-p (node)
                     (or (null node)
                         (let* ((left (persistent-node-left node))
                                (right (persistent-node-right node))
                                (left-size (persistent-node-size left))
                                (right-size (persistent-node-size right)))
                           (and
                            (not
                             (githack::%weight-too-small-p
                              left-size right-size))
                            (not
                             (githack::%weight-too-small-p
                              right-size left-size))
                            (balanced-p left)
                            (balanced-p right))))))
            (assert (balanced-p root))))
        (assert (string= original-sha
                         (persistent-wttree-sha reloaded)))
        (assert (string= "tree"
                         (string-trim
                          '(#\Space #\Newline #\Return)
                          (uiop:run-program
                           (list "git"
                                 (format nil "--git-dir=~A"
                                         (uiop:native-namestring
                                          *test-repo-path*))
                                 "cat-file" "-t" original-sha)
                           :output :string)))))
      (format t "PASS~%"))

    ;; Test 9: Persistent vectors use separate contents and length trees
    (format t "Test 9: Persistent vectors... ")
    (let* ((original
             (make-persistent-vector
              3 :initial-contents '(alpha beta gamma)))
           (updated (persistent-vector-update original 1 'replacement))
           (same
             (make-persistent-vector
              3 :initial-contents '(alpha beta gamma)))
           (filled
             (make-persistent-vector 4 :initial-element :empty))
           (unchanged
             (persistent-vector-update original 1 'beta)))
      (multiple-value-bind (grown appended-index)
          (persistent-vector-push-extend 'delta updated 16)
        (let ((reloaded
                (persistent-vector-from-sha
                 (persistent-vector-sha grown))))
          (assert (= appended-index 3))
          (assert (= (persistent-vector-length original) 3))
          (assert (= (persistent-vector-length grown) 4))
          (assert (eq (persistent-vector-ref original 1) 'beta))
          (assert (eq (persistent-vector-aref updated 1) 'replacement))
          (assert (equalp (persistent-vector->vector filled)
                          #(:empty :empty :empty :empty)))
          (assert (equalp (persistent-vector->vector reloaded)
                          #(alpha replacement gamma delta)))
          (assert (string= (persistent-vector-sha original)
                           (persistent-vector-sha same)))
          (assert (string= (persistent-vector-sha original)
                           (persistent-vector-sha unchanged)))
          (assert (not (string= (persistent-vector-sha original)
                                (persistent-vector-sha updated))))
          (assert (string=
                   (persistent-vector-length-tree-sha original)
                   (persistent-vector-length-tree-sha updated)))
          (assert (not (string=
                        (persistent-vector-length-tree-sha updated)
                        (persistent-vector-length-tree-sha grown))))
          (assert
           (handler-case
               (progn (persistent-vector-ref original 3) nil)
             (error () t)))
          (assert
           (handler-case
               (progn (persistent-vector-update original -1 :bad) nil)
             (error () t)))
          (assert
           (handler-case
               (progn
                 (make-persistent-vector
                  2 :initial-contents '(only-one))
                 nil)
             (error () t)))
          (githack::with-repository ()
            (assert
             (equal
              (mapcar
               #'first
               (githack::read-tree
                (githack::current-repository)
                (persistent-vector-sha grown)))
              '("contents" "length")))
            (assert
             (equal
              (mapcar
               #'first
               (githack::read-tree
                (githack::current-repository)
                (persistent-vector-length-tree-sha grown)))
              '("length"))))
          (dolist (sha
                    (list (persistent-vector-sha grown)
                          (persistent-vector-length-tree-sha grown)
                          (persistent-vector-contents-sha grown)))
            (assert
             (string=
              "tree"
              (string-trim
               '(#\Space #\Newline #\Return)
               (uiop:run-program
                (list "git"
                      (format nil "--git-dir=~A"
                              (uiop:native-namestring
                               *test-repo-path*))
                      "cat-file" "-t" sha)
                :output :string)))))))
      (multiple-value-bind (singleton index)
          (persistent-vector-push-extend
           'only (make-persistent-vector 0))
        (assert (zerop index))
        (assert (= (persistent-vector-length singleton) 1))
        (assert (eq (persistent-vector-ref singleton 0) 'only)))
      (format t "PASS~%"))

    ;; Test 10: Persistent hash tables use vector buckets and CONS chains
    (format t "Test 10: Persistent hash tables... ")
    (let* ((empty (make-persistent-hash-table :size 1 :test 'equal))
           (one (persistent-hash-table-set empty "alpha" 1))
           (two (persistent-puthash "beta" 2 one))
           (replaced (persistent-hash-table-set two "alpha" 10))
           (same
             (make-persistent-hash-table
              :size 1 :test 'equal
              :initial-contents '(("alpha" . 10) ("beta" . 2))))
           (reloaded
             (persistent-hash-table-from-sha
              (persistent-hash-table-sha replaced)))
           (case-insensitive
             (persistent-hash-table-set
              (make-persistent-hash-table :test 'equalp)
              "Mixed Case" 'found))
           (eql-table
             (persistent-hash-table-set
              (persistent-hash-table-set
               (make-persistent-hash-table :test 'eql :size 2)
               1 :integer)
              1.0 :float)))
      (assert (zerop (persistent-hash-table-count empty)))
      (assert (= (persistent-hash-table-count two) 2))
      (assert (= (persistent-hash-table-count replaced) 2))
      (assert (= (persistent-hash-table-size replaced) 1))
      (assert (eq (persistent-hash-table-test replaced) 'equal))
      (multiple-value-bind (value present-p)
          (persistent-gethash (copy-seq "alpha") replaced)
        (assert present-p)
        (assert (= value 10)))
      (multiple-value-bind (value present-p)
          (persistent-gethash "missing" replaced :absent)
        (assert (not present-p))
        (assert (eq value :absent)))
      (assert (= (persistent-gethash "beta" one -1) -1))
      (assert (eq (persistent-gethash "MIXED CASE" case-insensitive)
                  'found))
      (assert (= (persistent-hash-table-count eql-table) 2))
      (assert (eq (persistent-gethash 1 eql-table) :integer))
      (assert (eq (persistent-gethash 1.0 eql-table) :float))
      (assert (string= (persistent-hash-table-sha replaced)
                       (persistent-hash-table-sha same)))
      (assert (= (persistent-gethash "alpha" reloaded) 10))
      (let ((equalp-replaced
              (persistent-hash-table-set
               case-insensitive "MIXED CASE" 'replaced)))
        (assert (= (persistent-hash-table-count equalp-replaced) 1))
        (assert (eq (persistent-gethash
                     "mixed case" equalp-replaced)
                    'replaced)))
      (assert
       (handler-case
           (progn (make-persistent-hash-table :size 0) nil)
         (error () t)))
      (assert
       (handler-case
           (progn (make-persistent-hash-table :test 'unsupported) nil)
         (error () t)))
      (let* ((buckets (persistent-hash-table-buckets replaced))
             (bucket-sha (persistent-vector-ref buckets 0))
             (entries (persistent-cons->list bucket-sha)))
        (assert (equal entries '(("alpha" . 10) ("beta" . 2))))
        (githack::with-repository ()
          (assert
           (equal
            (mapcar
             #'first
             (githack::read-tree
              (githack::current-repository)
              (persistent-hash-table-sha replaced)))
            '("buckets" "metadata"))))
        (dolist (sha
                  (list (persistent-hash-table-sha replaced)
                        (persistent-hash-table-buckets-sha replaced)
                        bucket-sha))
          (assert
           (string=
            "tree"
            (string-trim
             '(#\Space #\Newline #\Return)
             (uiop:run-program
              (list "git"
                    (format nil "--git-dir=~A"
                            (uiop:native-namestring
                             *test-repo-path*))
                    "cat-file" "-t" sha)
              :output :string))))))
      (multiple-value-bind (removed removed-p)
          (persistent-remhash "alpha" replaced)
        (assert removed-p)
        (assert (= (persistent-hash-table-count removed) 1))
        (assert (not (nth-value
                      1 (persistent-gethash "alpha" removed))))
        (multiple-value-bind (unchanged absent-p)
            (persistent-remhash "missing" removed)
          (assert (not absent-p))
          (assert (eq unchanged removed))))
      (format t "PASS~%")))

  ;; Cleanup test repo (ignore errors on Windows read-only files)
  (ignore-errors
   (uiop:delete-directory-tree *test-repo-path* :validate t :if-does-not-exist :ignore))
  (format t "All tests passed successfully!~%"))

(run-githack-tests)
