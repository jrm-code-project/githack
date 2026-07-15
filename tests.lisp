(in-package "CL-USER")

(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)

(use-package :githack)

(defparameter *test-repo-path*
  (merge-pathnames (format nil "githack-test-repo-~A/" (get-universal-time))
                   (uiop:default-temporary-directory)))

(defclass test-versioned-object (versioned-standard-object)
  ((value
    :initarg :value
    :version-technique :scalar)
   (ordinary
    :initarg :ordinary
    :initform :ordinary))
  (:metaclass versioned-standard-class))

(defclass test-distributed-versioned-object (versioned-standard-object distributed-object)
  ((value
    :initarg :value
    :version-technique :scalar))
  (:metaclass versioned-standard-class))

(defun %make-test-distributed-versioned-object-root
    (repo-ptr repository-mapper-sha numeric-id value-sha)
  (githack::create-tree
   repo-ptr
   (list
    (list "name" (githack::%stored-object repo-ptr "TestObj")
          githack::+git-filemode-blob+)
    (list "numeric-id" (githack::%stored-object repo-ptr numeric-id)
          githack::+git-filemode-blob+)
    (list "repository-mapper" repository-mapper-sha
          githack::+git-filemode-tree+)
    (list "type" (githack::%stored-object repo-ptr :test-distributed-versioned)
          githack::+git-filemode-blob+)
    (list "value" value-sha githack::+git-filemode-tree+))))

(defun %test-distributed-versioned-object-data (repo-ptr sha)
  (let ((entries (githack::read-tree repo-ptr sha)))
    (let ((name (githack::%loaded-object repo-ptr (second (assoc "name" entries :test #'string=))))
          (numeric-id (githack::%loaded-object repo-ptr (second (assoc "numeric-id" entries :test #'string=))))
          (repository-mapper-sha (second (assoc "repository-mapper" entries :test #'string=)))
          (value-sha (second (assoc "value" entries :test #'string=))))
      (list name numeric-id repository-mapper-sha value-sha))))

(defmethod initialize-instance :after
    ((instance test-distributed-versioned-object) &key sha)
  (when sha
    (let* ((data
             (githack::with-repository ()
               (%test-distributed-versioned-object-data
                (githack::current-repository) sha)))
           (value-sha (fourth data)))
      (setf (slot-value-unversioned instance 'value)
            (versioned-value-from-sha value-sha)))))

(defmethod distributed-object-identifier
    ((object test-distributed-versioned-object))
  (githack::with-repository ()
    (let ((data
            (%test-distributed-versioned-object-data
             (githack::current-repository)
             (distributed-object-sha object))))
      (make-distributed-identifier
       :domain "example.com"
       :repository "dist-crossing-project"
       :class :test-distributed-versioned
       :numeric-id (second data)))))

;; Redefine distributed-object-from-sha to support our test type without invoking %distributed-object-data
(defun githack::distributed-object-from-sha (sha)
  (check-type sha string)
  (githack::with-repository ()
    (let* ((entries (githack::read-tree (githack::current-repository) sha))
           (type-entry (assoc "type" entries :test #'string=))
           (type (and type-entry (githack::%loaded-object (githack::current-repository) (second type-entry)))))
      (make-instance
       (cond
         ((eq type :core-user) 'githack::core-user)
         ((eq type :test-distributed-versioned) 'test-distributed-versioned-object)
         (t (error "Unknown distributed object type ~S for sha ~S." type sha)))
       :sha sha))))

(defun make-test-distributed-versioned-object (repository value)
  (let* ((local (githack::repository/local-mapper repository))
         (class-key :test-distributed-versioned)
         (existing (mapper/resolve local class-key))
         (class-mapper
           (or existing
               (make-ordered-mapper
                :mapping-level "Class TestDistributedVersionedObject"
                :key class-key :parent local))))
    (unless (typep class-mapper 'ordered-mapper)
      (error "class mapper is not ordered."))
    (multiple-value-bind (reserved numeric-id)
        (ordered-mapper/reserve-entry class-mapper)
      (let* ((val-obj (make-scalar-versioned-value :initial-value value))
             (val-sha (versioned-value-sha val-obj))
             (obj-sha
               (githack::with-repository ()
                 (%make-test-distributed-versioned-object-root
                  (githack::current-repository)
                  (mapper-sha local)
                  numeric-id
                  val-sha)))
             (object (make-instance 'test-distributed-versioned-object
                                    :sha obj-sha))
             (populated
               (ordered-mapper/set-entry reserved numeric-id object))
             (updated-local
               (unordered-mapper/set-entry local class-key populated))
             (updated-repository
               (githack::%repository-root-with-local-mapper repository updated-local)))
        (values updated-repository object)))))

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
      (format t "PASS~%"))

    ;; Test 11: CID objects, sets, and detail tables
    (format t "Test 11: Persistent CID objects... ")
    (let* ((unresolved (make-cid-object "repository.CID.7"))
           (resolved
             (cid-object-with-cid unresolved 7))
           (same
             (make-cid-object "repository.CID.7" :cid 7))
           (reloaded
             (cid-object-from-sha (cid-object-sha resolved))))
      (assert (string= (cid-object/did unresolved)
                       "repository.CID.7"))
      (assert (null (cid-object/cid unresolved)))
      (assert (= (cid-object/cid resolved) 7))
      (assert (cid-object-equal-p unresolved resolved))
      (assert (string= (cid-object-sha resolved)
                       (cid-object-sha same)))
      (assert (= (cid-object/cid reloaded) 7))
      (assert
       (handler-case
           (progn
             (make-cid-object "repository.CID.0" :cid 0)
             nil)
         (error () t)))
      (githack::with-repository ()
        (assert
         (equal
          (mapcar
           #'first
           (githack::read-tree
            (githack::current-repository)
            (cid-object-sha resolved)))
          '("cid" "did"))))
      (format t "PASS~%"))

    (format t "Test 12: Persistent CID sets... ")
    (let* ((empty (cid-set/empty "repository"))
           (set (list->cid-set "repository" '(5 1 3 3)))
           (other (list->cid-set "repository" '(3 4)))
           (removed (cid-set/remove set 3))
           (reloaded (cid-set-from-sha (cid-set-sha set))))
      (assert (cid-set? empty))
      (assert (cid-set/empty? empty))
      (assert (cid-set/member empty 0))
      (assert (not (cid-set/member empty 1)))
      (assert (= (cid-set-count set) 3))
      (assert (equal (cid-set->list set) '(1 3 5)))
      (assert (equal (cid-set->list reloaded) '(1 3 5)))
      (assert (= (cid-set/highest-active-cid set) 5))
      (assert (= (cid-set/last-cid set) 5))
      (assert (equal (cid-set->list removed) '(1 5)))
      (assert (cid-set/member set 3))
      (assert (eq set (cid-set/adjoin set 3)))
      (assert (eq set (cid-set/remove set 99)))
      (assert (equal (cid-set->list (cid-set/union set other))
                     '(1 3 4 5)))
      (assert
       (equal (cid-set->list (cid-set/intersection set other))
              '(3)))
      (assert (cid-set/intersection? set other))
      (assert
       (equal (cid-set->list (cid-set/exclusive-or set other))
              '(1 4 5)))
      (assert
       (equal (cid-set->list (range->cid-set "repository" 2 5))
              '(2 3 4)))
      (let ((comparison
              (list->cid-set "repository" '(3 5 1))))
        (multiple-value-bind (equal-p examined-p)
            (cid-set/equal? set comparison)
          (assert equal-p)
          (assert (typep examined-p 'boolean))
          (unless examined-p
            (assert
             (string= (cid-set-sha set)
                      (cid-set-sha comparison))))))
      (assert
       (handler-case
           (progn
             (cid-set/union set (cid-set/empty "other-repository"))
             nil)
         (error () t)))
      (format t "PASS~%"))

    (format t "Test 13: Persistent CID detail tables... ")
    (let* ((changed-object
             (make-cid-object "repository.CID.9" :cid 9))
           (empty (make-cid-detail-table :size 2)))
      (multiple-value-bind (first-table first-entry)
          (cid-detail-table/log-change
           empty changed-object 'slot-a)
        (multiple-value-bind (unchanged duplicate-entry)
            (cid-detail-table/log-change
             first-table changed-object 'slot-a)
          (assert (eq unchanged first-table))
          (assert
           (equalp
            (persistent-vector->vector
             (cid-detail-table-entry/slots-modified duplicate-entry))
            #(slot-a))))
        (multiple-value-bind (second-table second-entry)
            (cid-detail-table/log-change
             first-table changed-object 'slot-b)
          (declare (ignore second-entry))
          (multiple-value-bind (third-table third-entry)
              (cid-detail-table/log-change
               second-table
               (make-cid-object "repository.CID.9")
               'slot-c)
            (declare (ignore third-entry))
          (let* ((reloaded
                   (cid-detail-table-from-sha
                    (cid-detail-table-sha third-table)))
                 (found
                   (cid-detail-table/find-entry
                    reloaded changed-object)))
            (assert
             (cid-object-equal-p
              changed-object
              (cid-detail-table-entry/object-changed found)))
            (assert
             (equalp
              (persistent-vector->vector
               (cid-detail-table-entry/slots-modified first-entry))
              #(slot-a)))
            (assert
             (equalp
              (persistent-vector->vector
               (cid-detail-table-entry/slots-modified found))
              #(slot-a slot-b slot-c)))
            (assert
             (= (persistent-hash-table-count
                 (cid-detail-table/entries third-table))
                1))
            (githack::with-repository ()
              (assert
               (equal
                (mapcar
                 #'first
                 (githack::read-tree
                  (githack::current-repository)
                   (cid-detail-table-sha third-table)))
                '("entries"))))))))
      (format t "PASS~%"))

    ;; Test 14: Canonical and distributed identifiers
    (format t "Test 14: Persistent identifiers... ")
    (let* ((canonical
             (make-canonical-identifier
              '("example" "repository" :widget 42)))
           (same-canonical
             (make-canonical-identifier
              '("example" "repository" :widget 42)))
           (did
             (make-distributed-identifier
              :domain "example.com"
              :repository "repository.with.dots"
              :class :widget
              :numeric-id 42))
           (parsed
             (parse-did
              (format nil "[~A]" (did->string did))
              :brackets-allowed t))
           (reloaded
             (distributed-identifier-from-sha
              (canonical-identifier-sha did))))
      (assert
       (equal (canonical-identifier-components canonical)
              '("example" "repository" :widget 42)))
      (assert
       (string= (canonical-identifier-sha canonical)
                (canonical-identifier-sha same-canonical)))
      (assert (canonical-identifier-equal-p did parsed))
      (assert (equal (did->list reloaded)
                     '("example.com" "repository.with.dots" :widget 42)))
      (assert (string= (did/domain did) "example.com"))
      (assert (string= (did/repository did)
                       "repository.with.dots"))
      (assert (eq (did/class did) :widget))
      (assert (= (did/numeric-id did) 42))
      (assert
       (equal (did->list (did/locale did))
              '("example.com" "repository.with.dots" nil 0)))
      (assert
       (equal (did->list (parse-did "repository.widget.7"))
              '("" "repository" :widget 7)))
      (assert
       (handler-case
           (progn
             (make-distributed-identifier
              :repository "repository" :numeric-id 1)
             nil)
         (error () t)))
      (let* ((cid (make-cid-object did :cid 42))
             (cid-copy (cid-object-from-sha (cid-object-sha cid))))
        (assert
         (canonical-identifier-equal-p
          (cid-object/did cid-copy) did)))
      (format t "PASS~%"))

    ;; Test 15: Persistent unordered and ordered mappers
    (format t "Test 15: Persistent mappers... ")
    (let* ((root
             (make-unordered-mapper
              :mapping-level "Root" :test 'equal))
           (domain
             (make-unordered-mapper
              :mapping-level "Domain" :key "example"
              :parent root :test 'equal))
           (repository
             (make-unordered-mapper
              :mapping-level "Repository" :key "repository"
              :parent domain :test 'equal))
           (class
             (make-ordered-mapper
              :mapping-level "Class" :key :widget
              :parent repository))
           (target-did
             (make-distributed-identifier
              :domain "example" :repository "repository"
              :class :widget :numeric-id 1))
           (target
             (make-cid-object target-did :cid 1)))
      (multiple-value-bind (reserved-class numeric-id)
          (ordered-mapper/reserve-entry class)
        (assert (= numeric-id 1))
        (assert (null (mapper/resolve class 1)))
        (let* ((populated-class
                 (ordered-mapper/set-entry
                  reserved-class numeric-id target))
               (populated-repository
                 (mapper/install-child-mapper
                  repository populated-class))
               (populated-domain
                 (mapper/install-child-mapper
                  domain populated-repository))
               (populated-root
                 (mapper/install-child-mapper
                  root populated-domain))
               (reloaded-root
                 (mapper-from-sha (mapper-sha populated-root)))
               (resolved
                 (distributed-identifier/resolve
                  target-did reloaded-root)))
          (assert (typep resolved 'cid-object))
          (assert (cid-object-equal-p resolved target))
          (assert
           (equal
            (canonical-identifier-components
             (mapper/prefix populated-class))
            '("example" "repository" :widget)))
          (assert
           (equal
            (canonical-identifier-components
             (mapper/parent populated-class))
            '("example" "repository")))
          (assert
           (equal (did->list
                   (mapper-distributed-identifier populated-class))
                  '("example" "repository" :widget 0)))
          (assert
           (handler-case
               (progn
                 (mapper/install-child-mapper
                  populated-root populated-domain)
                 nil)
             (error () t)))
          (githack::with-repository ()
            (assert
             (equal
              (mapcar
               #'first
               (githack::read-tree
                (githack::current-repository)
                (mapper-sha populated-root)))
              '("entries" "key" "level" "prefix" "type"))))))
      (multiple-value-bind (hierarchy local)
          (distributed-identifier-create-mapper-hierarchy
           (make-distributed-identifier
            :domain "example" :repository "repository"))
        (let ((resolved-local
                (distributed-identifier/resolve
                 (make-distributed-identifier
                  :domain "example" :repository "repository")
                 hierarchy :repository-only t)))
          (assert (string= (mapper/key local) "repository"))
          (assert (string= (mapper/key resolved-local)
                           "repository"))))
      (format t "PASS~%"))

    ;; Test 16: Persistent versioned values
    (format t "Test 16: Persistent versioned values... ")
    (let* ((empty-set (cid-set/empty "repository"))
           (cid1-set (cid-set/adjoin empty-set 1))
           (both-set (cid-set/adjoin cid1-set 2))
           (nvi (make-nonlogged-versioned-value
                 :initial-value :initial :cid 1))
           (updated-nvi
             (versioned-value/update
              :updated nvi 2 nil 'slot))
           (lvi (make-logged-versioned-value
                 :initial-value :logged :cid 1))
           (updated-lvi
             (versioned-value/update
              :logged-again lvi 2 nil 'slot))
           (scalar (make-scalar-versioned-value
                    :initial-value :default))
           (scalar-1
             (versioned-value/update :one scalar 1 nil 'slot))
           (scalar-2
             (versioned-value/update :two scalar-1 2 nil 'slot))
           (scalar-replaced
             (versioned-value/update
              :two-replaced scalar-2 2 nil 'slot)))
      (assert
       (eq (versioned-value/view nvi empty-set nil 'slot)
           :initial))
      (assert
       (eq (versioned-value/view updated-nvi empty-set nil 'slot)
           :updated))
      (assert
       (eq (versioned-value/view nvi empty-set nil 'slot)
           :initial))
      (assert (equal (versioned-value/cid-list nvi) '(1)))
      (assert (versioned-value/contains-cid? nvi 1))
      (assert (= (versioned-value/most-recent-cid nvi both-set) 1))
      (assert (typep updated-lvi 'logged-versioned-value))
      (assert
       (eq (versioned-value/view
            updated-lvi empty-set nil 'slot)
           :logged-again))
      (assert
       (eq (versioned-value/view scalar empty-set nil 'slot)
           :default))
      (assert
       (eq (versioned-value/view scalar-2 both-set nil 'slot)
           :two))
      (assert
       (eq (versioned-value/view scalar-2 cid1-set nil 'slot)
           :one))
      (assert
       (eq (versioned-value/view
            scalar-replaced both-set nil 'slot)
           :two-replaced))
      (assert
       (equal (versioned-value/cid-list scalar-replaced)
              '(1 2)))
      (assert
       (eq (versioned-value/contains-cid?
            scalar-replaced 1)
           t))
      (assert
       (= (versioned-value/most-recent-cid
           scalar-replaced both-set)
          2))
      (assert
       (handler-case
           (progn
             (scalar-versioned-value/add-value
              scalar-replaced 1 :out-of-order)
             nil)
         (error () t)))
      (let ((reloaded-nvi
              (versioned-value-from-sha
               (versioned-value-sha nvi)))
            (reloaded-scalar
              (versioned-value-from-sha
               (versioned-value-sha scalar-replaced))))
        (assert
         (eq (versioned-value/view
              reloaded-nvi empty-set nil 'slot)
             :initial))
        (assert (typep reloaded-scalar
                       'scalar-versioned-value))
        (assert
         (eq (versioned-value/view
              reloaded-scalar both-set nil 'slot)
             :two-replaced)))
      (format t "PASS~%"))

    ;; Test 17: Persistent CID master tables
    (format t "Test 17: Persistent CID master tables... ")
    (let* ((basis (cid-set/empty "repository"))
           (who (make-distributed-identifier
                 :domain "example" :repository "repository"
                 :class :user :numeric-id 7))
           (object
             (make-cid-object
              (make-distributed-identifier
               :domain "example" :repository "repository"
               :class :object :numeric-id 9)))
           (entry
             (make-cid-master-table-entry
              :cid 1 :cid-set-basis basis :who who
              :why "initial import" :when-start 100))
           (logged-entry
             (nth-value
              0
              (cid-master-table-entry/log-change
               entry object 'name)))
           (finished-entry
             (cid-master-table-entry/note-finish
              logged-entry "completed" :change 200))
           (empty-master (make-cid-master-table))
           (master-1
             (cid-master-table/add-entry
              empty-master finished-entry))
           (cid-object
             (make-cid-object
              (make-distributed-identifier
               :domain "example" :repository "repository"
               :class :change :numeric-id 3)
              :cid 3))
           (master-with-object
             (cid-master-table/set-cid-object
              master-1 3 cid-object))
           (entry-3
             (make-cid-master-table-entry
              :cid 3 :cid-set-basis basis :who who
              :why "later change" :when-start 300))
           (master-3
             (cid-master-table/add-entry
              master-with-object entry-3)))
      (assert (= (cid-master-table-entry/cid entry) 1))
      (assert
       (canonical-identifier-equal-p
        (cid-master-table-entry/who entry) who))
      (assert
       (string= (cid-master-table-entry/why entry)
                "initial import"))
      (assert (= (cid-master-table-entry/when-start entry) 100))
      (assert (null (cid-master-table-entry/when-finish entry)))
      (let ((detail-entry
              (cid-detail-table/find-entry
               (cid-master-table-entry/cid-detail-table
                logged-entry)
               object)))
        (assert detail-entry)
        (assert
         (equalp
          (persistent-vector->vector
           (cid-detail-table-entry/slots-modified detail-entry))
          #(name))))
      (assert
       (null
        (cid-detail-table/find-entry
         (cid-master-table-entry/cid-detail-table entry)
         object)))
      (assert
       (= (cid-master-table-entry/when-finish finished-entry)
          200))
      (assert
       (eq
        (cid-master-table-entry/versioned-change-information
         finished-entry)
        :change))
      (assert
       (= (cid-master-table/last-allocated-cid
           empty-master)
          0))
      (assert
       (= (cid-master-table/last-allocated-cid master-3) 3))
      (assert
       (cid-master-table/contains-cid? master-3 1))
      (assert
       (not (cid-master-table/contains-cid? master-3 2)))
      (assert
       (cid-master-table/contains-cid? master-3 3))
      (assert
       (cid-object-equal-p
        (cid-master-table/cid-object master-3 3)
        cid-object))
      (assert
       (equal
        (cid-set->list
         (cid-master-table/active-cids
          "repository" master-3 :end-time 250))
        '(1)))
      (multiple-value-bind (why comparison change returned-basis)
          (cid-master-table/cid-information master-3 1)
        (assert (string= why "completed"))
        (assert (= comparison 200))
        (assert (eq change :change))
        (assert
         (string= (cid-set-sha returned-basis)
                  (cid-set-sha basis))))
      (let* ((entry-copy
               (cid-master-table-entry-from-sha
                (cid-master-table-entry-sha finished-entry)))
             (master-copy
               (cid-master-table-from-sha
                (cid-master-table-sha master-3))))
        (assert
         (= (cid-master-table-entry/when-finish entry-copy)
            200))
        (assert
         (= (cid-master-table/last-allocated-cid master-copy)
            3))
        (assert
         (cid-master-table/contains-cid? master-copy 1)))
      (format t "PASS~%"))

    ;; Test 18: Composite version indexes
    (format t "Test 18: Persistent composite version indexes... ")
    (let* ((empty-set (cid-set/empty "repository"))
           (cid1-set (cid-set/adjoin empty-set 1))
           (cid12-set (cid-set/adjoin cid1-set 2))
           (cid13-set (cid-set/adjoin cid1-set 3))
           (all-set (cid-set/adjoin cid12-set 3))
           (empty-cvi (make-cvi))
           (cvi-1
             (cvi/update empty-cvi '(a b c) 1 empty-set))
           (cvi-2
             (cvi/update cvi-1 '(a x c d) 2 cid1-set))
           (cvi-3
             (cvi/update cvi-2 '(z a b c) 3 cid1-set))
           (replaced-cvi
             (cvi/update cvi-3 '(a b) 3 cid1-set))
           (bound-empty
             (cvi/update empty-cvi '() 1 empty-set)))
      (assert
       (handler-case
           (progn
             (versioned-value/view
              empty-cvi empty-set nil 'slot)
             nil)
         (unbound-versioned-value () t)))
      (assert
       (equal (versioned-value/view
               cvi-1 cid1-set nil 'slot)
              '(a b c)))
      (assert (= (cvi/max-ion cvi-1) 3))
      (multiple-value-bind (ions bound-p)
          (cvi/active-ion-vector cvi-1 cid1-set)
        (assert bound-p)
        (assert (equalp ions #(0 1 2 3))))
      (assert
       (equal (versioned-value/view
               cvi-2 cid12-set nil 'slot)
              '(a x c d)))
      (assert
       (equal (versioned-value/view
               cvi-2 cid1-set nil 'slot)
              '(a b c)))
      (assert
       (equal (versioned-value/view
               cvi-3 cid13-set nil 'slot)
              '(z a b c)))
      (assert
       (equal (versioned-value/view
               cvi-3 all-set nil 'slot)
              '(z a x c d)))
      (assert
       (equal (versioned-value/view
               replaced-cvi cid13-set nil 'slot)
              '(a b)))
      (assert
       (equal (versioned-value/view
               cvi-3 cid13-set nil 'slot)
              '(z a b c)))
      (assert
       (equal (versioned-value/cid-list replaced-cvi)
              '(1 2 3)))
      (assert (versioned-value/contains-cid? replaced-cvi 2))
      (assert
       (= (versioned-value/most-recent-cid
           replaced-cvi cid13-set)
          3))
      (multiple-value-bind (record index)
          (cvi-find-cid-change-record replaced-cvi 2)
        (assert (= index 1))
        (assert (= (cvi-change-record/cid record) 2))
        (assert
         (plusp
          (length
           (cvi-change-record/insertion-records record))))
        (assert
         (plusp
          (length
           (cvi-change-record/deletion-records record)))))
      (multiple-value-bind (ions bound-p)
          (cvi/active-ion-vector bound-empty cid1-set)
        (assert bound-p)
        (assert (equalp ions #(0)))
        (assert
         (null
          (versioned-value/view
           bound-empty cid1-set nil 'slot))))
      (let ((default-cvi (make-cvi :initial-value nil)))
        (assert (cvi/default-allowed default-cvi))
        (assert
         (null
          (versioned-value/view
           default-cvi empty-set nil 'slot))))
      (assert
       (handler-case
           (progn
             (make-cvi-insertion-record 1 0 #())
             nil)
         (error () t)))
      (let* ((sha (versioned-value-sha cvi-3))
             (copy (versioned-value-from-sha sha)))
        (assert (typep copy 'cvi))
        (assert (string= sha (versioned-value-sha copy)))
        (assert
         (equal (versioned-value/view
                 copy all-set nil 'slot)
                '(z a x c d))))
      (format t "PASS~%"))

    ;; Test 19: Composite version files
    (format t "Test 19: Persistent composite version files... ")
    (let* ((empty-set (cid-set/empty "repository"))
           (cid1-set (cid-set/adjoin empty-set 1))
           (cid2-set (cid-set/adjoin empty-set 2))
           (both-set (cid-set/adjoin cid1-set 2))
           (file-1
             (make-cvfile
              :initial-value #("one") :cid 1
              :guid "guid-1" :fuid "file-1"))
           (file-2
             (cvfile/update file-1 #("one" "two") 2))
           (file-2-replaced
             (cvfile/update file-2 #("replacement") 2)))
      (assert (string= (cvfile/guid file-1) "guid-1"))
      (assert (string= (cvfile/fuid file-1) "file-1"))
      (assert
       (equalp
        (versioned-value/view file-1 cid1-set nil 'slot)
        #("one")))
      (assert
       (equalp
        (versioned-value/view file-2 both-set nil 'slot)
        #("one" "two")))
      (assert
       (equalp
        (versioned-value/view file-2 cid1-set nil 'slot)
        #("one")))
      (assert
       (equalp
        (versioned-value/view
         file-2-replaced cid2-set nil 'slot)
        #("replacement")))
      (assert
       (equalp
        (versioned-value/view file-2 cid2-set nil 'slot)
        #("one" "two")))
      (assert
       (equal (versioned-value/cid-list file-2-replaced)
              '(1 2)))
      (assert
       (equal (cvfile/map-cid-set
               file-2-replaced both-set)
              '(0 2)))
      (assert
       (= (versioned-value/most-recent-cid
           file-2-replaced both-set)
          2))
      (assert
       (versioned-value/contains-cid? file-2-replaced 1))
      (assert
       (handler-case
           (progn
             (versioned-value/view
              file-2-replaced empty-set nil 'slot)
             nil)
         (unbound-versioned-value () t)))
      (let ((same
              (handler-bind
                  ((versioned-object-no-change-condition
                     #'muffle-warning))
                (cvfile/update
                 file-2-replaced #("replacement") 2))))
        (assert
         (string= (versioned-value-sha same)
                  (versioned-value-sha file-2-replaced))))
      (let* ((sha (versioned-value-sha file-2-replaced))
             (copy (cvfile-from-sha sha)))
        (assert (string= sha (versioned-value-sha copy)))
        (assert
         (equalp
          (versioned-value/view copy both-set nil 'slot)
          #("replacement"))))
      (format t "PASS~%"))

    ;; Test 20: Persistent repository objects
    (format t "Test 20: Persistent repository objects... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "project"
              :type :master :parent "parent-project"))
           (root-value
             (make-persistent-vector
              2 :initial-contents '(root value)))
           (with-root
             (repository/add-locally-named-root
              repository root-value 'main))
           (with-satellite
             (repository/add-satellite-repository
              with-root "satellite-project")))
      (assert (string= (repository/domain repository)
                       "example.com"))
      (assert (string= (repository/name repository) "project"))
      (assert (eq (repository/type repository) :master))
      (assert (string= (repository/parent repository)
                       "parent-project"))
      (assert
       (equal (repository/identity repository)
              '("example.com" "project")))
      (assert
       (string= (repository-type-keyword->string-extension
                 :master)
                "ydm"))
      (assert (typep (repository/root-mapper repository)
                     'unordered-mapper))
      (assert (typep (repository/local-mapper repository)
                     'unordered-mapper))
      (assert (typep (repository/cid-mapper repository)
                     'ordered-mapper))
      (assert (= (repository/next-available-cid repository) 1))
      (assert (= (repository/last-allocated-cid repository) 0))
      (assert
       (null
        (nth-value
         1 (repository/locally-named-root repository 'main))))
      (multiple-value-bind (resolved present-p)
          (repository/locally-named-root with-root 'main)
        (assert present-p)
        (assert
         (equalp
          (persistent-vector->vector resolved)
          #(root value))))
      (assert
       (equal (repository/satellite-repositories with-satellite)
              '("satellite-project")))
      (multiple-value-bind (allocated cid cid-object)
          (repository/allocate-cid repository)
        (assert (= cid 1))
        (assert (= (repository/next-available-cid allocated) 2))
        (assert (= (repository/next-available-cid repository) 1))
        (assert
         (cid-object-equal-p
          cid-object
          (repository/resolve-distributed-identifier
           allocated
           (repository/cid-distributed-identifier
            allocated cid)))))
      (let* ((sha (repository-sha with-satellite))
             (copy (repository-from-sha sha)))
        (assert (string= sha (repository-sha copy)))
        (assert (string= (repository/name copy) "project"))
        (assert
         (equal (repository/satellite-repositories copy)
                '("satellite-project"))))
      (format t "PASS~%"))

    ;; Test 21: Repository-aware transactions
    (format t "Test 21: Repository-aware transactions... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "txn-project"))
           (value
             (make-scalar-versioned-value
              :initial-value :base))
           (changed-object
             (make-cid-object
              (make-distributed-identifier
               :domain "example.com" :repository "txn-project"
               :class :object :numeric-id 9)))
           (captured nil)
           (updated-value nil)
           (result
             (call-with-repository-transaction
              :repository repository
              :transaction-type :read-write
              :user-id-specifier :nobody
              :reason "first change"
              :cid-set-specifier :latest-version
              :receiver
              (lambda (transaction)
                (setf captured transaction)
                (assert (txn-for-update? transaction))
                (assert (= (repository-transaction/cid transaction)
                           1))
                (assert
                 (equal
                  (cid-set->list (transaction/cid-set transaction))
                  '(1)))
                (setf updated-value
                      (repository-transaction/update-versioned-value
                       transaction value :changed
                       changed-object 'value))
                (repository-transaction/note-change-set
                 transaction :change-set)
                :transaction-result))))
      (assert (eq result :transaction-result))
      (assert
       (eq (repository-transaction/status captured) :committed))
      (assert
       (eq
        (versioned-value/view
         updated-value
         (transaction/cid-set captured)
         changed-object 'value)
        :changed))
      (let ((committed
              (repository-transaction/repository captured)))
        (assert (not (string= (repository-sha repository)
                              (repository-sha committed))))
        (assert (repository/contains-cid? committed 1))
        (assert (= (repository/last-allocated-cid committed) 1))
        (assert (= (repository/next-available-cid committed) 2))
        (assert
         (cid-object-equal-p
          (repository-transaction/cid-object captured)
          (repository/resolve-distributed-identifier
           committed
           (repository/cid-distributed-identifier committed 1))))
        (multiple-value-bind (why comparison change basis)
            (repository/cid-information committed 1)
          (assert (string= why "first change"))
          (assert (integerp comparison))
          (assert (eq change :change-set))
          (assert (cid-set/empty? basis)))
        (let* ((entry
                 (cid-master-table/entry-for-cid
                  (repository/cid-master-table committed) 1))
               (detail
                 (cid-detail-table/find-entry
                  (cid-master-table-entry/cid-detail-table entry)
                  changed-object)))
          (assert detail)
          (assert
           (equalp
            (persistent-vector->vector
             (cid-detail-table-entry/slots-modified detail))
            #(value))))
        (let ((loaded nil))
          (call-with-transaction
           "reader" "load repository"
           (lambda (transaction)
             (setf loaded
                   (repository/load
                    transaction :error-if-missing t))))
          (assert
           (string= (repository-sha loaded)
                    (repository-sha committed))))
        (let* ((read-transaction nil)
              (read-result
                (call-with-repository-transaction
                 :transaction-type :read-only
                 :reason "read"
                 :cid-set-specifier :latest-version
                 :receiver
                 (lambda (transaction)
                   (setf read-transaction transaction)
                   (cid-set->list
                    (transaction/cid-set transaction))))))
          (assert (equal read-result '(1)))
          (assert
           (eq (repository-transaction/status read-transaction)
               :committed)))
        (let* ((before (cid-set/empty
                        (repository/identity committed)))
               (before-view nil)
               (after-view nil))
          (call-with-repository-transaction
           :repository committed
           :transaction-type :read-only-compare
           :reason "compare"
           :cid-set-specifier before
           :aux-cid-set-specifier :latest-version
           :receiver
           (lambda (transaction)
             (setf before-view
                   (call-with-before-view
                    (lambda ()
                      (cid-set->list
                       (transaction/cid-set transaction))))
                   after-view
                   (call-with-after-view
                    (lambda ()
                      (cid-set->list
                       (transaction/cid-set transaction)))))))
          (assert (null before-view))
          (assert (equal after-view '(1))))
        (let ((aborted
                (call-with-repository-transaction
                 :repository committed
                 :transaction-type :read-write
                 :reason "abort"
                 :cid-set-specifier :latest-version
                 :receiver
                 (lambda (transaction)
                   (repository-transaction/abort transaction)))))
          (assert
           (eq (repository-transaction/status aborted) :aborted))
          (assert
           (null
            (repository/txn-stack-for-uid-spec
             committed
             (repository-transaction/uid aborted)))))
        (assert (= (repository/last-allocated-cid committed) 1)))
      (format t "PASS~%"))

    ;; Test 22: Canonical objects and dense CID sets
    (format t "Test 22: Canonical objects and dense CID sets... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "canonical-project"))
           (did
             (make-distributed-identifier
              :domain "example.com" :repository "canonical-project"
              :class :object :numeric-id 7))
           (first (make-cid-object did :cid 7))
           (equivalent (make-cid-object did :cid 7)))
      (multiple-value-bind (canonical updated added-p)
          (repository/canonical-object-find-or-create
           repository first)
        (assert added-p)
        (assert (cid-object-equal-p canonical first))
        (assert (not (string= (repository-sha repository)
                              (repository-sha updated))))
        (multiple-value-bind (found unchanged second-added-p)
            (repository/canonical-object-find-or-create
             updated equivalent)
          (assert (not second-added-p))
          (assert (cid-object-equal-p found first))
          (assert (string= (repository-sha unchanged)
                           (repository-sha updated))))
        (let* ((dictionary
                 (repository/canonical-class-dictionary updated))
               (copy
                 (canonical-class-dictionary-from-sha
                  (canonical-class-dictionary-sha dictionary))))
          (assert
           (cid-object-equal-p
            (canonical-class-dictionary/find-object copy equivalent)
            first))))
      (let* ((set (list->cid-set '("example.com" "canonical-project")
                                 '(1 3 5)))
             (copy (cid-set-from-sha (cid-set-sha set))))
        (assert (typep set 'dense-cid-set))
        (assert (typep copy 'dense-cid-set))
        (assert (equal (cid-set->list copy) '(1 3 5))))
      (format t "PASS~%"))

    ;; Test 23: Integer range mappers
    (format t "Test 23: Persistent integer range mappers... ")
    (let* ((local-repository
             (make-repository
              :domain "example.com" :name "integer-local"))
           (remote-repository
             (make-repository
              :domain "remote.example" :name "integer-remote"))
           (local-map (repository/local-mapper local-repository))
           (remote-map (repository/local-mapper remote-repository))
           (mapper
             (make-integer-range-mapper
              :mapping-level "Widgets"
              :pseudo-class :widget
              :repository-mapper local-map)))
      (multiple-value-bind (local-mapper first)
          (integer-mapper/allocate-integers mapper 2 local-map)
        (assert (= first 1))
        (assert (= (integer-range-mapper/next-available-integer
                    local-mapper)
                   3))
        (assert (null (integer-range-mapper/entries local-mapper)))
        (multiple-value-bind (mixed-mapper remote-first)
            (integer-mapper/allocate-integers
             local-mapper 2 remote-map 50)
          (assert (= remote-first 3))
          (assert (= (integer-range-mapper/last-allocated-integer
                      mixed-mapper)
                     4))
          (assert (= (length (integer-range-mapper/entries
                              mixed-mapper))
                     1))
          (assert
           (= (integer-range-mapper/resolve-remote-reference
               mixed-mapper remote-map 51)
              4))
          (let ((did (integer-mapper/distributed-identifier
                      mixed-mapper 3)))
            (assert (string= (did/domain did) "remote.example"))
            (assert (string= (did/repository did) "integer-remote"))
            (assert (eq (did/class did) :widget))
            (assert (= (did/numeric-id did) 50))
            (assert
             (= (integer-mapper/resolve-distributed-identifier
                 mixed-mapper did remote-map)
                3)))
          (let ((copy
                  (integer-range-mapper-from-sha
                   (integer-mapper-sha mixed-mapper))))
            (assert (= (integer-range-mapper/next-available-integer
                        copy)
                       5))
            (assert
             (= (integer-range-mapper/resolve-remote-reference
                 copy remote-map 50)
                3))))))
      (format t "PASS~%"))

    ;; Test 24: Distributed core users
    (format t "Test 24: Persistent distributed core users... ")
    (let ((repository
            (make-repository
             :domain "example.com" :name "users-project")))
      (multiple-value-bind (with-first first)
          (make-core-user repository "Alice")
        (assert (= (distributed-object/numeric-id first) 1))
        (assert (string= (core-user/name first) "Alice"))
        (assert
         (string=
          (distributed-object-sha
           (repository/resolve-core-user
            with-first (distributed-object-identifier first)))
          (distributed-object-sha first)))
        (multiple-value-bind (with-second second)
            (make-core-user with-first "Bob")
          (assert (= (distributed-object/numeric-id second) 2))
          (assert (string= (core-user/name second) "Bob"))
          (let* ((copy
                   (repository-from-sha (repository-sha with-second)))
                 (resolved
                   (repository/resolve-core-user
                    copy (distributed-object-identifier second))))
            (assert (typep resolved 'core-user))
            (assert (string= (core-user/name resolved) "Bob"))))
      (format t "PASS~%"))

    ;; Test 25: Versioned standard objects
    (format t "Test 25: Versioned standard objects... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "versioned-object-project"))
           (standalone
             (make-instance
              'test-versioned-object
              :value :initial :ordinary :plain))
           (object nil))
      (assert (eq (slot-value standalone 'value) :initial))
      (assert (eq (slot-value standalone 'ordinary) :plain))
      (assert
       (typep (slot-value-unversioned standalone 'value)
              'scalar-versioned-value))
      (let ((update-without-transaction-signalled nil))
        (handler-case
            (setf (slot-value standalone 'value) :invalid)
          (error ()
            (setf update-without-transaction-signalled t)))
        (assert update-without-transaction-signalled))
      (call-with-repository-transaction
       :repository repository
       :transaction-type :read-write
       :reason "versioned slot update"
       :cid-set-specifier :latest-version
       :receiver
       (lambda (transaction)
         (declare (ignore transaction))
         (setf object
               (make-instance
                'test-versioned-object
                :value :initial :ordinary :plain))
         (setf (slot-value object 'value) :changed)
         (assert (eq (slot-value object 'value) :changed))
         (assert (typep (versioned-object/birth-cid-object object)
                        'cid-object))))
      (let ((visible-at-birth
              (list->cid-set
               (repository/identity repository) '(1)))
            (before-birth
              (cid-set/empty (repository/identity repository))))
        (assert
         (eq (call-with-cid-set-view
              before-birth
              (lambda (ignored)
                (declare (ignore ignored))
                (slot-value object 'value)))
             :initial))
        (assert
         (eq (call-with-cid-set-view
              visible-at-birth
              (lambda (ignored)
                (declare (ignore ignored))
                (slot-value object 'value)))
             :changed))
        (assert
         (equal
          (cid-set->list
           (versioned-object/cid-set repository object))
          '(1))))
      (format t "PASS~%"))

    ;; Test 26: Create a versioned standard object in one transaction and read it in another
    (format t "Test 26: Cons and read versioned object across transactions... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "crossing-tx-project"))
           (object nil))
      ;; First transaction: Create ("cons") the versioned object
      (call-with-repository-transaction
       :repository repository
       :transaction-type :read-write
       :reason "create versioned object"
       :cid-set-specifier :latest-version
       :receiver
       (lambda (transaction)
         (declare (ignore transaction))
         (setf object
               (make-instance
                'test-versioned-object
                :value :initial :ordinary :plain))
         (setf (slot-value object 'value) :changed-in-tx1)
         (assert (eq (slot-value object 'value) :changed-in-tx1))))
      
      ;; Second transaction: Read the versioned object slot and verify its value matches the state committed in tx1
      (call-with-repository-transaction
       :transaction-type :read-only
       :reason "read versioned object"
       :cid-set-specifier :latest-version
       :receiver
       (lambda (transaction)
         (declare (ignore transaction))
         (assert (eq (slot-value object 'value) :changed-in-tx1))))
      (format t "PASS~%"))

    ;; Test 27: Cons a versioned standard object with a distributed ID, then resolve it in another transaction
    (format t "Test 27: Cons and resolve distributed ID across transactions... ")
    (let* ((repository
             (make-repository
              :domain "example.com" :name "dist-crossing-project"))
           (did nil))
      ;; First transaction: Cons a versioned standard object (subclass of distributed-object) and return its distributed ID
      (call-with-repository-transaction
       :repository repository
       :transaction-type :read-write
       :reason "create distributed versioned object"
       :cid-set-specifier :latest-version
       :receiver
       (lambda (transaction)
         (let ((current-repo (repository-transaction/repository transaction)))
           (multiple-value-bind (updated-repo obj)
               (make-test-distributed-versioned-object current-repo :initial-val)
             (setf (repository-transaction/repository transaction) updated-repo)
             ;; Verify slot value
             (assert (eq (slot-value obj 'value) :initial-val))
             ;; Set slot value
             (setf (slot-value obj 'value) :updated-val)
             (assert (eq (slot-value obj 'value) :updated-val))
             ;; Return the distributed ID of the object
             (setf did (distributed-object-identifier obj))))))
      
      ;; Second transaction: Resolve the distributed ID back into the object and read its versioned slot value
      (call-with-repository-transaction
       :transaction-type :read-only
       :reason "resolve and read distributed versioned object"
       :cid-set-specifier :latest-version
       :receiver
       (lambda (transaction)
         ;; Resolve the distributed ID from the committed repository
         (let* ((committed-repo (repository-transaction/repository transaction))
                (resolved (repository/resolve-distributed-identifier committed-repo did)))
           (assert (typep resolved 'test-distributed-versioned-object))
           ;; Verify we resolved it back and we can query the slot correctly in the second transaction view!
           (assert (eq (slot-value resolved 'value) :updated-val)))))
      (format t "PASS~%")))

  ;; Cleanup test repo (ignore errors on Windows read-only files)
  (ignore-errors
   (uiop:delete-directory-tree *test-repo-path* :validate t :if-does-not-exist :ignore))
  (format t "All tests passed successfully!~%"))

(run-githack-tests)
