;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests: unlike every other test file in this suite,
;;; these genuinely shell out to a real `git` executable (via
;;; WITH-TEMPORARY-GIT-REPOSITORY, in test-helpers.lisp) against a
;;; fresh, empty, real bare repository in a temporary directory --
;;; no GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/GIT-
;;; UPDATE-REF fake is ever installed here. Each test exercises one
;;; kind of persistent object across a small SET of real
;;; transactions (via CALL-WITH-REPOSITORY/WITH-TRANSACTION/CALL-
;;; WITH-TRANSACTION), verifying both that the value round-trips
;;; correctly and, where applicable, that untouched substructure is
;;; never re-persisted (structural sharing survives a real commit).

(def-suite end-to-end-suite
  :in githack-suite
  :description "End-to-end tests exercising real (non-faked) Git transactions, via a temporary bare repository, for every kind of persistent object GitHack supports.")

(in-suite end-to-end-suite)

(defparameter +e2e-author+ "Test Author <test@githack.local>")

(defun %e2e-fetch-tree-and-meta-octets (repository sha)
  "Return, as two values, the raw Git tree bytes for SHA and the raw
bytes of its own \".meta\" entry, fetched via GIT-CAT-FILE/
DESERIALIZE-TREE against the real REPOSITORY -- exactly the two
byte-vectors DESERIALIZE-PERSISTENT-CONS/-VECTOR/-ARRAY each require
of their own caller."
  (let* ((tree-octets (git-cat-file repository sha))
         (entries (deserialize-tree repository tree-octets))
         (meta-entry (assoc ".meta" entries :test #'string=)))
    (unless meta-entry
      (error "Malformed persistent tree ~S: missing \".meta\" entry." sha))
    (values tree-octets (git-cat-file repository (sha (cdr meta-entry))))))

(defun %e2e-load-persistent-cons (repository sha)
  "Return a fully loaded PERSISTENT-CONS proxy for SHA, fetched for
real from REPOSITORY."
  (let ((cons (make-instance 'persistent-cons :repository repository :sha sha)))
    (multiple-value-bind (tree-octets meta-octets) (%e2e-fetch-tree-and-meta-octets repository sha)
      (deserialize-persistent-cons cons tree-octets meta-octets))))

(defun %e2e-decode-blob (git-object)
  "Decode GIT-OBJECT (a GIT-BLOB, fetched for real via
%ENSURE-BLOB-LOADED if not already loaded) into its real Lisp
PAYLOAD."
  (get-payload (%ensure-blob-loaded git-object)))

(test end-to-end-atomic-value-round-trips-across-transactions
  "A bare atomic Lisp value (no GIT-OBJECT wrapping needed by the
caller) round-trips through CALL-WITH-TRANSACTION/WITH-TRANSACTION
across three real, successive read-write transactions against a
fresh repository: NIL for the very first (empty-branch) read, then
each previously committed value, automatically un/re-wrapped in an
ATOMIC-WRAPPER-TREE behind the scenes."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "atom" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (is (null value))
         42)
       (with-transaction (value) (repository :read-write)
         (is (eql 42 value))
         (1+ value))
       (with-transaction (value) (repository :read-write)
         (is (eql 43 value))
         value)))))

(test end-to-end-persistent-cons-round-trips-and-shares-structure
  "A PERSISTENT-CONS chain, built and committed as the root of a
real transaction, round-trips (values, LENGTH, PROPER) through a
fresh PERSISTENT-CONS proxy fetched for real from Git in a second
transaction; extending it in a third transaction reuses -- rather
than re-persists -- the untouched tail's own SHA, confirming
structural sharing survives real Git persistence."
  (with-temporary-git-repository (repository-path)
    (let (tail-sha)
      (call-with-repository
       repository-path
       :branch "main" :author +e2e-author+ :message "cons" :mode :read-write
       :receiver
       (lambda (repository)
         ;; Transaction 1: commit the proper list (1 2 3).
         (with-transaction (value) (repository :read-write)
           (declare (ignore value))
           (let* ((tail (make-instance 'persistent-cons :repository repository-path :loaded? t
                                        :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                  :payload 3 :loaded? t)
                                        :persistent-cdr nil))
                  (mid (make-instance 'persistent-cons :repository repository-path :loaded? t
                                       :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                 :payload 2 :loaded? t)
                                       :persistent-cdr tail))
                  (head (make-instance 'persistent-cons :repository repository-path :loaded? t
                                        :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                  :payload 1 :loaded? t)
                                        :persistent-cdr mid)))
             head))
         ;; Transaction 2: read the real root back, verify its shape,
         ;; then extend it with a new head node.
         (with-transaction (value) (repository :read-write)
           (let ((head (%e2e-load-persistent-cons repository-path (sha value))))
             (is (= 3 (persistent-cons-length head)))
             (is (eq t (persistent-cons-proper head)))
             (is (eql 1 (%e2e-decode-blob (persistent-car head))))
             (let ((mid (%e2e-load-persistent-cons repository-path (sha (persistent-cdr head)))))
               (is (eql 2 (%e2e-decode-blob (persistent-car mid))))
               (let ((tail (%e2e-load-persistent-cons repository-path (sha (persistent-cdr mid)))))
                 (is (eql 3 (%e2e-decode-blob (persistent-car tail))))
                 (is (null (%e2e-decode-blob (persistent-cdr tail))))
                 ;; Capture the tail's own SHA for transaction 3 to
                 ;; confirm it goes untouched (never re-persisted).
                 (setf tail-sha (sha tail))))
             (make-instance 'persistent-cons :repository repository-path :loaded? t
                             :persistent-car (make-instance 'git-blob :repository repository-path
                                                                       :payload 0 :loaded? t)
                             :persistent-cdr head)))
         ;; Transaction 3: verify the extended list, and that the
         ;; original (1 2 3) tail's SHA still has not changed.
         (with-transaction (value) (repository :read-write)
           (let* ((new-head (%e2e-load-persistent-cons repository-path (sha value)))
                  (head (%e2e-load-persistent-cons repository-path (sha (persistent-cdr new-head))))
                  (mid (%e2e-load-persistent-cons repository-path (sha (persistent-cdr head))))
                  (tail (%e2e-load-persistent-cons repository-path (sha (persistent-cdr mid)))))
             (is (= 4 (persistent-cons-length new-head)))
             (is (eql 0 (%e2e-decode-blob (persistent-car new-head))))
             (is (string= tail-sha (sha tail)))
             value)))))))

(test end-to-end-persistent-vector-lazily-fetches-real-elements
  "A PERSISTENT-VECTOR, built and committed as the root of a real
transaction, round-trips its own LENGTH and every individual element
through PERSISTENT-VECTOR-REF against a hollow proxy in a second,
independent transaction, exercising the entire real fetch stack
(tree fetch, \".meta\" fetch, per-index blob fetch)."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "vector" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'persistent-vector :repository repository-path :loaded? t
                         :entries (loop for i from 0 below 5
                                        collect (cons (princ-to-string i)
                                                      (make-instance 'git-blob :repository repository-path
                                                                               :payload (* i i) :loaded? t)))))
       (with-transaction (value) (repository :read-write)
         (let ((vector (make-instance 'persistent-vector :repository repository-path :sha (sha value))))
           (is (= 5 (persistent-vector-length (%ensure-persistent-vector-loaded vector))))
           (dotimes (i 5)
             (is (= (* i i) (persistent-vector-ref vector i)))))
         value)))))

(test end-to-end-persistent-array-computes-row-major-index-against-real-git
  "A 2x3 PERSISTENT-ARRAY, built and committed as the root of a real
transaction, round-trips every (I J) subscript pair through
PERSISTENT-ARRAY-REF against a hollow proxy in a second, independent
transaction, confirming row-major flattening survives real Git
persistence."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "array" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let ((data (make-instance 'persistent-vector :repository repository-path :loaded? t
                                     :entries (loop for i from 0 below 6
                                                     collect (cons (princ-to-string i)
                                                                   (make-instance 'git-blob :repository repository-path
                                                                                            :payload i :loaded? t))))))
           (make-instance 'persistent-array :repository repository-path
                           :dimensions '(2 3) :data data)))
       (with-transaction (value) (repository :read-write)
         (let ((array (make-instance 'persistent-array :repository repository-path :sha (sha value))))
           (dotimes (i 2)
             (dotimes (j 3)
               (is (= (+ (* i 3) j) (persistent-array-ref array i j))))))
         value)))))

(test end-to-end-persistent-object-round-trips-with-nested-compound-slot
  "A PERSISTENT-OBJECT instance (a PERSISTENT-OWNER, holding a nested
PERSISTENT-WIDGET) round-trips through DESERIALIZE-PERSISTENT-OBJECT
called on the real, freshly-fetched commit root of a second,
independent real transaction; accessing both the top-level LABEL
slot and the nested WIDGET's own NAME slot transparently triggers
SLOT-VALUE-USING-CLASS's real proxy resolution against actual Git
data."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "object" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'persistent-owner
                        :repository repository-path
                        :label "top-level-owner"
                        :widget (make-instance 'persistent-widget
                                                :repository repository-path
                                                :name "nested-widget"
                                                :tag :gadget)))
       (with-transaction (value) (repository :read-write)
         (let ((owner (deserialize-persistent-object value)))
           (is (string= "top-level-owner" (owner-label owner)))
           (is (string= "nested-widget" (widget-name (owner-widget owner))))
           (is (eq :gadget (widget-tag (owner-widget owner)))))
         value)))))

(test end-to-end-persistent-struct-round-trips-through-real-git
  "A DEFINE-PERSISTENT-STRUCT instance (STRUCT-WIDGET) round-trips
through DESERIALIZE-PERSISTENT-OBJECT called on the real commit root
of a second, independent real transaction."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "struct" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'struct-widget :repository repository-path :id 7 :name "Widget-7" :serial-number 12345))
       (with-transaction (value) (repository :read-write)
         (let ((widget (deserialize-persistent-object value)))
           (is (struct-widget-p widget))
           (is (= 7 (sw-id widget)))
           (is (string= "Widget-7" (sw-name widget)))
           (is (= 12345 (sw-serial-number widget))))
         value)))))

(test end-to-end-persistent-hash-table-round-trips-through-real-git
  "A PERSISTENT-HASH-TABLE, built via PHASH-MAKE/PHASH-PUT and
committed as the root of a real transaction, round-trips every one
of its associations -- via PHASH-GET against a fresh
DESERIALIZE-PERSISTENT-OBJECT instance in a second, independent
transaction -- and correctly reports a missing key as absent."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "hash-table" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let* ((table (phash-make :repository repository-path :test 'equal :size 4)))
           (setf table (phash-put "one" 1 table))
           (setf table (phash-put "two" 2 table))
           (setf table (phash-put "three" 3 table))
           table))
       (with-transaction (value) (repository :read-write)
         (let ((table (deserialize-persistent-object value)))
           (is (= 3 (persistent-hash-table-count table)))
           (multiple-value-bind (v found?) (phash-get "one" table) (is (eql 1 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "two" table) (is (eql 2 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "three" table) (is (eql 3 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "missing" table 'default) (is (eq 'default v)) (is (eq nil found?))))
         value)))))

(test end-to-end-persistent-hash-table-round-trips-a-nested-persistent-object-value
  "A PERSISTENT-HASH-TABLE whose VALUEs are themselves nested
PERSISTENT-OBJECT instances (a STRUCT-WIDGET, stored via a
PERSISTENT-CONS collision chain inside the table's PERSISTENT-VECTOR
of buckets) round-trips correctly through a real transaction:
PHASH-GET, against a fresh DESERIALIZE-PERSISTENT-OBJECT instance in
a second, independent transaction, must itself be
DESERIALIZE-PERSISTENT-OBJECT'd again to recover the nested
STRUCT-WIDGET's own slots -- exercising %PERSIST-CONS-COMPONENT-BY-
TYPE/%PERSIST-VECTOR-COMPONENT-BY-TYPE's PERSISTENT-OBJECT methods
(persistent-standard-class.lisp), without which the nested value
would silently lose its \".meta\" entry and so its real class on
read-back."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "hash-table-nested-object" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let* ((widget (make-instance 'struct-widget :repository repository-path
                                        :id 7 :name "Widget-7" :serial-number 12345))
                (table (phash-make :repository repository-path :test 'equal)))
           (phash-put "widget" widget table)))
       (with-transaction (value) (repository :read-write)
         (let* ((table (deserialize-persistent-object value)))
           (multiple-value-bind (raw-widget found?) (phash-get "widget" table)
             (is (eq t found?))
             (let ((widget (deserialize-persistent-object raw-widget)))
               (is (struct-widget-p widget))
               (is (= 7 (sw-id widget)))
               (is (string= "Widget-7" (sw-name widget)))
               (is (= 12345 (sw-serial-number widget))))))
         value)))))
