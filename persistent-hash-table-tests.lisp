;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-hash-table-suite
  :in githack-suite
  :description "Tests for PERSISTENT-HASH-TABLE and its PHASH-MAKE/PHASH-GET/PHASH-PUT/PHASH-REMOVE operations.")

(in-suite persistent-hash-table-suite)

(test phash-make-creates-an-empty-table
  "PHASH-MAKE returns an empty PERSISTENT-HASH-TABLE: COUNT 0, TEST
normalized to the requested symbol, and a BUCKETS vector of the
requested SIZE."
  (let ((table (phash-make :repository :dummy-repo :test 'eql :size 4)))
    (is (persistent-hash-table-p table))
    (is (= 0 (persistent-hash-table-count table)))
    (is (eq 'eql (persistent-hash-table-test table)))
    (is (= 4 (persistent-vector-length (persistent-hash-table-buckets table))))))

(test phash-make-signals-error-for-non-positive-size
  "PHASH-MAKE rejects a SIZE that is not a positive integer, rather
than deferring failure to a later division/mod error inside
%PHASH-HASH."
  (signals invalid-argument-error (phash-make :repository :dummy-repo :size 0))
  (signals invalid-argument-error (phash-make :repository :dummy-repo :size -1))
  (signals invalid-argument-error (phash-make :repository :dummy-repo :size "4")))

(test phash-make-signals-error-for-test-not-naming-a-function
  "PHASH-MAKE rejects a TEST symbol that does not name a callable
function, rather than deferring failure to a later undefined-
function error from FDEFINITION."
  (signals invalid-argument-error
    (phash-make :repository :dummy-repo :test '%not-a-real-function-name-at-all)))

(test phash-get-on-empty-table-returns-default
  "PHASH-GET on a table with no associations returns (VALUES DEFAULT
NIL)."
  (let ((table (phash-make :repository :dummy-repo)))
    (multiple-value-bind (value found?) (phash-get :missing table :not-found)
      (is (eq :not-found value))
      (is (not found?)))))

(test phash-put-then-get-round-trips
  "PHASH-PUT associates a key with a value in a freshly-returned
table; PHASH-GET on that new table finds it, with T as its second
value."
  (let* ((table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :a 1 table)))
    (multiple-value-bind (value found?) (phash-get :a table2)
      (is (= 1 value))
      (is (eq t found?)))
    (is (= 1 (persistent-hash-table-count table2)))))

(test phash-put-does-not-mutate-the-original-table
  "PHASH-PUT never mutates its TABLE argument: the original table
still has no associations and a zero COUNT after PHASH-PUT returns a
new table."
  (let* ((table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :a 1 table)))
    (declare (ignore table2))
    (multiple-value-bind (value found?) (phash-get :a table)
      (declare (ignore value))
      (is (not found?)))
    (is (= 0 (persistent-hash-table-count table)))))

(test phash-put-replaces-an-existing-key-without-changing-count
  "PHASH-PUT for a key that already exists replaces its value but
leaves COUNT unchanged."
  (let* ((table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :a 1 table))
         (table3 (phash-put :a 2 table2)))
    (multiple-value-bind (value found?) (phash-get :a table3)
      (is (= 2 value))
      (is (eq t found?)))
    (is (= 1 (persistent-hash-table-count table3)))))

(test phash-put-handles-multiple-keys-in-the-same-bucket
  "Several distinct keys that happen to hash into the same bucket
(forced here via SIZE 1) are all independently retrievable."
  (let* ((table (phash-make :repository :dummy-repo :test 'eql :size 1))
         (table2 (phash-put :a 1 table))
         (table3 (phash-put :b 2 table2))
         (table4 (phash-put :c 3 table3)))
    (is (= 1 (phash-get :a table4)))
    (is (= 2 (phash-get :b table4)))
    (is (= 3 (phash-get :c table4)))))

(test phash-put-honors-the-equal-test
  "A table created with TEST 'EQUAL compares keys via EQUAL, so two
distinct (but EQUAL) strings refer to the same association."
  (let* ((table (phash-make :repository :dummy-repo :test 'equal :size 4))
         (table2 (phash-put (copy-seq "hello") 1 table)))
    (multiple-value-bind (value found?) (phash-get (copy-seq "hello") table2)
      (is (= 1 value))
      (is (eq t found?)))))

(test phash-remove-deletes-an-existing-key
  "PHASH-REMOVE for a key present in TABLE returns a new table
without that key, with COUNT decremented."
  (let* ((table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :a 1 table))
         (table3 (phash-put :b 2 table2))
         (table4 (phash-remove :a table3)))
    (multiple-value-bind (value found?) (phash-get :a table4)
      (is (eq nil value))
      (is (not found?)))
    (is (= 2 (phash-get :b table4)))
    (is (= 1 (persistent-hash-table-count table4)))))

(test phash-remove-of-a-missing-key-returns-the-same-table
  "PHASH-REMOVE for a key not present in TABLE returns TABLE itself,
unchanged (EQ), rather than allocating a needless copy."
  (let* ((table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :a 1 table)))
    (is (eq table2 (phash-remove :missing table2)))))

(test phash-put-rehashes-when-load-factor-exceeds-one
  "Inserting more distinct keys than there are buckets automatically
grows BUCKETS (doubling it), while every previously-inserted
association remains retrievable."
  (let ((table (phash-make :repository :dummy-repo :size 2)))
    (dotimes (i 10)
      (setf table (phash-put i (* i i) table)))
    (is (= 10 (persistent-hash-table-count table)))
    (is (> (persistent-vector-length (persistent-hash-table-buckets table)) 2))
    (dotimes (i 10)
      (multiple-value-bind (value found?) (phash-get i table)
        (is (eq t found?))
        (is (= (* i i) value))))))

(test phash-put-can-store-a-nested-persistent-object-as-a-value
  "A value that is already a GIT-OBJECT proxy (here, a nested,
already-loaded PERSISTENT-HASH-TABLE) is stored directly, not
wrapped in a further GIT-BLOB, and PHASH-GET returns that same proxy
object, EQ, rather than attempting to decode it as an atom."
  (let* ((nested (phash-make :repository :dummy-repo :size 2))
         (table (phash-make :repository :dummy-repo :size 4))
         (table2 (phash-put :nested nested table)))
    (is (eq nested (phash-get :nested table2)))))

(test phash-map-visits-every-association-exactly-once
  "PHASH-MAP calls its FUNCTION once per association currently in
TABLE, each time with that association's own (already-decoded) KEY
and VALUE, regardless of how many buckets those associations happen
to land in."
  (let ((table (phash-make :repository :dummy-repo :size 2))
        (seen '()))
    (dotimes (i 5)
      (setf table (phash-put i (* i i) table)))
    (phash-map (lambda (key value) (push (cons key value) seen)) table)
    (is (= 5 (length seen)))
    (dotimes (i 5)
      (is (= (* i i) (cdr (assoc i seen)))))))

(test phash-map-on-empty-table-calls-function-zero-times
  "PHASH-MAP on a table with no associations never calls FUNCTION."
  (let ((table (phash-make :repository :dummy-repo :size 4))
        (calls 0))
    (phash-map (lambda (key value) (declare (ignore key value)) (incf calls)) table)
    (is (= 0 calls))))
