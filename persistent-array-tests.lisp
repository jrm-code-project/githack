;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-array-suite
  :in githack-suite
  :description "Tests for the PERSISTENT-ARRAY proxy, SERIALIZE-PERSISTENT-ARRAY, DESERIALIZE-PERSISTENT-ARRAY, and PERSISTENT-ARRAY-REF.")

(in-suite persistent-array-suite)

(test persistent-array-is-a-git-tree
  "A PERSISTENT-ARRAY is a GIT-TREE (so it reuses ENTRIES,
INFER-GIT-MODE, and SERIALIZE-TREE), defaulting to ELEMENT-TYPE T
and unknown (NIL) DIMENSIONS/DATA until either serialized or
deserialized."
  (let ((array (make-instance 'persistent-array :repository :dummy-repo)))
    (is (typep array 'git-tree))
    (is (eq t (persistent-array-element-type array)))
    (is (null (persistent-array-dimensions array)))
    (is (string= "40000" (infer-git-mode array)))))

(defun %make-persistent-vector-of (n)
  "Build an unpersisted PERSISTENT-VECTOR whose N index entries are
GIT-BLOBs holding the integers 0..N-1, in order -- a convenience for
PERSISTENT-ARRAY-TESTS below."
  (make-instance 'persistent-vector :repository :dummy-repo
                                     :entries (loop for i from 0 below n
                                                     collect (cons (princ-to-string i)
                                                                   (make-instance 'git-blob
                                                                                  :repository :dummy-repo
                                                                                  :payload i)))))

(test serialize-persistent-array-flattens-row-major
  "Serializing a 2x3 persistent array persists its underlying
6-element PERSISTENT-VECTOR (via SERIALIZE-PERSISTENT-VECTOR),
computes its own DIMENSIONS-implied volume, and produces the
standard three tree entries in sorted order."
  (let* ((data (%make-persistent-vector-of 6))
         (array (make-instance 'persistent-array :repository :dummy-repo
                                                  :dimensions '(2 3)
                                                  :data data)))
    (with-fake-git-hash-object ()
      (let ((sha (serialize-persistent-array array)))
        (is (stringp sha))
        (is (string= sha (sha array)))))
    (is (get-loaded? array))
    (is (= 6 (persistent-vector-length data)))
    (is (equal (list ".meta" "README.md" "data") (mapcar #'car (get-entries array))))
    (is (eq data (cdr (assoc "data" (get-entries array) :test #'string=))))))

(test serialize-persistent-array-requires-dimensions
  "SERIALIZE-PERSISTENT-ARRAY signals an error when DIMENSIONS has
not been set (or is otherwise invalid)."
  (let ((array (make-instance 'persistent-array :repository :dummy-repo
                                                 :data (%make-persistent-vector-of 1))))
    (with-fake-git-hash-object ()
      (signals error (serialize-persistent-array array)))))

(test serialize-persistent-array-requires-data
  "SERIALIZE-PERSISTENT-ARRAY signals an error when DATA (the
underlying persistent-vector) has not been set."
  (let ((array (make-instance 'persistent-array :repository :dummy-repo :dimensions '(2 2))))
    (with-fake-git-hash-object ()
      (signals error (serialize-persistent-array array)))))

(test serialize-persistent-array-signals-error-for-volume-mismatch
  "SERIALIZE-PERSISTENT-ARRAY signals an error when its DATA
vector's actual element count does not match the volume implied by
DIMENSIONS."
  (let* ((data (%make-persistent-vector-of 5))
         (array (make-instance 'persistent-array :repository :dummy-repo
                                                  :dimensions '(2 3)
                                                  :data data)))
    (with-fake-git-hash-object ()
      (signals error (serialize-persistent-array array)))))

(test serialize-persistent-array-is-idempotent
  "SERIALIZE-PERSISTENT-ARRAY does nothing (beyond returning the
existing SHA) for an array that has already been persisted."
  (let* ((sha "ffffffffffffffffffffffffffffffffffffffff")
         (array (make-instance 'persistent-array :repository :dummy-repo :sha sha)))
    (is (string= sha (serialize-persistent-array array)))
    (is (null (get-entries array)))))

(test serialize-persistent-array-writes-standard-readme-and-meta
  "SERIALIZE-PERSISTENT-ARRAY writes the exact, fixed README.md
markdown content as raw (non-atom-envelope) UTF-8 bytes. (Its
\".meta\" blob's content is verified indirectly, by the round-trip
test below via the exported DESERIALIZE-PERSISTENT-ARRAY.)"
  (let* ((calls '())
         (data (%make-persistent-vector-of 1))
         (array (make-instance 'persistent-array :repository :dummy-repo
                                                  :dimensions '(1)
                                                  :data data)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-array array))
    (is (find (sb-ext:string-to-octets +persistent-array-readme+ :external-format :utf-8)
              calls :key #'third :test #'equalp))))

(test deserialize-persistent-array-round-trips-with-serialize
  "Deserializing the tree bytes and .meta bytes produced by
SERIALIZE-PERSISTENT-ARRAY reconstructs an equivalent
PERSISTENT-ARRAY with the same DIMENSIONS/ELEMENT-TYPE, and a hollow
PERSISTENT-VECTOR proxy (not a generic GIT-TREE) for the same DATA
SHA."
  (let* ((data (%make-persistent-vector-of 4))
         (original (make-instance 'persistent-array :repository :dummy-repo
                                                     :dimensions '(2 2)
                                                     :data data))
         (calls '()))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-array original))
    (let* ((tree-octets (serialize-tree original))
           (meta-entry (cdr (assoc ".meta" (get-entries original) :test #'string=)))
           (readme-entry (cdr (assoc "README.md" (get-entries original) :test #'string=)))
           (data-sha (sha data))
           (meta-octets (third (find (sha meta-entry) calls
                                      :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                      :test #'string=)))
           (hollow (make-instance 'persistent-array :repository :dummy-repo :sha (sha original))))
      (is (not (null meta-octets)))
      (with-fake-git-type ((list (cons data-sha "tree")
                                  (cons (sha meta-entry) "blob")
                                  (cons (sha readme-entry) "blob")))
        (deserialize-persistent-array hollow tree-octets meta-octets))
      (is (equal '(2 2) (persistent-array-dimensions hollow)))
      (is (eq t (persistent-array-element-type hollow)))
      (is (get-loaded? hollow))
      (is (typep (%persistent-array-data hollow) 'persistent-vector))
      (is (string= data-sha (sha (%persistent-array-data hollow)))))))

(test deserialize-persistent-array-signals-error-for-missing-entries
  "DESERIALIZE-PERSISTENT-ARRAY signals an error if the underlying
tree is missing its \".meta\", \"README.md\", or \"data\" entry."
  (let* ((blob-sha "6666666666666666666666666666666666666666")
         (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
         (incomplete-tree (make-instance 'git-tree :repository :dummy-repo
                                                    :entries (list (cons "README.md" blob))))
         (tree-octets (serialize-tree incomplete-tree))
         (hollow (make-instance 'persistent-array :repository :dummy-repo)))
    (with-fake-git-type ((list (cons blob-sha "blob")))
      (signals error (deserialize-persistent-array hollow tree-octets #())))))

(test persistent-array-ref-computes-row-major-index-and-delegates
  "PERSISTENT-ARRAY-REF, called against a hollow proxy for a 2x3
array, loads ARRAY's own tree/meta at most once, computes the
correct flat row-major index for each set of subscripts, and
delegates the actual fetch to the underlying PERSISTENT-VECTOR-REF,
returning the same decoded values SERIALIZE-PERSISTENT-VECTOR
originally stored at each flattened position."
  (let* ((data (%make-persistent-vector-of 6))
         (original (make-instance 'persistent-array :repository :dummy-repo
                                                     :dimensions '(2 3)
                                                     :data data)))
    (with-fake-git-object-store ()
      (serialize-persistent-array original)
      (let* ((hollow (make-instance 'persistent-array :repository :dummy-repo :sha (sha original)))
             (meta-sha (sha (cdr (assoc ".meta" (get-entries original) :test #'string=))))
             (readme-sha (sha (cdr (assoc "README.md" (get-entries original) :test #'string=))))
             (data-meta-sha (sha (cdr (assoc ".meta" (get-entries data) :test #'string=))))
             (data-readme-sha (sha (cdr (assoc "README.md" (get-entries data) :test #'string=)))))
        (with-fake-git-type ((list (cons (sha original) "tree")
                                    (cons meta-sha "blob")
                                    (cons readme-sha "blob")
                                    (cons (sha data) "tree")
                                    (cons data-meta-sha "blob")
                                    (cons data-readme-sha "blob")
                                    (cons (sha (cdr (assoc "0" (get-entries data) :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "1" (get-entries data) :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "2" (get-entries data) :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "3" (get-entries data) :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "4" (get-entries data) :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "5" (get-entries data) :test #'string=))) "blob")))
          ;; Row-major: index(i, j) = i*3 + j, so (1, 2) -> flat 5,
          ;; whose original blob payload was 5.
          (is (= 0 (persistent-array-ref hollow 0 0)))
          (is (= 1 (persistent-array-ref hollow 0 1)))
          (is (= 5 (persistent-array-ref hollow 1 2)))
          (is (= 3 (persistent-array-ref hollow 1 0))))))))

(test persistent-array-ref-signals-for-wrong-subscript-count
  "PERSISTENT-ARRAY-REF signals an error when given the wrong number
of subscripts for an already-loaded array."
  (let* ((data (%make-persistent-vector-of 6))
         (array (make-instance 'persistent-array :repository :dummy-repo
                                                  :dimensions '(2 3)
                                                  :data data
                                                  :loaded? t)))
    (signals error (persistent-array-ref array 0))
    (signals error (persistent-array-ref array 0 0 0))))

(test persistent-array-ref-signals-for-out-of-bounds-subscript
  "PERSISTENT-ARRAY-REF signals an error for any subscript out of
bounds for its own dimension, for an already-loaded array."
  (let* ((data (%make-persistent-vector-of 6))
         (array (make-instance 'persistent-array :repository :dummy-repo
                                                  :dimensions '(2 3)
                                                  :data data
                                                  :loaded? t)))
    (signals error (persistent-array-ref array 2 0))
    (signals error (persistent-array-ref array 0 3))
    (signals error (persistent-array-ref array -1 0))))
