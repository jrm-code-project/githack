;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-vector-suite
  :in githack-suite
  :description "Tests for the PERSISTENT-VECTOR proxy, SERIALIZE-PERSISTENT-VECTOR, DESERIALIZE-PERSISTENT-VECTOR, and PERSISTENT-VECTOR-REF.")

(in-suite persistent-vector-suite)

(test persistent-vector-is-a-git-tree
  "A PERSISTENT-VECTOR is a GIT-TREE (so it reuses ENTRIES,
INFER-GIT-MODE, and SERIALIZE-TREE), defaulting to ELEMENT-TYPE T
and an unknown (NIL) LENGTH until either serialized or
deserialized."
  (let ((vector (make-instance 'persistent-vector :repository :dummy-repo)))
    (is (typep vector 'git-tree))
    (is (eq t (persistent-vector-element-type vector)))
    (is (null (persistent-vector-length vector)))
    (is (string= "40000" (infer-git-mode vector)))))

(test serialize-persistent-vector-empty
  "Serializing a vector with no index entries computes LENGTH 0 and
produces just the two standard \".meta\"/\"README.md\" entries, in
sorted order."
  (let ((vector (make-instance 'persistent-vector :repository :dummy-repo)))
    (with-fake-git-hash-object ()
      (let ((sha (serialize-persistent-vector vector)))
        (is (stringp sha))
        (is (string= sha (sha vector)))))
    (is (= 0 (persistent-vector-length vector)))
    (is (get-loaded? vector))
    (is (equal (list ".meta" "README.md") (mapcar #'car (get-entries vector))))))

(test serialize-persistent-vector-computes-length
  "Serializing a vector whose ENTRIES already hold \"0\", \"1\", and
\"2\" index entries computes LENGTH 3 from that count, persists each
element, and produces the standard five tree entries in sorted
order (note Git's strict lexicographic, not numeric, byte
ordering)."
  (let* ((e0 (make-instance 'git-blob :repository :dummy-repo :payload 0))
         (e1 (make-instance 'git-blob :repository :dummy-repo :payload 1))
         (e2 (make-instance 'git-blob :repository :dummy-repo :payload 2))
         (vector (make-instance 'persistent-vector :repository :dummy-repo
                                                    :entries (list (cons "0" e0)
                                                                   (cons "1" e1)
                                                                   (cons "2" e2)))))
    (with-fake-git-hash-object ()
      (serialize-persistent-vector vector))
    (is (= 3 (persistent-vector-length vector)))
    (is (get-loaded? vector))
    (is (equal (list ".meta" "README.md" "0" "1" "2") (mapcar #'car (get-entries vector))))
    (is (stringp (sha e0)))
    (is (stringp (sha e1)))
    (is (stringp (sha e2)))))

(test serialize-persistent-vector-nested-elements
  "Serializing a vector whose elements are themselves unpersisted
compound objects (a plain GIT-TREE and a PERSISTENT-CONS) recursively
persists them first. (A plain, non-persistent GIT-TREE is only ever
expected to arrive with its own entries already persisted -- see
%PERSIST-VECTOR-COMPONENT -- so LEAF is persisted here first, exactly
as SERIALIZE-TREE itself requires.)"
  (let* ((leaf (make-instance 'git-blob :repository :dummy-repo :payload :leaf)))
    (with-fake-git-hash-object ()
      (setf (sha leaf) (git-hash-object :dummy-repo "blob" (serialize-atom (get-payload leaf)))))
    (let* ((nested-tree (make-instance 'git-tree :repository :dummy-repo
                                                  :entries (list (cons "only" leaf))))
           (cons-car (make-instance 'git-blob :repository :dummy-repo :payload :a))
           (nested-cons (make-instance 'persistent-cons :repository :dummy-repo
                                                         :persistent-car cons-car
                                                         :persistent-cdr nil))
           (vector (make-instance 'persistent-vector :repository :dummy-repo
                                                      :entries (list (cons "0" nested-tree)
                                                                     (cons "1" nested-cons)))))
      (with-fake-git-hash-object ()
        (serialize-persistent-vector vector))
      (is (= 2 (persistent-vector-length vector)))
      (is (stringp (sha nested-tree)))
      (is (stringp (sha nested-cons)))
      (is (= 1 (persistent-cons-length nested-cons))))))

(test serialize-persistent-vector-is-idempotent
  "SERIALIZE-PERSISTENT-VECTOR does nothing (beyond returning the
existing SHA) for a vector that has already been persisted."
  (let* ((sha "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
         (vector (make-instance 'persistent-vector :repository :dummy-repo :sha sha)))
    (is (string= sha (serialize-persistent-vector vector)))
    (is (null (get-entries vector)))))

(test serialize-persistent-vector-writes-standard-readme-and-meta
  "SERIALIZE-PERSISTENT-VECTOR writes the exact, fixed README.md
markdown content as raw (non-atom-envelope) UTF-8 bytes. (Its
\".meta\" blob's content is verified indirectly, by the round-trip
test below via the exported DESERIALIZE-PERSISTENT-VECTOR.)"
  (let* ((calls '())
         (e0 (make-instance 'git-blob :repository :dummy-repo :payload 0))
         (vector (make-instance 'persistent-vector :repository :dummy-repo
                                                    :entries (list (cons "0" e0)))))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-vector vector))
    (is (find (sb-ext:string-to-octets +persistent-vector-readme+ :external-format :utf-8)
              calls :key #'third :test #'equalp))))

(test deserialize-persistent-vector-round-trips-with-serialize
  "Deserializing the tree bytes and .meta bytes produced by
SERIALIZE-PERSISTENT-VECTOR reconstructs an equivalent
PERSISTENT-VECTOR with the same LENGTH/ELEMENT-TYPE and hollow
element proxies for the same SHAs."
  (let* ((e0-sha "3333333333333333333333333333333333333333")
         (e1-sha "4444444444444444444444444444444444444444")
         (e0 (make-instance 'git-blob :repository :dummy-repo :payload 10 :sha e0-sha))
         (e1 (make-instance 'git-blob :repository :dummy-repo :payload 20 :sha e1-sha))
         (original (make-instance 'persistent-vector :repository :dummy-repo
                                                      :entries (list (cons "0" e0)
                                                                     (cons "1" e1))))
         (calls '()))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-vector original))
    (let* ((tree-octets (serialize-tree original))
           (meta-entry (cdr (assoc ".meta" (get-entries original) :test #'string=)))
           (readme-entry (cdr (assoc "README.md" (get-entries original) :test #'string=)))
           (meta-octets (third (find (sha meta-entry) calls
                                      :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                      :test #'string=)))
           (hollow (make-instance 'persistent-vector :repository :dummy-repo :sha (sha original))))
      (is (not (null meta-octets)))
      (with-fake-git-type ((list (cons e0-sha "blob")
                                  (cons e1-sha "blob")
                                  (cons (sha meta-entry) "blob")
                                  (cons (sha readme-entry) "blob")))
        (deserialize-persistent-vector hollow tree-octets meta-octets))
      (is (= 2 (persistent-vector-length hollow)))
      (is (eq t (persistent-vector-element-type hollow)))
      (is (get-loaded? hollow))
      (is (string= e0-sha (sha (cdr (assoc "0" (get-entries hollow) :test #'string=)))))
      (is (string= e1-sha (sha (cdr (assoc "1" (get-entries hollow) :test #'string=))))))))

(test deserialize-persistent-vector-signals-error-for-missing-entries
  "DESERIALIZE-PERSISTENT-VECTOR signals an error if the underlying
tree is missing its \".meta\" or \"README.md\" entry."
  (let* ((blob-sha "5555555555555555555555555555555555555555")
         (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
         (incomplete-tree (make-instance 'git-tree :repository :dummy-repo
                                                    :entries (list (cons "0" blob))))
         (tree-octets (serialize-tree incomplete-tree))
         (hollow (make-instance 'persistent-vector :repository :dummy-repo)))
    (with-fake-git-type ((list (cons blob-sha "blob")))
      (signals error (deserialize-persistent-vector hollow tree-octets #())))))

(test persistent-vector-ref-fetches-decodes-and-caches
  "PERSISTENT-VECTOR-REF, called against a hollow proxy whose tree
has not yet been fetched, parses the tree exactly once (via
%ENSURE-TREE-ENTRIES-LOADED), fetches and decodes each requested
element's own GIT-BLOB content independently, and caches every
result so a second call for the same index performs no further
Git I/O."
  (let* ((e0 (make-instance 'git-blob :repository :dummy-repo :payload :zero))
         (e1 (make-instance 'git-blob :repository :dummy-repo :payload :one))
         (original (make-instance 'persistent-vector :repository :dummy-repo
                                                      :entries (list (cons "0" e0)
                                                                     (cons "1" e1)))))
    (with-fake-git-object-store ()
      (serialize-persistent-vector original)
      (let* ((hollow (make-instance 'persistent-vector :repository :dummy-repo :sha (sha original)))
             (meta-sha (sha (cdr (assoc ".meta" (get-entries original) :test #'string=))))
             (readme-sha (sha (cdr (assoc "README.md" (get-entries original) :test #'string=)))))
        (with-fake-git-type ((list (cons (sha original) "tree")
                                    (cons meta-sha "blob")
                                    (cons readme-sha "blob")
                                    (cons (sha e0) "blob")
                                    (cons (sha e1) "blob")))
          (is (eq :zero (persistent-vector-ref hollow 0)))
          (is (eq :one (persistent-vector-ref hollow 1)))
          ;; Calling again must not error even though the tree has
          ;; already been parsed and both elements already cached.
          (is (eq :zero (persistent-vector-ref hollow 0)))
          (is (eq :one (persistent-vector-ref hollow 1))))))))

(test persistent-vector-ref-out-of-bounds-signals-without-io
  "PERSISTENT-VECTOR-REF signals an ordinary Lisp error immediately,
without any further Git access, for an out-of-bounds index against
an already-loaded vector -- including negative indices and indices
at or beyond LENGTH."
  (let ((vector (make-instance 'persistent-vector :repository :dummy-repo :length 2 :loaded? t)))
    (signals error (persistent-vector-ref vector -1))
    (signals error (persistent-vector-ref vector 2))
    (signals error (persistent-vector-ref vector 100))))

(test persistent-vector-ref-signals-when-length-unknown
  "PERSISTENT-VECTOR-REF signals an error for a vector whose LENGTH
has not yet been determined and which also has no SHA to fetch it
from (so it can never become loaded)."
  (let ((vector (make-instance 'persistent-vector :repository :dummy-repo)))
    (signals error (persistent-vector-ref vector 0))))

(test scan-persistent-vector-of-empty-vector-is-empty
  "SCAN-PERSISTENT-VECTOR of an already-loaded, zero-length vector
produces an empty series."
  (let ((vector (make-instance 'persistent-vector :repository :dummy-repo :length 0 :loaded? t)))
    (is (null (series:collect (scan-persistent-vector vector))))))

(test scan-persistent-vector-collects-decoded-elements-in-index-order
  "SCAN-PERSISTENT-VECTOR of an in-memory, already-loaded vector
produces a series of its elements in index order, each decoded
exactly as PERSISTENT-VECTOR-REF would return it."
  (let* ((e0 (make-instance 'git-blob :repository :dummy-repo :payload :zero :loaded? t))
         (e1 (make-instance 'git-blob :repository :dummy-repo :payload :one :loaded? t))
         (e2 (make-instance 'git-blob :repository :dummy-repo :payload :two :loaded? t))
         (vector (make-instance 'persistent-vector :repository :dummy-repo :length 3 :loaded? t
                                                    :entries (list (cons "0" e0)
                                                                   (cons "1" e1)
                                                                   (cons "2" e2)))))
    (is (equal '(:zero :one :two) (series:collect (scan-persistent-vector vector))))))

(test scan-persistent-vector-passes-through-compound-elements-unchanged
  "SCAN-PERSISTENT-VECTOR leaves a compound (non-GIT-BLOB) element's
own GIT-OBJECT proxy unchanged in the resulting series, exactly as
PERSISTENT-VECTOR-REF does for a nested PERSISTENT-CONS/
PERSISTENT-VECTOR/GIT-TREE element."
  (let* ((e0 (make-instance 'git-blob :repository :dummy-repo :payload :zero :loaded? t))
         (nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                                  :persistent-car e0
                                                  :persistent-cdr nil))
         (vector (make-instance 'persistent-vector :repository :dummy-repo :length 2 :loaded? t
                                                    :entries (list (cons "0" e0)
                                                                   (cons "1" nested)))))
    (is (equal (list :zero nested) (series:collect (scan-persistent-vector vector))))))

(test scan-persistent-vector-round-trips-through-a-fake-git-repository
  "SCAN-PERSISTENT-VECTOR correctly walks a vector that must be
lazily fetched from Git: after serializing an in-memory vector,
scanning a hollow PERSISTENT-VECTOR proxy for the same SHA (exactly
as PERSISTENT-VECTOR-REF-FETCHES-DECODES-AND-CACHES constructs its
own hollow proxy above, since INFLATE-GIT-PROXY itself has no way to
know a plain tree SHA denotes a PERSISTENT-VECTOR rather than an
ordinary GIT-TREE) yields the same decoded elements as the original
in-memory vector, in index order."
  (let* ((e0 (make-instance 'git-blob :repository :dummy-repo :payload 10))
         (e1 (make-instance 'git-blob :repository :dummy-repo :payload 20))
         (e2 (make-instance 'git-blob :repository :dummy-repo :payload 30))
         (original (make-instance 'persistent-vector :repository :dummy-repo
                                                      :entries (list (cons "0" e0)
                                                                     (cons "1" e1)
                                                                     (cons "2" e2)))))
    (with-fake-git-object-store ()
      (serialize-persistent-vector original)
      (let* ((hollow (make-instance 'persistent-vector :repository :dummy-repo :sha (sha original)))
             (meta-sha (sha (cdr (assoc ".meta" (get-entries original) :test #'string=))))
             (readme-sha (sha (cdr (assoc "README.md" (get-entries original) :test #'string=)))))
        (with-fake-git-type ((list (cons (sha original) "tree")
                                    (cons meta-sha "blob")
                                    (cons readme-sha "blob")
                                    (cons (sha e0) "blob")
                                    (cons (sha e1) "blob")
                                    (cons (sha e2) "blob")))
          (is (equal '(10 20 30) (series:collect (scan-persistent-vector hollow)))))))))
