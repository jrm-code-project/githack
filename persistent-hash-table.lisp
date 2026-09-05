;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-HASH-TABLE implements an immutable hash table entirely
;;; in terms of GitHack's existing PERSISTENT-VECTOR and
;;; PERSISTENT-CONS infrastructure -- it introduces no bespoke Git
;;; serialization of its own. A PERSISTENT-HASH-TABLE is simply a
;;; DEFINE-PERSISTENT-STRUCT with three slots (TEST, COUNT, BUCKETS),
;;; so it automatically gets standard CLOS-tree serialization/
;;; deserialization for free via PERSISTENT-STANDARD-CLASS.
;;;
;;; BUCKETS is a PERSISTENT-VECTOR whose Nth element is either NIL
;;; (an empty bucket) or the head of a PERSISTENT-CONS chain: each
;;; node's own CAR is a further, dotted-pair PERSISTENT-CONS (itself
;;; CAR = key, CDR = value), and each node's own CDR is either the
;;; next node in the chain or NIL to terminate it -- exactly the same
;;; shape a plain Lisp ((k1 . v1) (k2 . v2) ...) alist would have,
;;; mapped 1:1 onto persistent conses.
;;;
;;; Every write operation (PHASH-PUT, PHASH-REMOVE) returns a
;;; brand-new PERSISTENT-HASH-TABLE, sharing as much of TABLE's own
;;; PERSISTENT-CONS/PERSISTENT-VECTOR structure as possible; TABLE
;;; itself is never mutated.

(define-persistent-struct persistent-hash-table
  (test 'eql)
  (count 0)
  (buckets nil))

(defparameter +persistent-hash-table-default-bucket-count+ 8
  "The number of buckets PHASH-MAKE allocates when the caller does
not request a specific SIZE.")

(defgeneric %normalize-hash-test (test)
  (:documentation
   "Return TEST as a symbol suitable for PERSISTENT-HASH-TABLE-TEST:
TEST itself, if already a symbol (e.g. 'EQL, 'EQUAL); or, if TEST is
a function object, the symbol naming it, via
FUNCTION-LAMBDA-EXPRESSION's third value. Signals an error if TEST is
a function whose name cannot be determined this way, since an
unnamed function is not a serializable atom -- callers should pass a
symbol (e.g. 'EQUAL) instead of #'EQUAL."))

(defmethod %normalize-hash-test ((test symbol))
  (unless (fboundp test)
    (error 'invalid-argument-error
           :format-control "TEST ~S does not name a callable function."
           :format-arguments (list test)))
  test)

(defmethod %normalize-hash-test ((test function))
  (or (nth-value 2 (function-lambda-expression test))
      (error 'invalid-argument-error
             :format-control "Cannot determine a symbol name for the function ~S; pass TEST as a symbol (e.g. 'EQUAL) instead."
             :format-arguments (list test))))

(defun %phash-test-function (table)
  "Return the two-argument equality predicate function named by
TABLE's own TEST slot (a symbol, e.g. EQL, EQUAL, EQUALP)."
  (fdefinition (persistent-hash-table-test table)))

(defun %phash-hash (key)
  "Return an integer hash code for KEY, used by PHASH-GET/PHASH-PUT/
PHASH-REMOVE to select KEY's bucket index."
  (sxhash key))

(defun %phash-bucket-index (key bucket-count)
  "Return KEY's bucket index within a BUCKETS vector of BUCKET-COUNT
elements."
  (mod (%phash-hash key) bucket-count))

(defun %phash-decode (git-object)
  "Return the real Lisp value GIT-OBJECT represents: its decoded
PAYLOAD, if GIT-OBJECT is a GIT-BLOB (fetching it from the repository
first via %ENSURE-BLOB-LOADED, if not yet loaded); or GIT-OBJECT
itself, unchanged, for any other (compound) GIT-OBJECT proxy."
  (if (typep git-object 'git-blob)
      (get-payload (%ensure-blob-loaded git-object))
      git-object))

(defun %phash-wrap (repository value)
  "Return VALUE unchanged if it is already a GIT-OBJECT proxy (e.g.
a nested PERSISTENT-VECTOR, PERSISTENT-CONS, or PERSISTENT-OBJECT);
otherwise, return a fresh, already-loaded GIT-BLOB, associated with
REPOSITORY, wrapping VALUE as a serializable atom."
  (if (typep value 'git-object)
      value
      (make-instance 'git-blob :repository repository :payload value :loaded? t)))

(defun %make-pair-node (repository key value)
  "Return a fresh, already-loaded, dotted-pair PERSISTENT-CONS
representing one (KEY . VALUE) association: its own PERSISTENT-CAR
holds KEY and its own PERSISTENT-CDR holds VALUE, each wrapped via
%PHASH-WRAP."
  (make-instance 'persistent-cons
                 :repository repository
                 :loaded? t
                 :persistent-car (%phash-wrap repository key)
                 :persistent-cdr (%phash-wrap repository value)))

(defun %make-list-node (repository head-pair tail)
  "Return a fresh, already-loaded PERSISTENT-CONS bucket-chain node:
its own PERSISTENT-CAR is HEAD-PAIR (a dotted-pair PERSISTENT-CONS
for one key/value association, from %MAKE-PAIR-NODE), and its own
PERSISTENT-CDR is TAIL (the next node in the chain, or NIL to
terminate it)."
  (make-instance 'persistent-cons
                 :repository repository
                 :loaded? t
                 :persistent-car head-pair
                 :persistent-cdr tail))

(defun %make-empty-buckets (repository n)
  "Return a fresh, already-loaded PERSISTENT-VECTOR of N elements,
every element NIL (an empty bucket). Both the in-memory cache and the
real ENTRIES alist (each index wrapped via %PHASH-WRAP, so an empty
bucket becomes a GIT-BLOB with a NIL payload) are populated, so this
vector serializes correctly via SERIALIZE-PERSISTENT-VECTOR, which
reads ENTRIES exclusively -- not the cache."
  (let* ((entries (loop for i from 0 below n
                         collect (cons (princ-to-string i) (%phash-wrap repository nil))))
         (vector (make-instance 'persistent-vector
                                 :repository repository
                                 :length n
                                 :element-type t
                                 :loaded? t
                                 :entries entries)))
    (setf (%persistent-vector-cache vector) (make-array n :initial-element nil))
    vector))

(defun %persistent-vector-copy-with (vector index new-value)
  "Return a new, already-loaded PERSISTENT-VECTOR of the same LENGTH,
ELEMENT-TYPE, and REPOSITORY as VECTOR, whose cache is a fresh copy of
VECTOR's own cache with NEW-VALUE substituted at INDEX -- every other
index's cached value (or not-yet-fetched status) is preserved
unchanged. VECTOR's own ENTRIES (if any -- e.g. if VECTOR was freshly
loaded from Git) are likewise copied, with only INDEX's own entry
replaced by NEW-VALUE (wrapped via %PHASH-WRAP), so every other
index's entry -- including any not-yet-resolved lazy proxy -- remains
shared, unmodified, with VECTOR, and the result still serializes
correctly via SERIALIZE-PERSISTENT-VECTOR (which reads ENTRIES
exclusively, not the cache). VECTOR itself is never modified."
  (%ensure-persistent-vector-loaded vector)
  (let* ((length (persistent-vector-length vector))
         (repository (get-repository vector))
         (old-cache (or (%persistent-vector-cache vector)
                        (make-array length :initial-element +persistent-vector-unloaded+)))
         (new-cache (copy-seq old-cache))
         (index-string (princ-to-string index))
         (old-entries (get-entries vector))
         (new-entries (mapcar (lambda (entry)
                                 (if (string= (car entry) index-string)
                                     (cons index-string (%phash-wrap repository new-value))
                                     entry))
                               old-entries))
         (new-vector (make-instance 'persistent-vector
                                     :repository repository
                                     :length length
                                     :element-type (persistent-vector-element-type vector)
                                     :loaded? t
                                     :entries new-entries)))
    (setf (svref new-cache index) new-value)
    (setf (%persistent-vector-cache new-vector) new-cache)
    new-vector))

(defun %ensure-persistent-cons-loaded (cons)
  "Ensure CONS's PERSISTENT-CAR/PERSISTENT-CDR (and LENGTH/PROPER)
slots are populated: first, if CONS is merely a plain, not-yet-more-
specifically-typed GIT-TREE (as returned by PERSISTENT-VECTOR-REF or
PERSISTENT-CAR for a bucket/pair node freshly fetched from Git, since
neither DESERIALIZE-TREE nor INFLATE-GIT-PROXY ever distinguish a
nested PERSISTENT-CONS from an ordinary GIT-TREE), retype it in place
into a PERSISTENT-CONS via CHANGE-CLASS; then, if CONS is not yet
loaded, fetch its raw tree bytes and its own \".meta\" blob via
GIT-CAT-FILE, and populate it via DESERIALIZE-PERSISTENT-CONS.
Returns CONS."
  (unless (typep cons 'persistent-cons)
    (change-class cons 'persistent-cons))
  (unless (get-loaded? cons)
    (let* ((repository (get-repository cons))
           (tree-octets (git-cat-file repository (sha cons)))
           (entries (deserialize-tree repository tree-octets))
           (meta-entry (assoc ".meta" entries :test #'string=))
           (meta-octets (git-cat-file repository (sha (cdr meta-entry)))))
      (deserialize-persistent-cons cons tree-octets meta-octets)))
  cons)

(defun %phash-bucket-node-p (node)
  "Return true if NODE is a real, non-terminal bucket-chain node (a
PERSISTENT-CONS, or a plain GIT-TREE proxy for one not yet retyped by
%ENSURE-PERSISTENT-CONS-LOADED), as opposed to NIL (an in-memory,
not-yet-serialized empty tail) or the GIT-BLOB that SERIALIZE-
PERSISTENT-CONS always writes to encode a proper list's terminal NIL
CDR once persisted for real -- either of which marks the end of the
chain."
  (and node (not (typep node 'git-blob))))

(defun %phash-bucket-find (bucket key test)
  "Return the dotted-pair PERSISTENT-CONS holding KEY's association
within BUCKET (a PERSISTENT-CONS chain, or NIL for an empty bucket),
comparing each node's own key against KEY via TEST, or NIL if KEY is
not present."
  (loop for node = bucket then (persistent-cdr (%ensure-persistent-cons-loaded node))
        while (%phash-bucket-node-p node)
        do (let* ((pair (persistent-car (%ensure-persistent-cons-loaded node)))
                  (existing-key (%phash-decode (persistent-car (%ensure-persistent-cons-loaded pair)))))
             (when (funcall test existing-key key)
               (return pair)))))

(defun %phash-bucket-put (repository bucket key value test)
  "Return two values: a new bucket chain associating KEY with VALUE,
and T if KEY did not previously appear in BUCKET (so the caller must
increment the table's COUNT), or NIL if an existing association was
merely replaced. If KEY is new, the new pair is simply consed onto
the front of BUCKET (BUCKET itself is entirely shared, unmodified).
If KEY already exists, only the prefix of nodes up to and including
its own node is rebuilt; every node after it is shared, unmodified,
with BUCKET."
  (labels ((walk (node)
             (cond
               ((not (%phash-bucket-node-p node)) (values nil nil))
               (t (%ensure-persistent-cons-loaded node)
                  (let* ((pair (%ensure-persistent-cons-loaded (persistent-car node)))
                         (existing-key (%phash-decode (persistent-car pair))))
                    (if (funcall test existing-key key)
                        (values (%make-list-node repository (%make-pair-node repository key value)
                                                  (persistent-cdr node))
                                t)
                        (multiple-value-bind (new-tail found?) (walk (persistent-cdr node))
                          (if found?
                              (values (%make-list-node repository pair new-tail) t)
                              (values nil nil)))))))))
    (multiple-value-bind (updated-bucket found?) (walk bucket)
      (if found?
          (values updated-bucket nil)
          (values (%make-list-node repository (%make-pair-node repository key value) bucket) t)))))

(defun %phash-bucket-remove (repository bucket key test)
  "Return two values: a new bucket chain with KEY's association
removed, and T; or (VALUES NIL NIL) if KEY is not present in BUCKET.
The node holding KEY is dropped outright (its successor is shared
directly as the new tail); every node before it is rebuilt, and every
node after it remains shared, unmodified, with BUCKET."
  (labels ((walk (node)
             (cond
               ((not (%phash-bucket-node-p node)) (values nil nil))
               (t (%ensure-persistent-cons-loaded node)
                  (let* ((pair (%ensure-persistent-cons-loaded (persistent-car node)))
                         (existing-key (%phash-decode (persistent-car pair))))
                    (if (funcall test existing-key key)
                        (values (persistent-cdr node) t)
                        (multiple-value-bind (new-tail removed?) (walk (persistent-cdr node))
                          (if removed?
                              (values (%make-list-node repository pair new-tail) t)
                              (values nil nil)))))))))
    (walk bucket)))

(defun %phash-rehash (table)
  "Return a new PERSISTENT-HASH-TABLE holding exactly the same
associations as TABLE, but with a fresh BUCKETS vector of twice
TABLE's own bucket count, every association re-hashed into its new
bucket. Each association's own dotted-pair PERSISTENT-CONS node is
reused verbatim (only fresh bucket-chain PERSISTENT-CONS nodes are
allocated), preserving structural sharing. Called automatically by
PHASH-PUT whenever inserting a new key would cause COUNT to exceed
the number of buckets (load factor > 1.0)."
  (let* ((repository (get-repository table))
         (old-buckets (persistent-hash-table-buckets table))
         (old-count (persistent-vector-length (%ensure-persistent-vector-loaded old-buckets)))
         (new-bucket-count (max 1 (* old-count 2)))
         (new-buckets (%make-empty-buckets repository new-bucket-count)))
    (dotimes (i old-count)
      (loop for node = (persistent-vector-ref old-buckets i) then (persistent-cdr (%ensure-persistent-cons-loaded node))
            while (%phash-bucket-node-p node)
            do (let* ((pair (%ensure-persistent-cons-loaded (persistent-car (%ensure-persistent-cons-loaded node))))
                      (key (%phash-decode (persistent-car pair)))
                      (new-index (%phash-bucket-index key new-bucket-count))
                      (existing (persistent-vector-ref new-buckets new-index)))
                 (setf new-buckets (%persistent-vector-copy-with new-buckets new-index
                                                                  (%make-list-node repository pair existing))))))
    (make-instance 'persistent-hash-table
                   :repository repository
                   :test (persistent-hash-table-test table)
                   :count (persistent-hash-table-count table)
                   :buckets new-buckets)))

(defun phash-make (&key repository (test 'eql) (size +persistent-hash-table-default-bucket-count+))
  "Return a new, empty PERSISTENT-HASH-TABLE with SIZE initial
buckets, comparing keys via TEST (a symbol naming a two-argument
equality predicate -- EQ, EQL, EQUAL, or EQUALP -- normalized via
%NORMALIZE-HASH-TEST so it is always stored as a serializable
symbol, never a raw function object). Signals INVALID-ARGUMENT-ERROR
if SIZE is not a positive integer."
  (unless (and (integerp size) (plusp size))
    (error 'invalid-argument-error
           :format-control "SIZE must be a positive integer, not ~S."
           :format-arguments (list size)))
  (make-instance 'persistent-hash-table
                 :repository repository
                 :test (%normalize-hash-test test)
                 :count 0
                 :buckets (%make-empty-buckets repository size)))

(defun phash-get (key table &optional default)
  "Return two values: the value associated with KEY in TABLE
(compared via TABLE's own TEST predicate) and T; or DEFAULT and NIL
if KEY is not present."
  (let* ((buckets (persistent-hash-table-buckets table))
         (bucket-count (persistent-vector-length (%ensure-persistent-vector-loaded buckets)))
         (index (%phash-bucket-index key bucket-count))
         (bucket (persistent-vector-ref buckets index))
         (pair (%phash-bucket-find bucket key (%phash-test-function table))))
    (if pair
        (values (%phash-decode (persistent-cdr pair)) t)
        (values default nil))))

(defun phash-put (key value table)
  "Return a new PERSISTENT-HASH-TABLE, structurally sharing with
TABLE wherever KEY's own bucket is unaffected, associating KEY with
VALUE. TABLE itself is left completely unmodified. If inserting a
new KEY would cause COUNT to exceed the number of buckets (load
factor > 1.0), the returned table is automatically rehashed (via
%PHASH-REHASH) into a larger BUCKETS vector."
  (let* ((repository (get-repository table))
         (buckets (persistent-hash-table-buckets table))
         (bucket-count (persistent-vector-length (%ensure-persistent-vector-loaded buckets)))
         (index (%phash-bucket-index key bucket-count))
         (bucket (persistent-vector-ref buckets index)))
    (multiple-value-bind (new-bucket added?)
        (%phash-bucket-put repository bucket key value (%phash-test-function table))
      (let* ((new-count (if added? (1+ (persistent-hash-table-count table)) (persistent-hash-table-count table)))
             (new-table (make-instance 'persistent-hash-table
                                        :repository repository
                                        :test (persistent-hash-table-test table)
                                        :count new-count
                                        :buckets (%persistent-vector-copy-with buckets index new-bucket))))
        (if (and added? (> new-count bucket-count))
            (%phash-rehash new-table)
            new-table)))))

(defun phash-remove (key table)
  "Return a new PERSISTENT-HASH-TABLE with KEY's association
removed, structurally sharing with TABLE wherever KEY's own bucket is
unaffected; or TABLE itself, unchanged, if KEY is not present."
  (let* ((repository (get-repository table))
         (buckets (persistent-hash-table-buckets table))
         (bucket-count (persistent-vector-length (%ensure-persistent-vector-loaded buckets)))
         (index (%phash-bucket-index key bucket-count))
         (bucket (persistent-vector-ref buckets index)))
    (multiple-value-bind (new-bucket removed?)
        (%phash-bucket-remove repository bucket key (%phash-test-function table))
      (if (not removed?)
          table
          (make-instance 'persistent-hash-table
                         :repository repository
                         :test (persistent-hash-table-test table)
                         :count (1- (persistent-hash-table-count table))
                         :buckets (%persistent-vector-copy-with buckets index new-bucket))))))

(defun phash-map (function table)
  "Call FUNCTION with two arguments -- KEY and VALUE, each already
decoded via %PHASH-DECODE exactly as PHASH-GET would return them --
once for every association currently in TABLE, in an unspecified
order. Returns NIL. Purely a read: TABLE itself is never modified,
and no bucket-chain node visited is retyped/loaded any differently
than PHASH-GET's own traversal already would."
  (let* ((buckets (persistent-hash-table-buckets table))
         (bucket-count (persistent-vector-length (%ensure-persistent-vector-loaded buckets))))
    (dotimes (i bucket-count)
      (loop for node = (persistent-vector-ref buckets i) then (persistent-cdr (%ensure-persistent-cons-loaded node))
            while (%phash-bucket-node-p node)
            do (let* ((pair (%ensure-persistent-cons-loaded (persistent-car (%ensure-persistent-cons-loaded node))))
                      (key (%phash-decode (persistent-car pair)))
                      (value (%phash-decode (persistent-cdr pair))))
                 (funcall function key value))))
    nil))
