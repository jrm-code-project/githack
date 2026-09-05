;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-ARRAY implements GitHack's mapping for an immutable,
;;; N-dimensional Lisp array: rather than introducing a deeply
;;; nested Git tree per dimension, it flattens the array's data, in
;;; standard row-major order, into a single, ordinary
;;; PERSISTENT-VECTOR, and wraps that flat vector in a small Git
;;; tree recording just the array's shape --
;;;
;;;   .meta      a blob holding the serialized (:TAG :ARRAY
;;;              :DIMENSIONS (d1 d2 ... dN) :ELEMENT-TYPE type)
;;;              property list
;;;   README.md  a blob holding a fixed, human-readable description
;;;              of this layout
;;;   data       a proxy pointer to the underlying, flattened
;;;              PERSISTENT-VECTOR
;;;
;;; so that PERSISTENT-ARRAY-REF need only compute a single row-major
;;; index from its subscripts and DIMENSIONS, then delegate entirely
;;; to PERSISTENT-VECTOR-REF's own lazy fetch/decode/cache machinery
;;; -- no new element-fetching logic of its own is required. Since
;;; this on-disk shape *is* an ordinary Git tree, PERSISTENT-ARRAY is
;;; implemented as a GIT-TREE subclass, reusing GIT-TREE's ENTRIES
;;; slot and SERIALIZE-TREE's binary encoding for its own underlying
;;; tree object.

(defparameter +persistent-array-readme+
  "# Persistent Multi-Dimensional Array

This Git tree represents an N-dimensional persistent array.
  * **.meta**: Contains the serialized property list defining the `:tag`, the list of `:dimensions`, and the `:element-type`.
  * **data**: The SHA pointer to a `persistent-vector` tree. This underlying 1D
    vector contains the flattened array data, stored in row-major order.
"
  "The fixed README.md content SERIALIZE-PERSISTENT-ARRAY writes,
verbatim and unencoded, into every persistent array tree.")

(defclass persistent-array (git-tree)
  ((dimensions
    :initarg :dimensions
    :initform nil
    :accessor persistent-array-dimensions
    :type (or null list)
    :documentation
    "This array's dimension sizes, as a list of non-negative
integers (D1 D2 ... DN), or NIL if not yet set (before serializing)
or not yet loaded (after deserializing).")
   (element-type
    :initarg :element-type
    :initform t
    :accessor persistent-array-element-type
    :documentation
    "This array's declared element type, typically T for a generic
persistent array. Recorded purely as metadata; GitHack does not
itself enforce it.")
   (data
    :initarg :data
    :initform nil
    :accessor %persistent-array-data
    :documentation
    "The underlying PERSISTENT-VECTOR holding this array's data,
flattened into standard row-major order: NIL if not yet set (before
serializing, in which case SERIALIZE-PERSISTENT-ARRAY signals an
error) or not yet loaded (after deserializing, until the first
PERSISTENT-ARRAY-REF)."))
  (:documentation
   "Proxy for a single, immutable N-dimensional Lisp array, stored
as a Git tree with a \".meta\" entry, a \"README.md\" entry, and a
\"data\" entry pointing at a single PERSISTENT-VECTOR holding this
array's data flattened into row-major order. See
SERIALIZE-PERSISTENT-ARRAY and DESERIALIZE-PERSISTENT-ARRAY for the
on-disk representation, and PERSISTENT-ARRAY-REF for lazily fetching
individual elements by their N-dimensional subscripts."))

(setf (documentation 'persistent-array-dimensions 'function)
      "Return ARRAY's (a PERSISTENT-ARRAY) dimension sizes, as a
list of non-negative integers (D1 D2 ... DN), or NIL if not yet set
(before serializing) or not yet loaded (after deserializing).")
(setf (documentation 'persistent-array-element-type 'function)
      "Return ARRAY's (a PERSISTENT-ARRAY) declared element type,
typically T for a generic persistent array. Recorded purely as
metadata; GitHack does not itself enforce it.")

(defun %serialize-persistent-array-meta (dimensions element-type)
  "Encode the small property list (:TAG :ARRAY :DIMENSIONS
DIMENSIONS :ELEMENT-TYPE ELEMENT-TYPE) as a UTF-8 octet vector, via
%SERIALIZE-PLIST: the exact raw content of a persistent array's
\".meta\" blob."
  (%serialize-plist (list :tag :array :dimensions dimensions :element-type element-type)))

(defun %deserialize-persistent-array-meta (octets)
  "Inverse of %SERIALIZE-PERSISTENT-ARRAY-META: parse OCTETS -- the
raw content of a persistent array's \".meta\" blob -- via
%DESERIALIZE-PLIST, and return two values, its :DIMENSIONS and
:ELEMENT-TYPE. Signals an error if OCTETS is not a plist whose :TAG
is :ARRAY."
  (let ((plist (%deserialize-plist octets)))
    (unless (eq (getf plist :tag) :array)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent array .meta blob: ~S."
             :format-arguments (list plist)))
    (values (getf plist :dimensions) (getf plist :element-type))))

(defun %persistent-array-volume (dimensions)
  "Return the total number of elements a persistent array of shape
DIMENSIONS (a list of non-negative integers) holds: the product of
every dimension size, or 1 for a zero-dimensional (scalar) array.
Signals an error if DIMENSIONS is not a proper list of non-negative
integers."
  (unless (and (listp dimensions) (every (lambda (d) (and (integerp d) (>= d 0))) dimensions))
    (error 'invalid-argument-error
           :format-control "Invalid persistent array dimensions: ~S."
           :format-arguments (list dimensions)))
  (reduce #'* dimensions :initial-value 1))

(defun %persistent-array-row-major-index (dimensions subscripts)
  "Return the single, flattened, zero-based row-major index -- into
a PERSISTENT-VECTOR of (%PERSISTENT-ARRAY-VOLUME DIMENSIONS)
elements -- corresponding to SUBSCRIPTS (a list of one zero-based
index per entry of DIMENSIONS), computed exactly as CL:ARRAY expects
for a standard, C/Lisp-order, row-major array of that shape: for
DIMENSIONS (D1 D2 D3) and SUBSCRIPTS (I J K), the result is
I*(D2*D3) + J*D3 + K. Signals an error immediately, without any Git
access, if SUBSCRIPTS' length does not match DIMENSIONS' length, or
if any individual subscript is out of bounds for its own dimension."
  (unless (= (length dimensions) (length subscripts))
    (error 'invalid-argument-error
           :format-control "Wrong number of subscripts ~S for persistent array of dimensions ~S."
           :format-arguments (list subscripts dimensions)))
  (let ((index 0))
    (loop for dimension in dimensions
          for subscript in subscripts
          do (unless (and (integerp subscript) (<= 0 subscript) (< subscript dimension))
               (error 'invalid-argument-error
                      :format-control "Subscript ~S out of bounds for dimension size ~S."
                      :format-arguments (list subscript dimension)))
             (setf index (+ (* index dimension) subscript)))
    index))

(defun serialize-persistent-array (array)
  "Compute ARRAY's total volume from its own DIMENSIONS, serialize
its underlying, flattened DATA PERSISTENT-VECTOR (via
SERIALIZE-PERSISTENT-VECTOR), create and persist the standard
\".meta\" and \"README.md\" blobs, and finally write ARRAY's own Git
tree object, with exactly three entries: \".meta\", \"README.md\",
and \"data\". Like SERIALIZE-PERSISTENT-VECTOR/SERIALIZE-PERSISTENT-
CONS, this performs real I/O, not pure encoding. Signals an error if
ARRAY's DIMENSIONS or DATA have not been set, or if DATA's own
element count (once serialized) does not match the volume implied by
DIMENSIONS. Returns ARRAY's own SHA, doing nothing further if ARRAY
already has one."
  (or (sha array)
      (let* ((dimensions (persistent-array-dimensions array))
             (element-type (persistent-array-element-type array))
             (data (%persistent-array-data array))
             (repository (get-repository array)))
        (unless dimensions
          (error 'unpersisted-object-error
                 :format-control "Cannot serialize persistent array: its DIMENSIONS have not been set."))
        (unless data
          (error 'unpersisted-object-error
                 :format-control "Cannot serialize persistent array: its DATA (underlying persistent-vector) has not been set."))
        (let ((volume (%persistent-array-volume dimensions)))
          (serialize-persistent-vector data)
          (unless (= volume (persistent-vector-length data))
            (error 'malformed-git-object-error
                   :format-control "Persistent array of dimensions ~S implies ~D element~:P, but its underlying persistent-vector has ~D."
                   :format-arguments (list dimensions volume (persistent-vector-length data))))
          (let* ((meta-blob (make-instance 'git-blob :repository repository
                                                      :sha (git-hash-object
                                                            repository "blob"
                                                            (%serialize-persistent-array-meta dimensions element-type))))
                 (readme-blob (make-instance 'git-blob :repository repository
                                                        :sha (git-hash-object
                                                              repository "blob"
                                                              (sb-ext:string-to-octets
                                                               +persistent-array-readme+
                                                               :external-format :utf-8)))))
            (setf (get-entries array)
                  (list (cons ".meta" meta-blob)
                        (cons "README.md" readme-blob)
                        (cons "data" data)))
            (setf (sha array) (git-hash-object repository "tree" (serialize-tree array)))
            (setf (get-loaded? array) t)
            (sha array))))))

(defun deserialize-persistent-array (array tree-octets meta-octets)
  "Parse TREE-OCTETS -- the raw byte-vector of ARRAY's own underlying
Git tree object -- together with META-OCTETS -- the raw byte-vector
of that tree's \".meta\" blob -- and populate ARRAY's ENTRIES,
DIMENSIONS, ELEMENT-TYPE, and DATA slots. DATA is set to a hollow
(unloaded) PERSISTENT-VECTOR proxy for the \"data\" entry's own SHA
-- not a generic GIT-TREE, as plain INFLATE-GIT-PROXY would produce
-- so that PERSISTENT-ARRAY-REF can delegate directly to
PERSISTENT-VECTOR-REF. No element of the underlying vector is ever
fetched eagerly by this function. Signals an error if TREE-OCTETS'
entries do not include \".meta\", \"README.md\", and \"data\". Marks
ARRAY loaded and returns it."
  (let* ((repository (get-repository array))
         (entries (deserialize-tree repository tree-octets))
         (data-entry (assoc "data" entries :test #'string=)))
    (unless (assoc ".meta" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent array tree: missing \".meta\" entry."))
    (unless (assoc "README.md" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent array tree: missing \"README.md\" entry."))
    (unless data-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent array tree: missing \"data\" entry."))
    (multiple-value-bind (dimensions element-type) (%deserialize-persistent-array-meta meta-octets)
      (setf (get-entries array) entries)
      (setf (persistent-array-dimensions array) dimensions)
      (setf (persistent-array-element-type array) element-type)
      (setf (%persistent-array-data array)
            (make-instance 'persistent-vector :repository repository :sha (sha (cdr data-entry))))
      (setf (get-loaded? array) t)
      array)))

(defun %ensure-persistent-array-loaded (array)
  "Ensure ARRAY's own Git tree entries, DIMENSIONS/ELEMENT-TYPE, and
DATA are all populated, fetching and parsing whatever raw Git bytes
are needed -- via %ENSURE-TREE-ENTRIES-LOADED for ARRAY's own tree,
then GIT-CAT-FILE plus %DESERIALIZE-PERSISTENT-ARRAY-META for its
\".meta\" entry -- for whichever of DIMENSIONS or DATA is not already
set. Mirrors PERSISTENT-VECTOR's own %ENSURE-PERSISTENT-VECTOR-
LOADED. Never fetches any element of the underlying DATA vector
itself; that remains entirely PERSISTENT-VECTOR-REF's own
responsibility. Returns ARRAY."
  (let ((repository (get-repository array)))
    (%ensure-tree-entries-loaded repository array)
    (unless (persistent-array-dimensions array)
      (let ((meta-entry (assoc ".meta" (get-entries array) :test #'string=)))
        (unless meta-entry
          (error 'malformed-git-object-error
                 :format-control "Malformed persistent array tree: missing \".meta\" entry."))
        (multiple-value-bind (dimensions element-type)
            (%deserialize-persistent-array-meta (git-cat-file repository (sha (cdr meta-entry))))
          (setf (persistent-array-dimensions array) dimensions)
          (setf (persistent-array-element-type array) element-type))))
    (unless (%persistent-array-data array)
      (let ((data-entry (assoc "data" (get-entries array) :test #'string=)))
        (unless data-entry
          (error 'malformed-git-object-error
                 :format-control "Malformed persistent array tree: missing \"data\" entry."))
        (setf (%persistent-array-data array)
              (make-instance 'persistent-vector :repository repository :sha (sha (cdr data-entry)))))))
  array)

(defun persistent-array-ref (array &rest subscripts)
  "Return the real Lisp value held at SUBSCRIPTS (one zero-based
index per dimension of ARRAY, in row-major/CL:AREF order) in ARRAY:
exactly (PERSISTENT-VECTOR-REF DATA (%PERSISTENT-ARRAY-ROW-MAJOR-
INDEX DIMENSIONS SUBSCRIPTS)), where DATA is ARRAY's own underlying,
flattened PERSISTENT-VECTOR. ARRAY's own Git tree and \".meta\" are
parsed (via %ENSURE-PERSISTENT-ARRAY-LOADED) at most once; the
underlying DATA vector's own further laziness -- one full parse of
its own tree, then independent per-index fetch/decode/cache -- is
then entirely PERSISTENT-VECTOR-REF's responsibility. Signals an
ordinary Lisp error for a wrong number of subscripts or any
out-of-bounds subscript, but only once ARRAY's own DIMENSIONS have
been established."
  (%ensure-persistent-array-loaded array)
  (let* ((dimensions (persistent-array-dimensions array))
         (index (%persistent-array-row-major-index dimensions subscripts)))
    (persistent-vector-ref (%persistent-array-data array) index)))
