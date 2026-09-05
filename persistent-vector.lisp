;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-VECTOR implements GitHack's mapping between a single,
;;; immutable 1-D Lisp vector (a simple array) and a Git tree object:
;;; every persistent vector serializes to a Git tree containing --
;;;
;;;   .meta       a blob holding the serialized (:TAG :VECTOR
;;;               :LENGTH n :ELEMENT-TYPE type) property list
;;;   README.md   a blob holding a fixed, human-readable description
;;;               of this layout
;;;   "0".."N-1"  one entry per index, each a proxy pointer to the
;;;               object held at that index -- a GIT-BLOB for a
;;;               scalar/atom, or a GIT-TREE/PERSISTENT-CONS/nested
;;;               PERSISTENT-VECTOR for a compound value
;;;
;;; so that a hollow proxy needs only its own SHA and its tree's
;;; ".meta" entry to report its LENGTH in O(1), without fetching a
;;; single element, and each element is then fetched, decoded, and
;;; cached completely independently of every other, on its own first
;;; access, via PERSISTENT-VECTOR-REF. Since this on-disk shape *is*
;;; an ordinary Git tree, PERSISTENT-VECTOR is implemented as a
;;; GIT-TREE subclass, reusing GIT-TREE's ENTRIES slot (populated
;;; with the "0".."N-1" index entries directly by the caller, before
;;; ".meta"/"README.md" are added by SERIALIZE-PERSISTENT-VECTOR) and
;;; SERIALIZE-TREE's binary encoding for its own underlying tree
;;; object.

(defparameter +persistent-vector-readme+
  "# Persistent Vector Object

This Git tree represents a 1D persistent vector (array).
  * **.meta**: Contains the serialized property list defining the `:tag`, `:length`, and `:element-type`.
  * **[0...N-1]**: Files named with integer indices. Each contains the SHA pointer
    to the data or nested structure at that index within the vector.
This flat tree structure allows for O(1) length lookups via `.meta` and efficient
lazy-loading of specific indices upon access.
"
  "The fixed README.md content SERIALIZE-PERSISTENT-VECTOR writes,
verbatim and unencoded, into every persistent vector tree.")

(defparameter +persistent-vector-unloaded+ (list :unloaded)
  "A unique marker, distinguishable via EQ from any real Lisp value
(including NIL), used to mark a PERSISTENT-VECTOR's per-index cache
slot as not yet fetched. See PERSISTENT-VECTOR-REF.")

(defclass persistent-vector (git-tree)
  ((length
    :initarg :length
    :initform nil
    :accessor persistent-vector-length
    :type (or null integer)
    :documentation
    "This vector's exact number of elements, or NIL if not yet
computed/loaded. Known immediately from a hollow (just-deserialized)
proxy's \".meta\" blob, in O(1), without fetching any element.")
   (element-type
    :initarg :element-type
    :initform t
    :accessor persistent-vector-element-type
    :documentation
    "This vector's declared array element type, typically T for a
generic persistent vector. Recorded purely as metadata; GitHack does
not itself enforce it.")
   (cache
    :initarg :cache
    :initform nil
    :accessor %persistent-vector-cache
    :documentation
    "NIL until the first call to PERSISTENT-VECTOR-REF for this
vector, which lazily allocates it as a SIMPLE-VECTOR of LENGTH
elements, every slot initially +PERSISTENT-VECTOR-UNLOADED+; each
slot is then replaced, independently and only once, with the real
decoded value PERSISTENT-VECTOR-REF fetches for that index."))
  (:documentation
   "Proxy for a single, immutable 1-D Lisp vector (simple array),
stored as a Git tree with a \".meta\" entry, a \"README.md\" entry,
and one further entry per index, named with its zero-based decimal
index. See SERIALIZE-PERSISTENT-VECTOR and
DESERIALIZE-PERSISTENT-VECTOR for the on-disk representation, and
PERSISTENT-VECTOR-REF for lazily fetching individual elements."))

(setf (documentation 'persistent-vector-length 'function)
      "Return VECTOR's (a PERSISTENT-VECTOR) exact number of
elements, or NIL if not yet computed/loaded. Known immediately from
a hollow (just-deserialized) proxy's \".meta\" blob, in O(1), without
fetching any element.")
(setf (documentation 'persistent-vector-element-type 'function)
      "Return VECTOR's (a PERSISTENT-VECTOR) declared array element
type, typically T for a generic persistent vector. Recorded purely
as metadata; GitHack does not itself enforce it.")

(defun %serialize-persistent-vector-meta (length element-type)
  "Encode the small property list (:TAG :VECTOR :LENGTH LENGTH
:ELEMENT-TYPE ELEMENT-TYPE) as a UTF-8 octet vector, via
%SERIALIZE-PLIST: the exact raw content of a persistent vector's
\".meta\" blob."
  (%serialize-plist (list :tag :vector :length length :element-type element-type)))

(defun %deserialize-persistent-vector-meta (octets)
  "Inverse of %SERIALIZE-PERSISTENT-VECTOR-META: parse OCTETS -- the
raw content of a persistent vector's \".meta\" blob -- via
%DESERIALIZE-PLIST, and return two values, its :LENGTH and
:ELEMENT-TYPE. Signals an error if OCTETS is not a plist whose :TAG
is :VECTOR."
  (let ((plist (%deserialize-plist octets)))
    (unless (eq (getf plist :tag) :vector)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent vector .meta blob: ~S."
             :format-arguments (list plist)))
    (values (getf plist :length) (getf plist :element-type))))

(defun %persistent-vector-index-entries (vector)
  "Return VECTOR's own ENTRIES with any \".meta\"/\"README.md\"
entries excluded: just the \"0\"..\"N-1\" index entries, in whatever
order GET-ENTRIES currently holds them."
  (remove-if (lambda (entry) (member (car entry) '(".meta" "README.md") :test #'string=))
             (get-entries vector)))

(defgeneric %persist-vector-component-by-type (git-object)
  (:documentation
   "Persist GIT-OBJECT (which is known not to have a SHA yet) to
Git's object database according to its concrete type, and return the
resulting SHA. Broken out of %PERSIST-VECTOR-COMPONENT so this
dispatch is its own generic function, with one DEFMETHOD per
concrete type in place of an ETYPECASE clause."))

(defmethod %persist-vector-component-by-type ((git-object persistent-vector))
  (serialize-persistent-vector git-object))

(defmethod %persist-vector-component-by-type ((git-object persistent-cons))
  (serialize-persistent-cons git-object))

(defmethod %persist-vector-component-by-type ((git-object git-tree))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "tree" (serialize-tree git-object))))

(defmethod %persist-vector-component-by-type ((git-object git-blob))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "blob"
                          (serialize-atom (get-payload git-object)))))

(defun %persist-vector-component (git-object)
  "Ensure GIT-OBJECT (a GIT-BLOB, a plain GIT-TREE, a PERSISTENT-CONS,
or a nested PERSISTENT-VECTOR) has a SHA, persisting it if it does
not already: recursively, through SERIALIZE-PERSISTENT-VECTOR or
SERIALIZE-PERSISTENT-CONS, for a nested PERSISTENT-VECTOR or
PERSISTENT-CONS; via GIT-HASH-OBJECT of its already-persisted
ENTRIES for a plain GIT-TREE (exactly as SERIALIZE-TREE itself
requires); or via GIT-HASH-OBJECT of its serialized PAYLOAD for a
GIT-BLOB. Mirrors PERSISTENT-CONS's own %PERSIST-CONS-COMPONENT, kept
separate (rather than shared) so this file need not depend on
persistent-cons.lisp's internals, and so neither persistent structure
need depend on GIT-TRANSACTION's own %PERSIST-GIT-OBJECT, breaking
what would otherwise be a load-order cycle. Returns GIT-OBJECT's
SHA."
  (or (sha git-object)
      (%persist-vector-component-by-type git-object)))

(defun serialize-persistent-vector (vector)
  "Compute VECTOR's LENGTH from its own index entries (every entry
of GET-ENTRIES other than \".meta\"/\"README.md\", which
SERIALIZE-PERSISTENT-VECTOR itself adds), create and persist its
standard \".meta\" and \"README.md\" blobs, recursively persist every
index entry's GIT-OBJECT (via %PERSIST-VECTOR-COMPONENT), and finally
write VECTOR's own Git tree object. Like SERIALIZE-PERSISTENT-CONS,
this performs real I/O, not pure encoding. Returns VECTOR's own SHA,
doing nothing further if VECTOR already has one."
  (or (sha vector)
      (let* ((index-entries (%persistent-vector-index-entries vector))
             (length (length index-entries))
             (element-type (persistent-vector-element-type vector))
             (repository (get-repository vector))
             (meta-blob (make-instance 'git-blob :repository repository
                                                  :sha (git-hash-object
                                                        repository "blob"
                                                        (%serialize-persistent-vector-meta length element-type))))
             (readme-blob (make-instance 'git-blob :repository repository
                                                    :sha (git-hash-object
                                                          repository "blob"
                                                          (sb-ext:string-to-octets
                                                           +persistent-vector-readme+
                                                           :external-format :utf-8)))))
        (dolist (entry index-entries)
          (%persist-vector-component (cdr entry)))
        (setf (persistent-vector-length vector) length)
        (setf (get-entries vector)
              (list* (cons ".meta" meta-blob)
                     (cons "README.md" readme-blob)
                     index-entries))
        (setf (sha vector) (git-hash-object repository "tree" (serialize-tree vector)))
        (setf (get-loaded? vector) t)
        (sha vector))))

(defun deserialize-persistent-vector (vector tree-octets meta-octets)
  "Parse TREE-OCTETS -- the raw byte-vector of VECTOR's own
underlying Git tree object -- together with META-OCTETS -- the raw
byte-vector of that tree's \".meta\" blob -- and populate VECTOR's
ENTRIES, LENGTH, and ELEMENT-TYPE slots. Every index entry is set to
a hollow (unloaded) GIT-OBJECT proxy via INFLATE-GIT-PROXY, exactly
as DESERIALIZE-TREE already does for its own nested SHA references --
so no element's own content is ever fetched eagerly by this function,
only later, on demand, by PERSISTENT-VECTOR-REF. Signals an error if
TREE-OCTETS' entries do not include \".meta\" and \"README.md\".
Marks VECTOR loaded and returns it."
  (let* ((entries (deserialize-tree (get-repository vector) tree-octets)))
    (unless (assoc ".meta" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent vector tree: missing \".meta\" entry."))
    (unless (assoc "README.md" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent vector tree: missing \"README.md\" entry."))
    (multiple-value-bind (length element-type) (%deserialize-persistent-vector-meta meta-octets)
      (setf (get-entries vector) entries)
      (setf (persistent-vector-length vector) length)
      (setf (persistent-vector-element-type vector) element-type)
      (setf (%persistent-vector-cache vector) nil)
      (setf (get-loaded? vector) t)
      vector)))

(defun %ensure-persistent-vector-loaded (vector)
  "Ensure VECTOR's ENTRIES and LENGTH/ELEMENT-TYPE slots are all
populated: parse VECTOR's underlying Git tree object (via
%ENSURE-TREE-ENTRIES-LOADED, a no-op if already done), then, if
LENGTH is still unknown, fetch and decode its \".meta\" entry's raw
bytes via GIT-CAT-FILE and %DESERIALIZE-PERSISTENT-VECTOR-META.
Mirrors %ATOMIC-WRAPPER-TREE-P's own direct GIT-CAT-FILE lookup of a
tree's \".meta\" entry, rather than routing it through
%ENSURE-BLOB-LOADED, since a persistent vector's own PAYLOAD-less
GIT-BLOB proxy for \".meta\" is never otherwise needed. Returns
VECTOR."
  (let ((repository (get-repository vector)))
    (%ensure-tree-entries-loaded repository vector)
    (unless (persistent-vector-length vector)
      (let ((meta-entry (assoc ".meta" (get-entries vector) :test #'string=)))
        (unless meta-entry
          (error 'malformed-git-object-error
                 :format-control "Malformed persistent vector tree: missing \".meta\" entry."))
        (multiple-value-bind (length element-type)
            (%deserialize-persistent-vector-meta (git-cat-file repository (sha (cdr meta-entry))))
          (setf (persistent-vector-length vector) length)
          (setf (persistent-vector-element-type vector) element-type)))))
  vector)

(defun persistent-vector-ref (vector index)
  "Return the real Lisp value held at INDEX (a non-negative integer
less than (PERSISTENT-VECTOR-LENGTH VECTOR)) in VECTOR: the decoded
atom, if the GIT-OBJECT proxy at that index is a GIT-BLOB, or that
GIT-OBJECT proxy itself (a GIT-TREE, PERSISTENT-CONS, or nested
PERSISTENT-VECTOR) otherwise. VECTOR's underlying Git tree (and, if
necessary, its \".meta\" entry) is parsed (via
%ENSURE-PERSISTENT-VECTOR-LOADED) at most once, no matter how many
distinct indices are eventually requested across multiple calls,
or even if LENGTH was not yet known when this function was first
called for VECTOR; each individual index's own GIT-OBJECT is then
fetched, decoded, and cached completely independently of every other
index, only on that index's own first access. Signals an ordinary
Lisp error for an out-of-bounds INDEX -- but only after VECTOR's
LENGTH has been established, since an unloaded proxy cannot know its
own bounds without first consulting Git.

Not thread-safe: see git-transaction.lisp's CONCURRENCY POLICY
comment. Neither %ENSURE-PERSISTENT-VECTOR-LOADED nor this
function's own per-index cache array is synchronized."
  (%ensure-persistent-vector-loaded vector)
  (let ((length (persistent-vector-length vector)))
    (unless (and (integerp index) (<= 0 index) (< index length))
      (error 'invalid-argument-error
             :format-control "Index ~S out of bounds for persistent vector of length ~S."
             :format-arguments (list index length)))
    (let ((cache (or (%persistent-vector-cache vector)
                      (setf (%persistent-vector-cache vector)
                            (make-array length :initial-element +persistent-vector-unloaded+)))))
      (let ((cached (svref cache index)))
        (if (not (eq cached +persistent-vector-unloaded+))
            cached
            (let* ((entries (get-entries vector))
                   (entry (assoc (princ-to-string index) entries :test #'string=)))
              (unless entry
                (error 'malformed-git-object-error
                       :format-control "Malformed persistent vector tree: missing entry ~S."
                       :format-arguments (list index)))
              (let* ((object (cdr entry))
                     (value (if (typep object 'git-blob)
                                (get-payload (%ensure-blob-loaded object))
                                object)))
                (setf (svref cache index) value)
                value)))))))
