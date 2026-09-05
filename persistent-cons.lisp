;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-CONS implements GitHack's strict 1:1 mapping between
;;; a single, immutable Lisp cons cell and a Git tree object: every
;;; persistent cons serializes to a Git tree containing exactly four
;;; entries --
;;;
;;;   .meta      a blob holding the serialized (:TAG :CONS :LENGTH n
;;;              :PROPER bool) property list
;;;   README.md  a blob holding a fixed, human-readable description
;;;              of this layout
;;;   car        a proxy pointer to the object held in this cons's CAR
;;;   cdr        a proxy pointer to the object held in this cons's CDR
;;;
;;; so that structurally identical tails hash to the same SHA and so
;;; are stored only once in Git's object database -- Lisp's native
;;; structural sharing and tail-deduplication, realized directly as
;;; Git object deduplication. Since this on-disk shape *is* an
;;; ordinary Git tree, PERSISTENT-CONS is implemented as a GIT-TREE
;;; subclass, reusing GIT-TREE's ENTRIES slot and SERIALIZE-TREE's
;;; binary encoding for its own underlying tree object.

(defparameter +persistent-cons-readme+
  "# Persistent Cons Object

This Git tree represents a single Lisp cons cell
within a persistent, immutable data structure.
  * **.meta**: Contains the serialized property list defining the  `:tag`, calculated `:length`, and `:proper` boolean flag for this  node and its descendants. 
  * **car**: The SHA pointer to the data or nested structure held in the `car` of this cons. 
  * **cdr**: The SHA pointer to the data or nested structure held in the `cdr` of this cons.
This 1:1 tree-to-cons mapping intentionally preserves Lisp's
native structural sharing and tail-deduplication within the Git object database.
"
  "The fixed README.md content SERIALIZE-PERSISTENT-CONS writes,
verbatim and unencoded, into every persistent cons tree.")

(defclass persistent-cons (git-tree)
  ((persistent-car
    :initarg :persistent-car
    :initform nil
    :accessor persistent-car
    :documentation
    "The GIT-OBJECT proxy held in this cons's CAR -- a GIT-BLOB for
a scalar/atom, or a GIT-TREE/PERSISTENT-CONS for a compound value --
or NIL if not yet set (before serializing) or not yet loaded (after
deserializing).")
   (persistent-cdr
    :initarg :persistent-cdr
    :initform nil
    :accessor persistent-cdr
    :documentation
    "The value held in this cons's CDR: NIL if this cons is the
last cons of a proper list (before serializing -- see
SERIALIZE-PERSISTENT-CONS, which then replaces it with a real
GIT-BLOB encoding the atom NIL), a nested PERSISTENT-CONS if the CDR
is itself a cons, any other GIT-OBJECT proxy (typically a GIT-BLOB)
for a dotted pair's final atom, or NIL if not yet loaded (after
deserializing, before this cons's ENTRIES have been examined).")
   (length
    :initarg :length
    :initform nil
    :accessor persistent-cons-length
    :type (or null integer)
    :documentation
    "This cons's list length, counting itself and every cons in its
CDR chain (1 for a dotted pair or a singleton list), or NIL if not
yet computed/loaded. See %PERSISTENT-CONS-METADATA.")
   (proper
    :initarg :proper
    :initform nil
    :accessor persistent-cons-proper
    :documentation
    "True if this cons and its entire CDR chain terminates in NIL
(a proper list); NIL if it instead terminates in some other atom (a
dotted pair), or if not yet computed/loaded. See
%PERSISTENT-CONS-METADATA."))
  (:documentation
   "Proxy for a single, immutable Lisp cons cell, stored as a Git
tree with exactly four entries: \".meta\", \"README.md\", \"car\",
and \"cdr\". See SERIALIZE-PERSISTENT-CONS and
DESERIALIZE-PERSISTENT-CONS for the on-disk representation."))

(setf (documentation 'persistent-car 'function)
      "Return CONS's (a PERSISTENT-CONS) GIT-OBJECT proxy held in
its CAR -- a GIT-BLOB for a scalar/atom, or a GIT-TREE/
PERSISTENT-CONS for a compound value -- or NIL if not yet set
(before serializing) or not yet loaded (after deserializing).")
(setf (documentation 'persistent-cdr 'function)
      "Return the value held in CONS's (a PERSISTENT-CONS) CDR: NIL
if CONS is the last cons of a proper list, a nested PERSISTENT-CONS
if the CDR is itself a cons, any other GIT-OBJECT proxy (typically a
GIT-BLOB) for a dotted pair's final atom, or NIL if not yet loaded.")
(setf (documentation 'persistent-cons-length 'function)
      "Return CONS's (a PERSISTENT-CONS) list length, counting
itself and every cons in its CDR chain (1 for a dotted pair or a
singleton list), or NIL if not yet computed/loaded.")
(setf (documentation 'persistent-cons-proper 'function)
      "Return true if CONS (a PERSISTENT-CONS) and its entire CDR
chain terminates in NIL (a proper list); NIL if it instead
terminates in some other atom (a dotted pair), or if not yet
computed/loaded.")

(defun %serialize-persistent-cons-meta (length proper)
  "Encode the small property list (:TAG :CONS :LENGTH LENGTH :PROPER
PROPER) as a UTF-8 octet vector, via %SERIALIZE-PLIST: the exact raw
content of a persistent cons's \".meta\" blob."
  (%serialize-plist (list :tag :cons :length length :proper proper)))

(defun %deserialize-persistent-cons-meta (octets)
  "Inverse of %SERIALIZE-PERSISTENT-CONS-META: parse OCTETS -- the
raw content of a persistent cons's \".meta\" blob -- via
%DESERIALIZE-PLIST, and return two values, its :LENGTH and :PROPER.
Signals an error if OCTETS is not a plist whose :TAG is :CONS."
  (let ((plist (%deserialize-plist octets)))
    (unless (eq (getf plist :tag) :cons)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent cons .meta blob: ~S."
             :format-arguments (list plist)))
    (values (getf plist :length) (getf plist :proper))))

(defgeneric %persist-cons-component-by-type (git-object)
  (:documentation
   "Persist GIT-OBJECT (which is known not to have a SHA yet) to
Git's object database according to its concrete type, and return the
resulting SHA. Broken out of %PERSIST-CONS-COMPONENT so this
dispatch is its own generic function, with one DEFMETHOD per
concrete type in place of an ETYPECASE clause."))

(defmethod %persist-cons-component-by-type ((git-object persistent-cons))
  (serialize-persistent-cons git-object))

(defmethod %persist-cons-component-by-type ((git-object git-tree))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "tree" (serialize-tree git-object))))

(defmethod %persist-cons-component-by-type ((git-object git-blob))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "blob"
                          (serialize-atom (get-payload git-object)))))

(defun %persist-cons-component (git-object)
  "Ensure GIT-OBJECT (a GIT-BLOB, a plain GIT-TREE, or a nested
PERSISTENT-CONS) has a SHA, persisting it via GIT-HASH-OBJECT if it
does not already. A PERSISTENT-CONS is persisted recursively,
through its own CAR/CDR chain, via SERIALIZE-PERSISTENT-CONS; any
other GIT-TREE is assumed to already have every one of its ENTRIES
persisted, exactly as SERIALIZE-TREE itself requires. Returns
GIT-OBJECT's SHA."
  (or (sha git-object)
      (%persist-cons-component-by-type git-object)))

(defun %persistent-cons-metadata (cons)
  "Return two values, the :LENGTH and :PROPER CONS's \".meta\" must
record, computed from CONS's PERSISTENT-CDR: 1 and T if
PERSISTENT-CDR is NIL (CONS is the last cons of a proper list); one
more than, and the same properness as, PERSISTENT-CDR's own
LENGTH/PROPER if PERSISTENT-CDR is itself a PERSISTENT-CONS
(persisting it first via %PERSIST-CONS-COMPONENT if necessary, so
those slots are guaranteed to already be populated); or 1 and NIL
for any other (dotted pair) PERSISTENT-CDR."
  (let ((cdr (persistent-cdr cons)))
    (cond
      ((null cdr) (values 1 t))
      ((typep cdr 'persistent-cons)
       (%persist-cons-component cdr)
       (values (1+ (persistent-cons-length cdr)) (persistent-cons-proper cdr)))
      (t (values 1 nil)))))

(defun serialize-persistent-cons (cons)
  "Compute CONS's LENGTH/PROPER metadata, create and persist its
standard \".meta\" and \"README.md\" blobs, recursively persist its
CAR and CDR (obtaining their SHAs -- synthesizing and persisting a
fresh GIT-BLOB encoding the atom NIL for CDR when CONS is the last
cons of a proper list, and replacing CONS's PERSISTENT-CDR with that
GIT-BLOB), and finally write CONS's own Git tree object. Unlike
SERIALIZE-TREE/SERIALIZE-COMMIT elsewhere in this layer, this
function performs real I/O: it is the PERSISTENT-CONS analogue of
GIT-TRANSACTION's %PERSIST-GIT-TREE-OBJECT, not a pure encoder.
Signals an error if CONS's PERSISTENT-CAR has not been set. Returns
CONS's own SHA, doing nothing further if CONS already has one."
  (or (sha cons)
      (multiple-value-bind (length proper) (%persistent-cons-metadata cons)
        (setf (persistent-cons-length cons) length)
        (setf (persistent-cons-proper cons) proper)
        (let* ((repository (get-repository cons))
               (meta-blob (make-instance 'git-blob :repository repository
                                                    :sha (git-hash-object
                                                          repository "blob"
                                                          (%serialize-persistent-cons-meta length proper))))
               (readme-blob (make-instance 'git-blob :repository repository
                                                      :sha (git-hash-object
                                                            repository "blob"
                                                            (sb-ext:string-to-octets
                                                             +persistent-cons-readme+
                                                             :external-format :utf-8))))
               (car-object (persistent-car cons))
               (cdr-object (or (persistent-cdr cons)
                                (make-instance 'git-blob :repository repository :payload nil))))
          (unless car-object
            (error 'unpersisted-object-error
                   :format-control "Cannot serialize persistent cons: its PERSISTENT-CAR has not been set."))
          (%persist-cons-component car-object)
          (%persist-cons-component cdr-object)
          (setf (persistent-cdr cons) cdr-object)
          (setf (get-entries cons)
                (list (cons ".meta" meta-blob)
                      (cons "README.md" readme-blob)
                      (cons "car" car-object)
                      (cons "cdr" cdr-object)))
          (setf (sha cons) (git-hash-object repository "tree" (serialize-tree cons)))
          (setf (get-loaded? cons) t)
          (sha cons)))))

(defun deserialize-persistent-cons (cons tree-octets meta-octets)
  "Parse TREE-OCTETS -- the raw byte-vector of CONS's own underlying
Git tree object -- together with META-OCTETS -- the raw byte-vector
of that tree's \".meta\" blob -- and populate CONS's ENTRIES,
PERSISTENT-CAR, PERSISTENT-CDR, LENGTH, and PROPER slots.
PERSISTENT-CAR and PERSISTENT-CDR are set to hollow (unloaded)
GIT-OBJECT proxies via INFLATE-GIT-PROXY, exactly as
DESERIALIZE-TREE/DESERIALIZE-COMMIT already do for their own nested
SHA references -- so the CAR's/CDR's own content, including whether
either is itself a further-nested PERSISTENT-CONS, is only ever
fetched later, on demand, never eagerly by this function. Signals an
error if TREE-OCTETS' entries do not include \".meta\", \"README.md\",
\"car\", and \"cdr\". Marks CONS loaded and returns it."
  (let* ((repository (get-repository cons))
         (entries (deserialize-tree repository tree-octets))
         (car-entry (assoc "car" entries :test #'string=))
         (cdr-entry (assoc "cdr" entries :test #'string=)))
    (unless (assoc ".meta" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent cons tree: missing \".meta\" entry."))
    (unless (assoc "README.md" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent cons tree: missing \"README.md\" entry."))
    (unless car-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent cons tree: missing \"car\" entry."))
    (unless cdr-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent cons tree: missing \"cdr\" entry."))
    (multiple-value-bind (length proper) (%deserialize-persistent-cons-meta meta-octets)
      (setf (get-entries cons) entries)
      (setf (persistent-car cons) (cdr car-entry))
      (setf (persistent-cdr cons) (cdr cdr-entry))
      (setf (persistent-cons-length cons) length)
      (setf (persistent-cons-proper cons) proper)
      (setf (get-loaded? cons) t)
      cons)))

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

(defun %persistent-cons-decode (git-object)
  "Return the real Lisp value GIT-OBJECT represents: its decoded
PAYLOAD, if GIT-OBJECT is a GIT-BLOB (fetching it from the
repository first via %ENSURE-BLOB-LOADED, if not yet loaded); or
GIT-OBJECT itself, unchanged, for any other (compound) GIT-OBJECT
proxy."
  (if (typep git-object 'git-blob)
      (get-payload (%ensure-blob-loaded git-object))
      git-object))

(defun %persistent-cons-tail-p (tail)
  "Return true if TAIL (a raw PERSISTENT-CDR value, possibly not yet
retyped by %ENSURE-PERSISTENT-CONS-LOADED) represents a further cons
cell continuing the list, as opposed to NIL (a proper list's own
terminator) or a GIT-BLOB (the sentinel SERIALIZE-PERSISTENT-CONS
always uses to encode either a proper list's terminal NIL or a
dotted pair's own final atom) -- either of which ends the chain.
Mirrors PERSISTENT-HASH-TABLE.LISP's own %PHASH-BUCKET-NODE-P, which
applies this identical convention to its own PERSISTENT-CONS
bucket chains."
  (and tail (not (typep tail 'git-blob))))

(defun scan-persistent-list (list)
  "Return a series of the successive PERSISTENT-CAR elements of LIST
(a PERSISTENT-CONS, a not-yet-retyped GIT-TREE proxy for one, or NIL
for the empty list), each decoded via %PERSISTENT-CONS-DECODE (a
GIT-BLOB element's own PAYLOAD, or any other, compound GIT-OBJECT
proxy left unchanged), fetching each successive cons cell via
%ENSURE-PERSISTENT-CONS-LOADED one at a time as the underlying
SCAN-FN/MAP-FN series is advanced. Terminates -- producing a finite
series -- at the first tail for which %PERSISTENT-CONS-TAIL-P is
false: NIL (a proper list's own terminator) or a GIT-BLOB (a dotted
pair's own final atom); LIST itself may already be either of these,
in which case the series is empty. An OPTIMIZABLE-SERIES-FUNCTION,
built entirely from the primitive SCAN-FN/MAP-FN series functions,
so it can be spliced and fused into a surrounding series expression
by the SERIES compiler exactly like the standard SCAN/SCAN-SUBLISTS
-- when consumed from within such an optimized series expression
(e.g. one ending in UNTIL-IF or a bounded COLLECT), only the prefix
of LIST actually demanded is ever fetched from Git. A bare,
unoptimized call (as when invoked directly, outside any surrounding
SERIES-optimized DEFUN) instead produces its result eagerly in the
ordinary Lisp-list representation SERIES falls back to, and so
forces every cons cell of LIST up front regardless of how much of
the resulting series is later consumed."
  (declare (optimizable-series-function))
  (let ((tails (scan-fn t
                        (lambda () list)
                        (lambda (tail) (persistent-cdr (%ensure-persistent-cons-loaded tail)))
                        (lambda (tail) (not (%persistent-cons-tail-p tail))))))
    (map-fn t
            (lambda (tail)
              (%persistent-cons-decode (persistent-car (%ensure-persistent-cons-loaded tail))))
            tails)))
