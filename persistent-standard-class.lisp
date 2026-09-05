;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-STANDARD-CLASS is a CLOS metaclass, together with
;;; PERSISTENT-OBJECT, its mandatory base class, that transparently
;;; bridges ordinary CLOS instances to the GIT-OBJECT proxy layer.
;;;
;;; Every PERSISTENT-OBJECT subclass instance serializes to a single
;;; Git tree containing --
;;;
;;;   .meta        a blob holding the plist (:TAG :CLOS :CLASS
;;;                "CLASS-NAME" :VERSION n) -- structural metadata
;;;                only, never any application data
;;;   README.md    a blob holding a Markdown "# " title line naming
;;;                INSTANCE's own class, followed by that class's
;;;                own DOCUMENTATION string (its DEFCLASS's own
;;;                :DOCUMENTATION option), for a human Git user
;;;                browsing the object database -- see
;;;                %PERSISTENT-OBJECT-README-CONTENT
;;;   <initarg>    one further entry per non-transient initarg
;;;                recorded when the instance was created, named
;;;                with that initarg's keyword name, downcased, with
;;;                its leading colon dropped (e.g. :USERNAME becomes
;;;                "username") -- a proxy pointer to that initarg's
;;;                own value, a GIT-BLOB for a raw Lisp atom or any
;;;                other GIT-OBJECT (a GIT-TREE, PERSISTENT-CONS,
;;;                PERSISTENT-VECTOR, PERSISTENT-ARRAY, or nested
;;;                PERSISTENT-OBJECT) for a compound value
;;;
;;; so that a hollow, just-deserialized instance need only read its
;;; own ".meta" entry to know which concrete subclass (and slot
;;; initargs) to MAKE-INSTANCE, and every other entry's own value is
;;; then fetched, decoded, and cached completely independently, on
;;; its own first access, transparently, via an ordinary SLOT-VALUE
;;; call -- the caller never sees a GIT-OBJECT proxy at all.
;;;
;;; A slot is marked :TRANSIENT T to keep it entirely in memory: its
;;; associated initarg data is filtered out of SERIALIZE-PERSISTENT-
;;; OBJECT's tree and so never reaches Git. PERSISTENT-OBJECT itself
;;; marks GIT-TREE's own REPOSITORY/SHA/ENTRIES/LOADED? slots (along
;;; with its own VERSION and internal %INITIALIZER-PAYLOAD slots)
;;; :TRANSIENT T this way, so this bookkeeping is never mistaken for
;;; application data.

(defclass persistent-standard-class (standard-class) ()
  (:documentation
   "Metaclass for a CLOS class whose instances transparently
serialize to, and deserialize from, a Git tree via
SERIALIZE-PERSISTENT-OBJECT/DESERIALIZE-PERSISTENT-OBJECT. Every
class using this metaclass must inherit from PERSISTENT-OBJECT."))

(defclass %persistent-slot-definition-mixin ()
  ((transient
    :initarg :transient
    :initform nil
    :reader persistent-slot-definition-transient
    :documentation
    "True if this slot's data must never be persisted to Git: its
associated initarg (if any) is entirely filtered out of
SERIALIZE-PERSISTENT-OBJECT's tree entries. Defaults to NIL."))
  (:documentation
   "Mixin shared by PERSISTENT-DIRECT-SLOT-DEFINITION and
PERSISTENT-EFFECTIVE-SLOT-DEFINITION, holding the :TRANSIENT slot
option both must support."))

(setf (documentation 'persistent-slot-definition-transient 'function)
      "Return true if SLOT-DEFINITION's (a
PERSISTENT-DIRECT-SLOT-DEFINITION or
PERSISTENT-EFFECTIVE-SLOT-DEFINITION) data must never be persisted
to Git: its associated initarg (if any) is entirely filtered out of
SERIALIZE-PERSISTENT-OBJECT's tree entries. Defaults to NIL.")

(defclass persistent-direct-slot-definition
    (%persistent-slot-definition-mixin sb-mop:standard-direct-slot-definition)
  ()
  (:documentation
   "Direct slot definition class for PERSISTENT-STANDARD-CLASS,
adding the :TRANSIENT slot option."))

(defclass persistent-effective-slot-definition
    (%persistent-slot-definition-mixin sb-mop:standard-effective-slot-definition)
  ()
  (:documentation
   "Effective slot definition class for PERSISTENT-STANDARD-CLASS.
Its TRANSIENT flag is computed by COMPUTE-EFFECTIVE-SLOT-DEFINITION
from the most specific direct slot definition (across the class
precedence list) that specifies one."))

(defmethod sb-mop:validate-superclass
    ((class persistent-standard-class) (superclass standard-class))
  t)

(defmethod sb-mop:direct-slot-definition-class
    ((class persistent-standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-direct-slot-definition))

(defmethod sb-mop:effective-slot-definition-class
    ((class persistent-standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-effective-slot-definition))

(defmethod sb-mop:compute-effective-slot-definition :around
    ((class persistent-standard-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((effective (call-next-method)))
    (when (typep effective 'persistent-effective-slot-definition)
      (let ((transient-direct
              (find-if (lambda (direct)
                         (and (typep direct 'persistent-direct-slot-definition)
                              (persistent-slot-definition-transient direct)))
                       direct-slot-definitions)))
        (setf (slot-value effective 'transient) (and transient-direct t))))
    effective))

(defclass persistent-object (git-tree)
  ((repository :transient t)
   (sha :transient t)
   (loaded? :transient t)
   (entries :transient t)
   (version
    :initarg :version
    :initform 0
    :accessor persistent-object-version
    :transient t
    :type integer
    :documentation
    "This instance's \".meta\" schema/migration version, recorded
directly in \".meta\" (never as its own tree entry). Defaults to 0.")
   (%initializer-payload
    :initform nil
    :accessor %persistent-object-initializer-payload
    :transient t
    :documentation
    "The full list of INITARGS (a plist of alternating
initarg/value pairs) most recently passed to
(SETF SB-MOP:SLOT-VALUE-USING-CLASS)'s underlying SHARED-INITIALIZE,
captured by the :AFTER method below. See
%PERSISTENT-OBJECT-FILTERED-PAYLOAD, which filters this down to
exactly the entries SERIALIZE-PERSISTENT-OBJECT writes to Git."))
  (:metaclass persistent-standard-class)
  (:documentation
   "Mandatory base class for every PERSISTENT-STANDARD-CLASS
instance: supplies the (transient) bookkeeping GIT-TREE slots every
persistent CLOS instance needs (REPOSITORY, SHA, ENTRIES, LOADED?),
its own (also transient) VERSION slot, and the internal
%INITIALIZER-PAYLOAD slot SHARED-INITIALIZE populates. See
SERIALIZE-PERSISTENT-OBJECT and DESERIALIZE-PERSISTENT-OBJECT for
the on-disk representation."))

(setf (documentation 'persistent-object-version 'function)
      "Return INSTANCE's (a PERSISTENT-OBJECT) \".meta\"
schema/migration version, recorded directly in \".meta\" (never as
its own tree entry). Defaults to 0.")
(setf (documentation '%persistent-object-initializer-payload 'function)
      "Return the full list of INITARGS (a plist of alternating
initarg/value pairs) most recently passed to INSTANCE's (a
PERSISTENT-OBJECT) (SETF SB-MOP:SLOT-VALUE-USING-CLASS)'s underlying
SHARED-INITIALIZE. See %PERSISTENT-OBJECT-FILTERED-PAYLOAD, which
filters this down to exactly the entries SERIALIZE-PERSISTENT-OBJECT
writes to Git.")

(defmethod shared-initialize :after
    ((instance persistent-object) slot-names &rest initargs &key &allow-other-keys)
  "Capture every INITARG passed to INSTANCE's creation (or
re-initialization) verbatim into its %INITIALIZER-PAYLOAD slot, for
SERIALIZE-PERSISTENT-OBJECT to later filter and persist."
  (declare (ignore slot-names))
  (setf (%persistent-object-initializer-payload instance) initargs))

(defun %persistent-object-initarg-filename (initarg)
  "Return the Git tree entry filename for INITARG (a keyword): its
symbol name, downcased, with the leading colon dropped (e.g.
:USERNAME becomes \"username\")."
  (string-downcase (symbol-name initarg)))

(defun %persistent-object-filename-initarg (filename)
  "Inverse of %PERSISTENT-OBJECT-INITARG-FILENAME: return the
keyword symbol FILENAME (a Git tree entry name) denotes (e.g.
\"username\" becomes :USERNAME)."
  (intern (string-upcase filename) "KEYWORD"))

(defun %persistent-object-effective-slots (instance)
  "Return the finalized list of (CLASS-OF INSTANCE)'s effective
slot definitions, finalizing that class first via
SB-MOP:FINALIZE-INHERITANCE if it is not already finalized."
  (let ((class (class-of instance)))
    (unless (sb-mop:class-finalized-p class)
      (sb-mop:finalize-inheritance class))
    (sb-mop:class-slots class)))

(defun %persistent-object-initarg-transient-p (instance initarg)
  "Return true if INITARG (a keyword) is among the initargs of some
TRANSIENT effective slot of INSTANCE's class -- i.e. INITARG's data
must never be persisted to Git. Returns NIL for any initarg that
does not correspond to a known slot at all, matching ordinary CLOS
semantics for an unrecognized initarg."
  (some (lambda (slot)
          (and (typep slot 'persistent-effective-slot-definition)
               (persistent-slot-definition-transient slot)
               (member initarg (sb-mop:slot-definition-initargs slot))))
        (%persistent-object-effective-slots instance)))

(defun %persistent-object-filtered-payload (instance)
  "Return INSTANCE's %INITIALIZER-PAYLOAD (a plist of alternating
initarg/value pairs) as an alist of (INITARG . VALUE) conses,
filtered to drop :VERSION (recorded separately, directly in
\".meta\") and every INITARG mapping to a TRANSIENT slot (per
%PERSISTENT-OBJECT-INITARG-TRANSIENT-P): exactly the entries
SERIALIZE-PERSISTENT-OBJECT writes to Git."
  (loop for (initarg value) on (%persistent-object-initializer-payload instance) by #'cddr
        unless (or (eq initarg :version)
                   (%persistent-object-initarg-transient-p instance initarg))
          collect (cons initarg value)))

(defgeneric %persist-object-component-by-type (value)
  (:documentation
   "Persist VALUE (a GIT-OBJECT known to already satisfy (TYPEP VALUE
'GIT-OBJECT)) to Git's object database according to its concrete
type. Broken out of %PERSIST-OBJECT-COMPONENT so this dispatch is its
own generic function, with one DEFMETHOD per concrete type in place
of an ETYPECASE clause."))

(defmethod %persist-object-component-by-type ((value persistent-object))
  (serialize-persistent-object value))

(defmethod %persist-object-component-by-type ((value persistent-cons))
  (serialize-persistent-cons value))

(defmethod %persist-object-component-by-type ((value persistent-vector))
  (serialize-persistent-vector value))

(defmethod %persist-object-component-by-type ((value persistent-array))
  (serialize-persistent-array value))

(defmethod %persist-object-component-by-type ((value git-tree))
  (unless (sha value)
    (setf (sha value)
          (git-hash-object (get-repository value) "tree" (serialize-tree value)))))

(defmethod %persist-object-component-by-type ((value git-blob))
  (unless (sha value)
    (setf (sha value)
          (git-hash-object (get-repository value) "blob"
                            (serialize-atom (get-payload value))))))

;;; PERSISTENT-CONS.LISP and PERSISTENT-VECTOR.LISP each define their
;;; own analogous %PERSIST-CONS-COMPONENT-BY-TYPE/%PERSIST-VECTOR-
;;; COMPONENT-BY-TYPE generic function, with methods for every
;;; concrete GIT-OBJECT type they knew about at the time -- but
;;; neither file can add a method specializing on PERSISTENT-OBJECT
;;; itself, since PERSISTENT-OBJECT is defined here, in
;;; PERSISTENT-STANDARD-CLASS.LISP, which loads AFTER both (it
;;; depends on them, not the other way around -- see githack.asd).
;;; Without these two methods, storing a PERSISTENT-OBJECT (e.g. a
;;; DEFINE-PERSISTENT-STRUCT instance) as a raw CAR/CDR of a
;;; PERSISTENT-CONS, or as an element of a PERSISTENT-VECTOR (and so,
;;; transitively, as a value inside a PERSISTENT-HASH-TABLE, which is
;;; built from both), would silently fall through to each generic
;;; function's plain GIT-TREE method instead -- persisting only its
;;; (at that point still-empty) ENTRIES slot via a bare SERIALIZE-TREE
;;; call, entirely skipping SERIALIZE-PERSISTENT-OBJECT and so never
;;; writing the \".meta\" entry DESERIALIZE-PERSISTENT-OBJECT requires
;;; to reconstruct the object's real class later.
(defmethod %persist-cons-component-by-type ((git-object persistent-object))
  (serialize-persistent-object git-object))

(defmethod %persist-vector-component-by-type ((git-object persistent-object))
  (serialize-persistent-object git-object))

(defun %persist-object-component (value repository)
  "Return the persisted GIT-OBJECT proxy for VALUE: if VALUE is
already a GIT-OBJECT (a GIT-BLOB, plain GIT-TREE, PERSISTENT-CONS,
PERSISTENT-VECTOR, PERSISTENT-ARRAY, or nested PERSISTENT-OBJECT),
ensure it has a SHA, persisting it (and, for a compound structure,
its own children) if it does not already; otherwise, treat VALUE as
a raw Lisp atom and freshly wrap and persist it as a new GIT-BLOB via
SERIALIZE-ATOM. Mirrors PERSISTENT-CONS/PERSISTENT-VECTOR/PERSISTENT-
ARRAY's own local %PERSIST-*-COMPONENT helpers, kept separate from
GIT-TRANSACTION's shared %PERSIST-GIT-OBJECT so as not to introduce
a load-order cycle."
  (if (typep value 'git-object)
      (progn
        (%persist-object-component-by-type value)
        value)
      (make-instance 'git-blob :repository repository
                                :sha (git-hash-object repository "blob" (serialize-atom value))
                                :payload value
                                :loaded? t)))

(defparameter +persistent-object-no-documentation-readme+
  "(No documentation string was provided for this class.)"
  "The fixed README.md body content SERIALIZE-PERSISTENT-OBJECT
writes for an instance of a class with no DOCUMENTATION string of
its own (i.e. (DOCUMENTATION (CLASS-OF INSTANCE) T) returns NIL).")

(defun %persistent-object-readme-content (instance)
  "Return the UTF-8-encoded octet vector SERIALIZE-PERSISTENT-OBJECT
writes as INSTANCE's own \"README.md\" entry: a Markdown \"# \"
title line naming INSTANCE's own class (via CLASS-NAME), followed by
a blank line and that class's own DOCUMENTATION string -- i.e. its
DEFCLASS's own :DOCUMENTATION option -- or, if that class has none,
+PERSISTENT-OBJECT-NO-DOCUMENTATION-README+."
  (let ((class-symbol (class-name (class-of instance))))
    (sb-ext:string-to-octets
     (format nil "# ~A~%~%~A~%"
             class-symbol
             (or (documentation (class-of instance) t)
                 +persistent-object-no-documentation-readme+))
     :external-format :utf-8)))

(defun serialize-persistent-object (instance)
  "Compile INSTANCE (a PERSISTENT-OBJECT) into a Git tree: a
\".meta\" entry holding (:TAG :CLOS :CLASS \"CLASS-NAME\" :PACKAGE
\"PACKAGE-NAME\" :VERSION n), a \"README.md\" entry holding a
Markdown title naming INSTANCE's own class followed by that class's
own DOCUMENTATION string (via %PERSISTENT-OBJECT-README-CONTENT),
and one further entry per non-transient initarg recorded in
INSTANCE's %INITIALIZER-PAYLOAD (via %PERSISTENT-OBJECT-FILTERED-
PAYLOAD) -- filename the initarg's own downcased name, each a proxy
pointer to that initarg's own (recursively persisted, via
%PERSIST-OBJECT-COMPONENT) GIT-OBJECT value. INSTANCE's own
transient slots (VERSION, and any other slot marked :TRANSIENT T)
are entirely excluded, appearing nowhere in the
Git tree. Returns INSTANCE's own SHA, doing nothing further if
INSTANCE already has one."
  (or (sha instance)
      (let* ((repository (get-repository instance))
             (class-symbol (class-name (class-of instance)))
             (class-name (symbol-name class-symbol))
             (class-package (package-name (symbol-package class-symbol)))
             (payload (%persistent-object-filtered-payload instance))
             (tree-entries
               (mapcar (lambda (pair)
                         (cons (%persistent-object-initarg-filename (car pair))
                               (%persist-object-component (cdr pair) repository)))
                       payload))
             (meta-blob (make-instance 'git-blob :repository repository
                                        :sha (git-hash-object
                                              repository "blob"
                                              (%serialize-plist
                                               (list :tag :clos
                                                     :class class-name
                                                    :package class-package
                                                    :version (persistent-object-version instance))))))
             (readme-blob (make-instance 'git-blob :repository repository
                                          :sha (git-hash-object
                                                repository "blob"
                                                (%persistent-object-readme-content instance)))))
        (setf (get-entries instance) (list* (cons ".meta" meta-blob)
                                             (cons "README.md" readme-blob)
                                             tree-entries))
        (setf (sha instance) (git-hash-object repository "tree" (serialize-tree instance)))
        (setf (get-loaded? instance) t)
        (sha instance))))

(defun deserialize-persistent-object (tree)
  "TREE is a hollow GIT-OBJECT proxy (typically freshly
INFLATE-GIT-PROXY'd) already bound to its own REPOSITORY and SHA.
Fetch and parse TREE's raw Git tree bytes (via GIT-CAT-FILE and
DESERIALIZE-TREE), read its \".meta\" entry to learn the originally
serialized CLOS CLASS-NAME and VERSION, reconstruct an initarg list
from every other entry other than \".meta\"/\"README.md\" (each
already a hollow GIT-OBJECT proxy, via INFLATE-GIT-PROXY, exactly as
DESERIALIZE-TREE provides -- so no initarg's own value is ever
fetched eagerly by this function, only later, on demand,
transparently, by SLOT-VALUE-USING-CLASS below), and return a fresh
(MAKE-INSTANCE CLASS-NAME ...) of that class, bound to TREE's own
REPOSITORY and SHA and marked already loaded. \"README.md\" (if
present -- older trees, serialized before SERIALIZE-PERSISTENT-
OBJECT wrote one, may lack it) is otherwise ignored: it exists only
for a human Git user browsing the object database, never as an
initarg. Signals an error if TREE's entries do not include \".meta\",
or if its \".meta\" blob's :TAG is not :CLOS."
  (let* ((repository (get-repository tree))
         (entries (deserialize-tree repository (git-cat-file repository (sha tree))))
         (meta-entry (assoc ".meta" entries :test #'string=)))
    (unless meta-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent object tree: missing \".meta\" entry."))
    (let ((meta (%deserialize-plist (git-cat-file repository (sha (cdr meta-entry))))))
      (unless (eq (getf meta :tag) :clos)
        (error 'malformed-git-object-error
               :format-control "Malformed persistent object .meta blob: ~S."
               :format-arguments (list meta)))
      (let* ((class-name (intern (getf meta :class) (getf meta :package)))
             (version (getf meta :version))
             (initargs
               (loop for entry in entries
                     unless (member (car entry) '(".meta" "README.md") :test #'string=)
                       append (list (%persistent-object-filename-initarg (car entry)) (cdr entry))))
             (instance (apply #'make-instance class-name :version version :repository repository initargs)))
        (setf (sha instance) (sha tree))
        (setf (get-entries instance) entries)
        (setf (get-loaded? instance) t)
        instance))))

(defun %persistent-tree-tag (repository tree)
  "Return the :TAG keyword recorded in TREE's own \".meta\" entry
(:CLOS, :CONS, :VECTOR, :ARRAY, or :ATOMIC-WRAPPER -- see
SERIALIZE-PERSISTENT-OBJECT/-CONS/-VECTOR/-ARRAY and WRAP-ATOMIC-
COMMIT-ROOT), fetched via GIT-CAT-FILE and %DESERIALIZE-PLIST from
TREE's \".meta\" entry (TREE's own ENTRIES must already be loaded);
or NIL for any ordinary, untagged GIT-TREE with no \".meta\" entry
at all."
  (let ((meta-entry (assoc ".meta" (get-entries tree) :test #'string=)))
    (and meta-entry
         (getf (%deserialize-plist (git-cat-file repository (sha (cdr meta-entry)))) :tag))))

(defun %redispatch-persistent-tree (tree)
  "Return the correctly, specifically typed proxy for TREE (a plain,
not-yet-more-specifically-typed GIT-TREE, with its own ENTRIES
already loaded): a freshly DESERIALIZE-PERSISTENT-OBJECT'd CLOS
instance, DESERIALIZE-PERSISTENT-CONS'd/-VECTOR'd/-ARRAY'd hollow
proxy, if TREE's own \".meta\" entry reveals (via %PERSISTENT-TREE-
TAG) that it actually holds one of those compound types; or TREE
itself, unchanged, if it has no \".meta\" entry at all (an ordinary,
untagged GIT-TREE). This is exactly the re-dispatch DESERIALIZE-
PERSISTENT-OBJECT's own initarg reconstruction cannot itself perform
-- since DESERIALIZE-TREE/INFLATE-GIT-PROXY only ever distinguish a
raw GIT-BLOB from a plain GIT-TREE, never a nested PERSISTENT-CONS/
-VECTOR/-ARRAY/-OBJECT -- needed both for a fresh commit root and for
any persistent-object slot (e.g. a PERSISTENT-HASH-TABLE's own
BUCKETS slot) whose stored value is one of these compound types."
  (let* ((repository (get-repository tree))
         (sha (sha tree))
         (tag (%persistent-tree-tag repository tree)))
    (case tag
      (:clos (deserialize-persistent-object tree))
      ((:cons :vector :array)
       (let* ((tree-octets (git-cat-file repository sha))
              (meta-entry (assoc ".meta" (get-entries tree) :test #'string=))
              (meta-octets (git-cat-file repository (sha (cdr meta-entry))))
              (hollow (make-instance (ecase tag
                                        (:cons 'persistent-cons)
                                        (:vector 'persistent-vector)
                                        (:array 'persistent-array))
                                      :repository repository :sha sha)))
         (ecase tag
           (:cons (deserialize-persistent-cons hollow tree-octets meta-octets))
           (:vector (deserialize-persistent-vector hollow tree-octets meta-octets))
           (:array (deserialize-persistent-array hollow tree-octets meta-octets)))))
      (t tree))))

(defun %resolve-persistent-slot-value (value)
  "Return the real Lisp data VALUE (a slot's raw stored value)
represents: unchanged, if VALUE is not a GIT-OBJECT proxy at all;
its decoded PAYLOAD, ensuring VALUE is first loaded via
%ENSURE-BLOB-LOADED, if VALUE is a GIT-BLOB; the result of
%REDISPATCH-PERSISTENT-TREE, if VALUE is a plain (not yet more
specifically typed) GIT-TREE proxy (retyped into a PERSISTENT-CONS/
-VECTOR/-ARRAY/-OBJECT if its own \".meta\" entry says so, or left
unchanged otherwise); or VALUE itself, unchanged, for any other kind
of GIT-OBJECT (PERSISTENT-CONS, PERSISTENT-VECTOR, PERSISTENT-ARRAY,
or already-typed PERSISTENT-OBJECT), since those already are the
correct, lazily self-loading proxy for their own compound data."
  (cond
    ((typep value 'git-blob) (get-payload (%ensure-blob-loaded value)))
    ((and (typep value 'git-tree)
          (not (typep value '(or persistent-cons persistent-vector persistent-array persistent-object))))
     (let ((repository (get-repository value)))
       (%ensure-tree-entries-loaded repository value))
     (%redispatch-persistent-tree value))
    (t value)))

(defmethod sb-mop:slot-value-using-class
    ((class persistent-standard-class) instance (slot persistent-effective-slot-definition))
  "Transparently resolve and cache any GIT-OBJECT proxy held raw in
this slot: if the raw stored value is a GIT-OBJECT, replace it (via
(SETF SB-MOP:SLOT-VALUE-USING-CLASS)) with its fully resolved Lisp
value (per %RESOLVE-PERSISTENT-SLOT-VALUE) and return that resolved
value; otherwise return the raw value unchanged. To the end user,
(SLOT-VALUE OBJ 'HEAVY-DATA) immediately returns the real Lisp data,
never a proxy."
  (let ((value (call-next-method)))
    (if (typep value 'git-object)
        (let ((resolved (%resolve-persistent-slot-value value)))
          (setf (sb-mop:slot-value-using-class class instance slot) resolved)
          resolved)
        value)))
