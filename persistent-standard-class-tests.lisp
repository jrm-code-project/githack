;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-standard-class-suite
  :in githack-suite
  :description "Tests for PERSISTENT-STANDARD-CLASS, PERSISTENT-OBJECT, SERIALIZE-PERSISTENT-OBJECT, DESERIALIZE-PERSISTENT-OBJECT, and the transparent SLOT-VALUE-USING-CLASS proxy resolution.")

(in-suite persistent-standard-class-suite)

;;; Fixture classes, deliberately defined in GITHACK-TEST (not
;;; GITHACK) to exercise DESERIALIZE-PERSISTENT-OBJECT's :PACKAGE
;;; round-trip.

(defclass persistent-widget (persistent-object)
  ((name
    :initarg :name
    :initform nil
    :accessor widget-name)
   (tag
    :initarg :tag
    :initform nil
    :accessor widget-tag)
   (cache
    :initarg :cache
    :initform nil
    :accessor widget-cache
    :transient t))
  (:metaclass persistent-standard-class)
  (:documentation
   "A fixture class used to test PERSISTENT-STANDARD-CLASS/
SERIALIZE-PERSISTENT-OBJECT, including that this very documentation
string ends up in a persisted instance's own \"README.md\" entry."))

(defclass persistent-owner (persistent-object)
  ((label
    :initarg :label
    :initform nil
    :accessor owner-label)
   (widget
    :initarg :widget
    :initform nil
    :accessor owner-widget))
  (:metaclass persistent-standard-class))

(test persistent-object-is-a-git-tree
  "A PERSISTENT-OBJECT subclass instance is a GIT-TREE (so it reuses
ENTRIES, INFER-GIT-MODE, and SERIALIZE-TREE), and its VERSION slot
defaults to 0."
  (let ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x)))
    (is (typep widget 'git-tree))
    (is (= 0 (persistent-object-version widget)))
    (is (string= "40000" (infer-git-mode widget)))))

(test shared-initialize-captures-initializer-payload
  "Creating a PERSISTENT-OBJECT instance captures every INITARG
passed to it, verbatim, in its (internal) %INITIALIZER-PAYLOAD
slot."
  (let ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x :cache 42)))
    (let ((payload (%persistent-object-initializer-payload widget)))
      (is (eq :dummy-repo (getf payload :repository)))
      (is (string= "Bob" (getf payload :name)))
      (is (eq :x (getf payload :tag)))
      (is (= 42 (getf payload :cache))))))

(test transient-slot-defaults-to-nil
  "PERSISTENT-DIRECT-SLOT-DEFINITION's :TRANSIENT slot option
defaults to NIL: an ordinary slot with no :TRANSIENT option is not
transient."
  (let* ((class (find-class 'persistent-widget))
         (name-slot (find 'name (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name)))
    (is (not (persistent-slot-definition-transient name-slot)))))

(test transient-slot-flag-propagates-to-effective-slot
  "A slot declared :TRANSIENT T (WIDGET-CACHE) has a TRUE effective
TRANSIENT flag; an ordinary slot (WIDGET-NAME) does not."
  (let* ((class (find-class 'persistent-widget))
         (cache-slot (find 'cache (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))
         (name-slot (find 'name (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name)))
    (is (persistent-slot-definition-transient cache-slot))
    (is (not (persistent-slot-definition-transient name-slot)))))

(test serialize-persistent-object-excludes-transient-initargs
  "SERIALIZE-PERSISTENT-OBJECT's Git tree includes an entry for
every non-transient initarg (\"name\", \"tag\"), but never for a
transient one (\"cache\"), nor for \"repository\" (a bookkeeping
GIT-TREE slot, marked :TRANSIENT T by PERSISTENT-OBJECT itself)."
  (let ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x :cache 42)))
    (with-fake-git-hash-object ()
      (serialize-persistent-object widget))
    (is (equal (list ".meta" "README.md" "name" "tag") (mapcar #'car (get-entries widget))))))

(test serialize-persistent-object-writes-readme-from-class-documentation
  "SERIALIZE-PERSISTENT-OBJECT's \"README.md\" entry begins with a
Markdown \"# \" title line naming INSTANCE's own class, followed by
that class's own DOCUMENTATION string, UTF-8 encoded."
  (let* ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x))
         (calls '()))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-object widget))
    (let* ((readme-entry (cdr (assoc "README.md" (get-entries widget) :test #'string=)))
           (readme-octets (third (find (sha readme-entry) calls
                                        :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                        :test #'string=)))
           (readme-text (sb-ext:octets-to-string readme-octets :external-format :utf-8)))
      (is (eql 0 (search (format nil "# PERSISTENT-WIDGET~%") readme-text)))
      (is (search (documentation (find-class 'persistent-widget) t) readme-text)))))

(test serialize-persistent-object-writes-placeholder-readme-for-undocumented-class
  "SERIALIZE-PERSISTENT-OBJECT falls back to +PERSISTENT-OBJECT-NO-
DOCUMENTATION-README+ (after the usual class-name title line) for a
class with no DOCUMENTATION string of its own."
  (let* ((owner (make-instance 'persistent-owner :repository :dummy-repo :label "Alice"))
         (calls '()))
    (is (null (documentation (find-class 'persistent-owner) t)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-object owner))
    (let* ((readme-entry (cdr (assoc "README.md" (get-entries owner) :test #'string=)))
           (readme-octets (third (find (sha readme-entry) calls
                                        :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                        :test #'string=)))
           (readme-text (sb-ext:octets-to-string readme-octets :external-format :utf-8)))
      (is (eql 0 (search (format nil "# PERSISTENT-OWNER~%") readme-text)))
      (is (search githack::+persistent-object-no-documentation-readme+ readme-text)))))

(test serialize-persistent-object-writes-standard-meta
  "SERIALIZE-PERSISTENT-OBJECT's \".meta\" blob holds (:TAG :CLOS
:CLASS \"PERSISTENT-WIDGET\" :PACKAGE \"GITHACK-TEST\" :VERSION 0)."
  (let* ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x))
         (calls '()))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-object widget))
    (let* ((meta-entry (cdr (assoc ".meta" (get-entries widget) :test #'string=)))
           (meta-octets (third (find (sha meta-entry) calls
                                      :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                      :test #'string=)))
           (meta (githack::%deserialize-plist meta-octets)))
      (is (eq :clos (getf meta :tag)))
      (is (string= "PERSISTENT-WIDGET" (getf meta :class)))
      (is (string= "GITHACK-TEST" (getf meta :package)))
      (is (= 0 (getf meta :version))))))

(test serialize-persistent-object-is-idempotent
  "SERIALIZE-PERSISTENT-OBJECT does nothing (beyond returning the
existing SHA) for an instance that already has one."
  (let* ((sha "7777777777777777777777777777777777777777")
         (widget (make-instance 'persistent-widget :repository :dummy-repo :sha sha)))
    (is (string= sha (serialize-persistent-object widget)))
    (is (null (get-entries widget)))))

(test serialize-persistent-object-wraps-raw-atoms-as-blobs
  "SERIALIZE-PERSISTENT-OBJECT automatically wraps a raw Lisp atom
initarg value (a plain STRING, here) in a fresh GIT-BLOB via
SERIALIZE-ATOM, rather than requiring the caller to pre-wrap it."
  (let ((widget (make-instance 'persistent-widget :repository :dummy-repo :name "Bob" :tag :x)))
    (with-fake-git-object-store ()
      (serialize-persistent-object widget))
    (let ((name-object (cdr (assoc "name" (get-entries widget) :test #'string=))))
      (is (typep name-object 'git-blob))
      (is (string= "Bob" (get-payload name-object))))))

(defun %persist-widget-in-fake-store (name tag)
  "Build and persist (against the current WITH-FAKE-GIT-OBJECT-STORE)
a fresh, unpersisted PERSISTENT-WIDGET with NAME/TAG, returning it."
  (let ((widget (make-instance 'persistent-widget :repository :dummy-repo :name name :tag tag)))
    (serialize-persistent-object widget)
    widget))

(defun %widget-git-type-mapping (widget)
  "Return the (SHA . TYPE) alist WITH-FAKE-GIT-TYPE needs to
correctly INFLATE-GIT-PROXY every entry of an already-persisted
WIDGET's own tree."
  (list* (cons (sha widget) "tree")
         (mapcar (lambda (entry) (cons (sha (cdr entry)) (if (typep (cdr entry) 'git-tree) "tree" "blob")))
                 (get-entries widget))))

(test deserialize-persistent-object-round-trips-with-serialize
  "DESERIALIZE-PERSISTENT-OBJECT, given a hollow proxy for an
already-persisted PERSISTENT-WIDGET's SHA, reconstructs a fresh
instance of that same concrete class, with hollow GIT-OBJECT
initarg proxies for NAME/TAG and VERSION 0."
  (with-fake-git-object-store ()
    (let* ((original (%persist-widget-in-fake-store "Bob" :x))
           (hollow (make-instance 'git-tree :repository :dummy-repo :sha (sha original))))
      (with-fake-git-type ((%widget-git-type-mapping original))
        (let ((rehydrated (deserialize-persistent-object hollow)))
          (is (typep rehydrated 'persistent-widget))
          (is (string= (sha original) (sha rehydrated)))
          (is (get-loaded? rehydrated))
          (is (= 0 (persistent-object-version rehydrated)))
          ;; SLOT-VALUE always goes through SLOT-VALUE-USING-CLASS,
          ;; so even this very first read transparently resolves
          ;; the hollow GIT-OBJECT proxy straight through to its
          ;; decoded atom -- never exposing the proxy itself.
          (is (string= "Bob" (slot-value rehydrated 'name)))
          (is (eq :x (slot-value rehydrated 'tag))))))))

(test deserialize-persistent-object-signals-error-for-non-clos-tree
  "DESERIALIZE-PERSISTENT-OBJECT signals an error when handed a
tree whose \".meta\" is missing, or whose \"tag\" is not :CLOS."
  (let* ((blob-sha "8888888888888888888888888888888888888888")
         (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
         (incomplete-tree (make-instance 'git-tree :repository :dummy-repo
                                                    :entries (list (cons "value" blob))))
         (tree-octets (serialize-tree incomplete-tree)))
    (with-fake-git-cat-file ((list (cons "9999999999999999999999999999999999999999" tree-octets)))
      (let ((hollow (make-instance 'git-tree :repository :dummy-repo
                                    :sha "9999999999999999999999999999999999999999")))
        (with-fake-git-type ((list (cons blob-sha "blob")))
          (signals error (deserialize-persistent-object hollow)))))))

(test slot-value-using-class-transparently-resolves-atom-proxies
  "Reading a slot whose raw stored value is a GIT-BLOB proxy (as
DESERIALIZE-PERSISTENT-OBJECT leaves it) transparently returns the
decoded atom, and caches it: SLOT-VALUE never exposes a GIT-OBJECT
to the caller."
  (with-fake-git-object-store ()
    (let* ((original (%persist-widget-in-fake-store "Bob" :x))
           (hollow (make-instance 'git-tree :repository :dummy-repo :sha (sha original))))
      (with-fake-git-type ((%widget-git-type-mapping original))
        (let ((rehydrated (deserialize-persistent-object hollow)))
          (is (string= "Bob" (widget-name rehydrated)))
          (is (eq :x (widget-tag rehydrated)))
          ;; The raw slot must now hold the resolved, cached value,
          ;; not the GIT-BLOB proxy anymore.
          (is (string= "Bob" (slot-value rehydrated 'name)))
          ;; A second read must not error even though the proxy has
          ;; already been replaced by its resolved value.
          (is (string= "Bob" (widget-name rehydrated))))))))

(test slot-value-using-class-transparently-resolves-nested-persistent-object
  "Reading a slot whose raw stored value is a plain (not yet
specifically typed) GIT-TREE proxy that turns out to be a nested
PERSISTENT-OBJECT's own tree transparently returns a fully
reconstructed instance of that nested object's own concrete class,
not a bare GIT-TREE."
  (with-fake-git-object-store ()
    (let* ((widget (%persist-widget-in-fake-store "Gadget" :y))
           (owner (make-instance 'persistent-owner :repository :dummy-repo :label "Owner" :widget widget)))
      (serialize-persistent-object owner)
      (let ((hollow (make-instance 'git-tree :repository :dummy-repo :sha (sha owner))))
        (with-fake-git-type ((append (%widget-git-type-mapping owner) (%widget-git-type-mapping widget)))
          (let ((rehydrated (deserialize-persistent-object hollow)))
            (is (typep rehydrated 'persistent-owner))
            (is (string= "Owner" (owner-label rehydrated)))
            (let ((nested (owner-widget rehydrated)))
              (is (typep nested 'persistent-widget))
              (is (string= "Gadget" (widget-name nested)))
              (is (eq :y (widget-tag nested))))))))))
