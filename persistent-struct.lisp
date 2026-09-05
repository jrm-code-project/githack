;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; DEFINE-PERSISTENT-STRUCT gives DEFSTRUCT's familiar surface syntax
;;; (a struct name, optional :CONC-NAME, and slot descriptions of the
;;; form SLOT-NAME or (SLOT-NAME DEFAULT-INITFORM &KEY TYPE READ-ONLY
;;; TRANSIENT)) while expanding into an ordinary DEFCLASS with
;;; :METACLASS PERSISTENT-STANDARD-CLASS, so the generated class gets
;;; the full PERSISTENT-OBJECT machinery (transparent Git tree
;;; serialization, transient-slot filtering, SLOT-VALUE-USING-CLASS
;;; proxy auto-resolution) for free, alongside DEFSTRUCT's usual
;;; MAKE-<NAME> constructor and <NAME>-P predicate.

(defclass persistent-struct (persistent-object)
  ()
  (:metaclass persistent-standard-class)
  (:documentation
   "Mandatory base class for every DEFINE-PERSISTENT-STRUCT-generated
class, itself a PERSISTENT-OBJECT (and so, transitively, a GIT-TREE
and STANDARD-OBJECT), identifying every such generated class as one
family for EQL/TYPEP-based introspection."))

(defun %persistent-struct-parse-name-and-options (name-and-options)
  "Return two values, the struct's NAME (a symbol) and its CONC-NAME
prefix (a string, possibly empty), parsed DEFSTRUCT-style from
NAME-AND-OPTIONS: either a bare symbol NAME (CONC-NAME then defaults
to \"<NAME>-\"), or a list (NAME &KEY CONC-NAME) (CONC-NAME defaults
identically if not supplied at all, but is the empty string if
explicitly supplied as NIL, exactly as DEFSTRUCT's own :CONC-NAME NIL
disables any prefix)."
  (if (symbolp name-and-options)
      (values name-and-options (concatenate 'string (string name-and-options) "-"))
      (destructuring-bind (name &key (conc-name nil conc-name-supplied?)) name-and-options
        (values name
                (cond
                  ((not conc-name-supplied?) (concatenate 'string (string name) "-"))
                  ((null conc-name) "")
                  (t (string conc-name)))))))

(defun %persistent-struct-parse-slot-description (slot-description)
  "Return a plist (:NAME :INITFORM :TYPE :READ-ONLY :TRANSIENT)
parsed DEFSTRUCT-style from SLOT-DESCRIPTION: either a bare symbol
NAME (INITFORM defaults to NIL, with no :TYPE/:READ-ONLY/:TRANSIENT),
or a list (NAME &OPTIONAL INITFORM &KEY TYPE READ-ONLY TRANSIENT)."
  (if (symbolp slot-description)
      (list :name slot-description :initform nil :type nil :read-only nil :transient nil)
      (destructuring-bind (name &optional initform &key type read-only transient) slot-description
        (list :name name :initform initform :type type :read-only read-only :transient transient))))

(defun %persistent-struct-slot-accessor-name (slot conc-name package)
  "Return the symbol, interned in PACKAGE, DEFINE-PERSISTENT-STRUCT
generates as SLOT's :ACCESSOR (or :READER, if SLOT is :READ-ONLY):
CONC-NAME concatenated with SLOT's own :NAME."
  (intern (concatenate 'string conc-name (string (getf slot :name))) package))

(defun %persistent-struct-class-slot-spec (slot conc-name package)
  "Return the CLOS slot specifier DEFINE-PERSISTENT-STRUCT emits for
SLOT within its generated DEFCLASS: :INITARG the keyword version of
SLOT's :NAME, :INITFORM its :INITFORM, :ACCESSOR (or :READER, if
:READ-ONLY) CONC-NAME concatenated with :NAME (via
%PERSISTENT-STRUCT-SLOT-ACCESSOR-NAME), and, if present, SLOT's own
:TYPE and :TRANSIENT passed through verbatim so
PERSISTENT-STANDARD-CLASS's MOP can see them."
  (let ((name (getf slot :name))
        (accessor (%persistent-struct-slot-accessor-name slot conc-name package)))
    `(,name
      :initarg ,(intern (symbol-name name) "KEYWORD")
      :initform ,(getf slot :initform)
      ,(if (getf slot :read-only) :reader :accessor) ,accessor
      ,@(when (getf slot :type) (list :type (getf slot :type)))
      ,@(when (getf slot :transient) (list :transient t)))))

(defmacro define-persistent-struct (name-and-options &rest slot-descriptions)
  "Define a PERSISTENT-STANDARD-CLASS-metaclassed class, DEFSTRUCT-
style: NAME-AND-OPTIONS is a bare struct name symbol, or a list
(NAME &KEY CONC-NAME); each of SLOT-DESCRIPTIONS is a bare slot name
symbol, or a list (NAME DEFAULT-INITFORM &KEY TYPE READ-ONLY
TRANSIENT). Expands into a DEFCLASS named NAME, inheriting from
PERSISTENT-STRUCT, with :METACLASS PERSISTENT-STANDARD-CLASS and one
slot per description (see %PERSISTENT-STRUCT-CLASS-SLOT-SPEC),
alongside a MAKE-<NAME> constructor function (accepting the same
&KEY defaults as the slot descriptions themselves, and calling
MAKE-INSTANCE) and a <NAME>-P predicate function (calling TYPEP)."
  (multiple-value-bind (name conc-name) (%persistent-struct-parse-name-and-options name-and-options)
    (let* ((package (symbol-package name))
           (slots (mapcar #'%persistent-struct-parse-slot-description slot-descriptions))
           (class-slot-specs (mapcar (lambda (slot) (%persistent-struct-class-slot-spec slot conc-name package))
                                      slots))
           (make-name (intern (concatenate 'string "MAKE-" (string name)) package))
           (predicate-name (intern (concatenate 'string (string name) "-P") package)))
      `(progn
         (defclass ,name (persistent-struct)
           ,class-slot-specs
           (:metaclass persistent-standard-class))
         (defun ,make-name (&key ,@(mapcar (lambda (slot) (list (getf slot :name) (getf slot :initform))) slots))
           (make-instance ',name
                          ,@(mapcan (lambda (slot)
                                      (list (intern (symbol-name (getf slot :name)) "KEYWORD") (getf slot :name)))
                                    slots)))
         (defun ,predicate-name (object)
           (typep object ',name))))))
