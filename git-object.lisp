;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; A lightweight proxy layer mirroring Git's native object model
;;; (blob / tree / commit) as CLOS classes. Each proxy identifies a
;;; Git object by SHA and lazily loads its payload from disk, so that
;;; later serialization/deserialization code can dispatch on the
;;; concrete subclass via ordinary generic functions.

(defclass git-object ()
  ((sha
    :initarg :sha
    :initform nil
    :accessor sha
    :type (or null string)
    :documentation "The object's Git SHA-1, or NIL if not yet computed.")
   (repository
    :initarg :repository
    :reader get-repository
    :documentation "The repository this object belongs to.")
   (loaded?
    :initarg :loaded?
    :initform nil
    :accessor get-loaded?
    :type boolean
    :documentation
    "True once this object's payload has been fetched from disk. Supports lazy-loading: a proxy may exist with only its SHA known."))
  (:documentation
   "Abstract base class for proxies mirroring Git's native object
model (blob, tree, commit). Never instantiated directly; use one of
its subclasses, typically via INFLATE-GIT-PROXY."))

(defmethod initialize-instance :after ((object git-object) &key)
  "Signal an error if OBJECT is a direct instance of the abstract
GIT-OBJECT class rather than one of its concrete subclasses."
  (when (eq (class-of object) (find-class 'git-object))
    (error 'invalid-argument-error
           :format-control "GIT-OBJECT is abstract and may not be instantiated directly.")))

(defclass git-blob (git-object)
  ((payload
    :initarg :payload
    :initform nil
    :accessor get-payload
    :documentation
    "The blob's decoded Lisp atom (an INTEGER, SYMBOL, KEYWORD,
SINGLE-FLOAT, DOUBLE-FLOAT, CHARACTER, STRING, BIT-VECTOR, or 1-D
(UNSIGNED-BYTE 8) vector), or NIL if not yet loaded/decoded. See
SERIALIZE-ATOM and DESERIALIZE-ATOM for the on-disk representation."))
  (:documentation
   "Proxy for a Git blob: raw file data, decoded to a single Lisp
atom held in PAYLOAD."))

(defclass git-tree (git-object)
  ((entries
    :initarg :entries
    :initform nil
    :accessor get-entries
    :documentation
    "An alist of (FILENAME . GIT-OBJECT) pairs describing this
tree's directory listing, where FILENAME is a string and each cdr is
a GIT-BLOB or GIT-TREE proxy (possibly still unloaded), or NIL if
not yet loaded/decoded. See SERIALIZE-TREE and DESERIALIZE-TREE for
the on-disk representation."))
  (:documentation "Proxy for a Git tree: a directory listing of named entries."))

(defclass git-commit (git-object)
  ((tree
    :initarg :tree
    :initform nil
    :accessor get-tree
    :documentation
    "The GIT-TREE proxy for this commit's root directory snapshot,
or NIL if not yet loaded/decoded.")
   (parents
    :initarg :parents
    :initform nil
    :accessor get-parents
    :documentation
    "A list of GIT-COMMIT proxies for this commit's parent commits:
empty for an initial commit, more than one for a merge commit, or
NIL if not yet loaded/decoded.")
   (author
    :initarg :author
    :initform nil
    :accessor get-author
    :type (or null string)
    :documentation
    "The commit's author, e.g. \"The Boss <boss@githack.local>\", or
NIL if not yet loaded/decoded.")
   (committer
    :initarg :committer
    :initform nil
    :accessor get-committer
    :type (or null string)
    :documentation
    "The commit's committer, usually identical to AUTHOR, or NIL if
not yet loaded/decoded.")
   (timestamp
    :initarg :timestamp
    :initform nil
    :accessor get-timestamp
    :type (or null integer)
    :documentation
    "The commit's Unix epoch timestamp (shared by both its author
and committer signatures), or NIL if not yet loaded/decoded.")
   (message
    :initarg :message
    :initform nil
    :accessor get-message
    :type (or null string)
    :documentation
    "The commit's raw message text, or NIL if not yet loaded/decoded."))
  (:documentation
   "Proxy for a Git commit: a history anchor pointing to a root
GIT-TREE snapshot and zero or more parent GIT-COMMIT proxies. See
SERIALIZE-COMMIT and DESERIALIZE-COMMIT for the on-disk
representation."))

;;; GIT-TYPE is assumed to already exist: it shells out to
;;; `git cat-file -t <sha>` against REPOSITORY and returns one of the
;;; strings "blob", "tree", or "commit". Declared here only so this
;;; file can be compiled/loaded ahead of that helper's definition.
(declaim (ftype (function (t string) string) git-type))

(defun inflate-git-proxy (repository sha)
  "Return a new, unloaded GIT-OBJECT proxy for SHA, of the concrete
subclass (GIT-BLOB, GIT-TREE, or GIT-COMMIT) matching its actual Git
object type as reported by GIT-TYPE."
  (check-type sha string)
  (let ((type (git-type repository sha)))
    (make-instance
     (cond
       ((string= type "blob") 'git-blob)
       ((string= type "tree") 'git-tree)
       ((string= type "commit") 'git-commit)
       (t (error 'malformed-git-object-error
                 :format-control "Unrecognized Git object type ~S for SHA ~S."
                 :format-arguments (list type sha))))
     :sha sha
     :repository repository)))
