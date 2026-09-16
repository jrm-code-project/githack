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

;; DEFCLASS's own slot :DOCUMENTATION option attaches a docstring to
;; the *slot definition* (visible via SB-MOP:SLOT-DEFINITION-
;; DOCUMENTATION), not to the reader/accessor *function* DEFCLASS
;; implicitly generates for it (visible via (DOCUMENTATION 'NAME
;; 'FUNCTION)); the two are unrelated namespaces. Since SHA and
;; GET-REPOSITORY/GET-LOADED? are exported as part of GitHack's
;; public API, they get their own (SETF DOCUMENTATION) here as well,
;; mirroring each slot's own :DOCUMENTATION text above. GET-REPOSITORY
;; is reused, with the same general meaning, by GIT-BRANCH and
;; BRANCH-NOT-FOUND-ERROR (see git-branch.lisp/conditions.lisp).
(setf (documentation 'sha 'function)
      "Return OBJECT's (a GIT-OBJECT) Git SHA-1, or NIL if not yet computed (not yet persisted).")
(setf (documentation 'get-repository 'function)
      "Return the repository OBJECT belongs to: for a GIT-OBJECT
proxy or a GIT-BRANCH, the REPOSITORY it was constructed with; for a
BRANCH-NOT-FOUND-ERROR, the repository that was searched.")
(setf (documentation 'get-loaded? 'function)
      "Return true once OBJECT's (a GIT-OBJECT) payload has been
fetched from disk. Supports lazy-loading: a proxy may exist with
only its SHA known, GET-LOADED? then being NIL until its content is
actually fetched.")

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

(setf (documentation 'get-payload 'function)
      "Return BLOB's (a GIT-BLOB) decoded Lisp atom (an INTEGER,
SYMBOL, KEYWORD, SINGLE-FLOAT, DOUBLE-FLOAT, CHARACTER, STRING,
BIT-VECTOR, or 1-D (UNSIGNED-BYTE 8) vector), or NIL if not yet
loaded/decoded. See SERIALIZE-ATOM and DESERIALIZE-ATOM.")

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

(setf (documentation 'get-entries 'function)
      "Return TREE's (a GIT-TREE) alist of (FILENAME . GIT-OBJECT)
pairs describing its directory listing, or NIL if not yet
loaded/decoded. See SERIALIZE-TREE and DESERIALIZE-TREE.")

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

(setf (documentation 'get-tree 'function)
      "Return COMMIT's (a GIT-COMMIT) GIT-TREE proxy for its root
directory snapshot, or NIL if not yet loaded/decoded.")
(setf (documentation 'get-parents 'function)
      "Return a list of GIT-COMMIT proxies for OBJECT's parent
commits: for a GIT-COMMIT, empty for an initial commit, more than one
for a merge commit, or NIL if not yet loaded/decoded; for a
GIT-TRANSACTION, the parents its next commit will record (see
git-transaction.lisp).")
(setf (documentation 'get-author 'function)
      "Return OBJECT's author signature, e.g. \"The Boss
<boss@githack.local>\": for a GIT-COMMIT, or NIL if not yet
loaded/decoded; for a GIT-REPOSITORY or GIT-TRANSACTION, the cascaded
default new commits inherit unless overridden.")
(setf (documentation 'get-committer 'function)
      "Return OBJECT's committer signature, usually identical to its
author: for a GIT-COMMIT, or NIL if not yet loaded/decoded; for a
GIT-REPOSITORY or GIT-TRANSACTION, the cascaded default new commits
inherit unless overridden.")
(setf (documentation 'get-timestamp 'function)
      "Return COMMIT's (a GIT-COMMIT) Unix epoch timestamp (shared by
both its author and committer signatures), or NIL if not yet
loaded/decoded.")
(setf (documentation 'get-message 'function)
      "Return OBJECT's raw message text: for a GIT-COMMIT, or NIL if
not yet loaded/decoded; for a GIT-REPOSITORY or GIT-TRANSACTION, the
cascaded default new commits inherit unless overridden.")

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

;;; THREAD SAFETY: multiple threads in the same Lisp image may
;;; safely share and concurrently navigate a single, already-
;;; constructed GIT-OBJECT/PERSISTENT-OBJECT proxy graph -- e.g. a
;;; GIT-BRANCH's own TARGET commit, handed to several worker threads
;;; at once -- via the small set of lightweight, mostly lock-free
;;; primitives below, used throughout every %ENSURE-*-LOADED lazy-
;;; load function in this codebase (atomic-wrapper.lisp,
;;; persistent-cons.lisp, persistent-vector.lisp, persistent-
;;; array.lisp, persistent-wttree.lisp) and every per-element cache
;;; (chief among them, PERSISTENT-VECTOR-REF's own index cache): see
;;; %PUBLISH-LOADED!, %CAS-INSTALL-ONCE, and WITH-OBJECT-LOAD-LOCK.
;;; Concurrent WRITERS (distinct GIT-TRANSACTIONs producing brand-new
;;; proxy instances) were already safe, by construction, since
;;; GitHack's persistent data model never mutates an already-
;;; persisted proxy in place; this closes the remaining gap of
;;; multiple threads racing to lazily populate one still-hollow proxy
;;; they all happen to share. See git-transaction.lisp's own
;;; CONCURRENCY POLICY comment for the bigger picture.

(defparameter +load-stripe-mutex-count+ 32
  "The number of mutexes in +LOAD-STRIPE-MUTEXES+ -- a small, fixed
pool, rather than one mutex per GIT-OBJECT instance, so that
acquiring a load lock never itself allocates or grows any shared
table under contention. Any two distinct objects whose own SXHASH
happens to fall in the same stripe merely serialize against each
other during their own (rare, one-time) first concurrent load; this
never affects correctness, only how widely that already-rare event's
own contention is shared.")

(defparameter +load-stripe-mutexes+
  (coerce (loop repeat +load-stripe-mutex-count+
                collect (sb-thread:make-mutex :name "githack-object-load-stripe"))
          'simple-vector)
  "A small, fixed pool of SB-THREAD:MUTEX objects, indexed via
%LOAD-STRIPE-MUTEX, used by WITH-OBJECT-LOAD-LOCK to serialize only
the rare event of two threads racing to perform a *retyping* lazy
load (CL:CHANGE-CLASS, as %ENSURE-PERSISTENT-CONS-LOADED and
%ENSURE-PERSISTENT-WTTREE-NODE-LOADED both perform) of the exact same
GIT-OBJECT instance at once -- CHANGE-CLASS is not documented safe to
invoke concurrently on one instance from two threads, unlike this
file's other, plain-data lazy loads (which merely redo idempotent,
side-effect-free Git fetch/decode work if raced, and so need no
mutex at all -- see %PUBLISH-LOADED!).")

(defun %load-stripe-mutex (object)
  "Return one of +LOAD-STRIPE-MUTEXES+'s own fixed pool of mutexes,
chosen deterministically from OBJECT's own SXHASH (guaranteed stable
for OBJECT's whole lifetime, per the CL spec, even under a moving
GC), for WITH-OBJECT-LOAD-LOCK to acquire while serializing a
possible CL:CHANGE-CLASS lazy load of OBJECT."
  (svref +load-stripe-mutexes+ (mod (sxhash object) +load-stripe-mutex-count+)))

(defmacro with-object-load-lock ((object) &body body)
  "Evaluate BODY with a lock held on one of +LOAD-STRIPE-MUTEXES+'s
own fixed pool of mutexes (chosen deterministically from OBJECT's
own SXHASH, via %LOAD-STRIPE-MUTEX), serializing BODY against any
other thread concurrently trying to lazily load (in particular,
CL:CHANGE-CLASS) OBJECT itself, or any other object that happens to
hash to the same stripe. Callers should still check GET-LOADED? (or
their own analogous already-loaded condition) *before* entering this
macro at all, so the common, already-loaded case never even attempts
to acquire a lock -- see, e.g., %ENSURE-PERSISTENT-CONS-LOADED's own
outer (UNLESS (AND (TYPEP CONS 'PERSISTENT-CONS) (GET-LOADED? CONS))
...) guard."
  `(sb-thread:with-mutex ((%load-stripe-mutex ,object))
     ,@body))

(defun %publish-loaded! (object)
  "Atomically transition OBJECT's (a GIT-OBJECT, or any other CLOS
instance with a LOADED? slot, e.g. a PERSISTENT-OBJECT) own LOADED?
slot from NIL to T, via SB-EXT:COMPARE-AND-SWAP. Every %ENSURE-*-
LOADED lazy-load function in this codebase calls this only as its
own very last step, after every other slot it populates (ENTRIES,
PAYLOAD, TREE/PARENTS/AUTHOR/COMMITTER/..., etc.) has already been
SETF: SBCL implements COMPARE-AND-SWAP as a full memory barrier, so
any other thread that subsequently observes GET-LOADED? true for
this same OBJECT -- including one racing to lazily load it at the
very same time, and so redundantly redoing the identical, side-
effect-free Git fetch/decode work itself -- is guaranteed to see
every one of those other slots' own values too, never a half-
populated OBJECT. Harmless (simply a no-op failed CAS) if another
thread already won this same race first. Always returns OBJECT."
  (sb-ext:compare-and-swap (slot-value object 'loaded?) nil t)
  object)

(defmacro %cas-install-once (place old new)
  "Attempt to atomically install NEW into PLACE (any SETF-able,
SB-EXT:COMPARE-AND-SWAP-capable place, e.g. a SLOT-VALUE or SVREF
form) via SB-EXT:COMPARE-AND-SWAP, expecting to find OLD (compared
via EQL) there beforehand, and return whichever value is now
actually stored at PLACE: NEW itself, if this call won the race, or
some other thread's own already-installed value, if it lost. Used
for this codebase's other lock-free, install-only-once cache slots
(PERSISTENT-VECTOR-REF's own per-index cache, PERSISTENT-ARRAY's own
lazily-allocated DIMENSIONS/DATA), where -- unlike WITH-OBJECT-LOAD-
LOCK's own CL:CHANGE-CLASS concern -- every racing thread computes an
equally valid, purely functional NEW value, so losing this race costs
only a little redundant work, never correctness."
  (let ((old-var (gensym "OLD")) (new-var (gensym "NEW")) (previous (gensym "PREVIOUS")))
    `(let* ((,old-var ,old) (,new-var ,new)
            (,previous (sb-ext:compare-and-swap ,place ,old-var ,new-var)))
       (if (eql ,previous ,old-var) ,new-var ,previous))))
