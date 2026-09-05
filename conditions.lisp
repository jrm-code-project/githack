;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; GITHACK-ERROR and its subtypes let callers programmatically
;;; distinguish categories of GitHack's own failures (as opposed to
;;; conditions signaled by underlying libraries, or by Common Lisp
;;; itself) without resorting to parsing error-message text. Loaded
;;; very early (depending on nothing but PACKAGE) so every other file
;;; may signal these conditions without introducing a load-order
;;; cycle.
;;;
;;; Most subtypes below carry no slots of their own and are simply
;;; signaled via (ERROR 'SUBTYPE :FORMAT-CONTROL "..." :FORMAT-ARGUMENTS
;;; (LIST ...)), inheriting SIMPLE-ERROR's own REPORT method -- so
;;; they read identically, from a caller's point of view, to the bare
;;; CL:ERROR calls they replace, except for now being a
;;; programmatically distinguishable condition class. A few
;;; (BRANCH-NOT-FOUND-ERROR, CONCURRENT-MODIFICATION-ERROR in
;;; git-branch.lisp, TRANSACTION-LOCK-TIMEOUT-ERROR in
;;; transaction-lock.lisp) carry their own structured slots and a
;;; custom REPORT method instead.

(define-condition githack-error (simple-error)
  ()
  (:documentation
   "Base condition for every error GitHack itself signals, as
opposed to conditions signaled by underlying libraries or by Common
Lisp itself. Subclasses let callers programmatically distinguish
categories of GitHack failure instead of pattern-matching error
message text. Like SIMPLE-ERROR (which this inherits from), accepts
:FORMAT-CONTROL/:FORMAT-ARGUMENTS initargs for building a
human-readable REPORT."))

(define-condition malformed-git-object-error (githack-error)
  ()
  (:documentation
   "Signaled when parsing/deserializing a Git object's raw bytes or
text -- a tree's packed binary entries, a commit's plain-text
headers, a blob's atom envelope, or the \".meta\" blob of a
persistent structure -- finds unexpected or invalid structure: a
corrupt object, or one that was never written by GitHack's own
serialization code in the first place."))

(define-condition unpersisted-object-error (githack-error)
  ()
  (:documentation
   "Signaled when an operation requires a GIT-OBJECT to already be
persisted (i.e. to already have a SHA), or a persistent structure's
own required internal state to already be complete (e.g. a
PERSISTENT-CONS's PERSISTENT-CAR, or a PERSISTENT-ARRAY's DIMENSIONS/
DATA), before it can be serialized -- but it is not."))

(define-condition transaction-state-error (githack-error)
  ()
  (:documentation
   "Signaled when an operation on a GIT-TRANSACTION -- or the
:READ-ONLY/:READ-WRITE mode of the GIT-REPOSITORY it was opened
against -- requires a state (e.g. :ACTIVE, :READ-WRITE) other than
its actual current one."))

(define-condition invalid-argument-error (githack-error)
  ()
  (:documentation
   "Signaled when a public entry point is called with an argument of
the wrong type, shape, or value -- an unsupported atom type, invalid
array dimensions, an out-of-range index or subscript, an
unnameable hash-table TEST function, and so on -- as opposed to any
problem with previously-persisted Git data (see
MALFORMED-GIT-OBJECT-ERROR) or an object's own persistence state
(see UNPERSISTED-OBJECT-ERROR)."))

(define-condition branch-not-found-error (githack-error)
  ((repository :initarg :repository :reader get-repository)
   (name :initarg :name :reader get-name))
  (:report
   (lambda (condition stream)
     (format stream "No branch named ~S in repository ~A."
             (get-name condition) (get-repository condition))))
  (:documentation
   "Signaled by RESOLVE-BRANCH when no branch named NAME exists in
REPOSITORY and :IF-DOES-NOT-EXIST is :ERROR (the default)."))
