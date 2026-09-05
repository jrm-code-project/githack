;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; *REPOSITORY* is the GIT-REPOSITORY context object currently in
;;; dynamic scope, bound by CALL-WITH-REPOSITORY around its call to
;;; RECEIVER. It is unbound at the top level -- referencing it
;;; outside the dynamic extent of a CALL-WITH-REPOSITORY call signals
;;; UNBOUND-VARIABLE.
(defvar *repository*)

;; DEFVAR's own DOCUMENTATION argument cannot be supplied without
;; also supplying an INITIAL-VALUE (which would leave *REPOSITORY*
;; bound instead of unbound by default), so its docstring is attached
;; separately here via (SETF DOCUMENTATION).
(setf (documentation '*repository* 'variable)
      "The GIT-REPOSITORY context object currently in dynamic scope,
dynamically bound by CALL-WITH-REPOSITORY to the GIT-REPOSITORY it
constructs, for the duration of its call to RECEIVER. Unbound at the
top level -- referencing it outside the dynamic extent of a
CALL-WITH-REPOSITORY call signals UNBOUND-VARIABLE.")

;;; GIT-REPOSITORY is a lightweight context object: it bundles a Git
;;; repository's on-disk location (its --git-dir PATHNAME) together
;;; with the cascading defaults (BRANCH, AUTHOR, COMMITTER, MESSAGE,
;;; MODE) that new GIT-TRANSACTIONs inherit unless they explicitly
;;; override them. See CALL-WITH-REPOSITORY.
;;;
;;; NOTE: the context class itself is named GIT-REPOSITORY, not
;;; REPOSITORY, because REPOSITORY already names an unrelated class
;;; (see repository.lisp) belonging to the older versioned-object /
;;; mapper subsystem. CALL-WITH-REPOSITORY -- the entry point below
;;; -- has no such collision and keeps its natural name.

(defclass git-repository ()
  ((pathname
    :initarg :pathname
    :reader get-pathname
    :documentation
    "The pathname of this repository's Git directory on disk (its
--git-dir), passed through to the low-level GIT-OBJECT/GIT-BRANCH
proxies as their opaque REPOSITORY handle.")
   (branch
    :initarg :branch
    :reader get-branch
    :documentation
    "The default branch name (e.g. \"main\") new GIT-TRANSACTIONs
operate against unless they supply their own.")
   (author
    :initarg :author
    :reader get-author
    :documentation "The default commit author signature.")
   (committer
    :initarg :committer
    :reader get-committer
    :documentation
    "The default commit committer signature. Defaults to AUTHOR
when not explicitly supplied to CALL-WITH-GIT-REPOSITORY.")
   (message
    :initarg :message
    :reader get-message
    :documentation "The default commit message.")
   (mode
    :initarg :mode
    :initform :read-only
    :reader get-mode
    :type (member :read-only :read-write)
    :documentation
    "Either :READ-ONLY or :READ-WRITE. A GIT-TRANSACTION opened in
:READ-WRITE mode against a :READ-ONLY repository is rejected by
CALL-WITH-GIT-TRANSACTION."))
  (:documentation
   "A context object bundling a Git repository's on-disk location
together with the cascading defaults new GIT-TRANSACTIONs inherit.
See CALL-WITH-GIT-REPOSITORY."))

(defun call-with-repository (repository-specifier &rest defaults
                              &key receiver branch author committer message (mode :read-only))
  "Instantiate a GIT-REPOSITORY naming the Git directory
REPOSITORY-SPECIFIER, holding BRANCH, AUTHOR, COMMITTER, MESSAGE,
and MODE as the cascading defaults later GIT-TRANSACTIONs inherit,
then invoke RECEIVER with that GIT-REPOSITORY, with *REPOSITORY*
dynamically bound to it for the duration of the call. COMMITTER
defaults to AUTHOR when not explicitly supplied. Returns whatever
RECEIVER returns. Signals INVALID-ARGUMENT-ERROR if
REPOSITORY-SPECIFIER is NIL, if MODE is not :READ-ONLY or
:READ-WRITE, or if RECEIVER is not a callable function."
  (declare (ignore defaults))
  (unless repository-specifier
    (error 'invalid-argument-error
           :format-control "REPOSITORY-SPECIFIER must not be NIL."))
  (unless (member mode '(:read-only :read-write))
    (error 'invalid-argument-error
           :format-control "MODE must be :READ-ONLY or :READ-WRITE, not ~S."
           :format-arguments (list mode)))
  (unless (or (functionp receiver) (and (symbolp receiver) receiver (fboundp receiver)))
    (error 'invalid-argument-error
           :format-control "RECEIVER must be a callable function, not ~S."
           :format-arguments (list receiver)))
  (let ((repository (make-instance 'git-repository
                                    :pathname repository-specifier
                                    :branch branch
                                    :author author
                                    :committer (or committer author)
                                    :message message
                                    :mode mode)))
    (let ((*repository* repository))
      (funcall receiver repository))))

(defmacro with-repository ((repository-var) (repository-specifier &key branch author committer message (mode :read-only)) &body body)
  "Macro wrapper around CALL-WITH-REPOSITORY: expands into a call to
CALL-WITH-REPOSITORY on REPOSITORY-SPECIFIER (evaluated once),
passing BRANCH/AUTHOR/COMMITTER/MESSAGE/MODE through unchanged (MODE
defaulting to :READ-ONLY exactly as CALL-WITH-REPOSITORY's own does),
with :RECEIVER bound to a closure over BODY. Within BODY,
REPOSITORY-VAR is bound to the GIT-REPOSITORY CALL-WITH-REPOSITORY
constructs -- exactly as it would be passed to an explicit RECEIVER
function, and exactly the same instance *REPOSITORY* is dynamically
bound to for the duration of the call. Returns whatever BODY
returns."
  `(call-with-repository ,repository-specifier
                          :branch ,branch
                          :author ,author
                          :committer ,committer
                          :message ,message
                          :mode ,mode
                          :receiver (lambda (,repository-var) ,@body)))
