;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; *REPOSITORY* is the GIT-REPOSITORY context object currently in
;;; dynamic scope, bound by CALL-WITH-REPOSITORY around its call to
;;; RECEIVER. It is unbound at the top level -- referencing it
;;; outside the dynamic extent of a CALL-WITH-REPOSITORY call signals
;;; UNBOUND-VARIABLE.
(defvar *repository*)

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
RECEIVER returns."
  (declare (ignore defaults))
  (let ((repository (make-instance 'git-repository
                                    :pathname repository-specifier
                                    :branch branch
                                    :author author
                                    :committer (or committer author)
                                    :message message
                                    :mode mode)))
    (let ((*repository* repository))
      (funcall receiver repository))))
