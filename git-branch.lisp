;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; GIT-BRANCH is a mutable pointer onto an immutable GIT-COMMIT: it
;;; mirrors a Git branch ref (a file under `.git/refs/heads/`, or
;;; more portably its packed-refs entry) that names the tip commit
;;; of a line of history. Unlike GIT-OBJECT proxies, a GIT-BRANCH has
;;; no SHA of its own -- only the SHA of the commit it currently
;;; targets -- so it does not subclass GIT-OBJECT.

(defclass git-branch ()
  ((repository
    :initarg :repository
    :reader get-repository
    :documentation "The repository this branch belongs to.")
   (name
    :initarg :name
    :reader get-name
    :type string
    :documentation "The branch's name, e.g. \"main\" or \"master\".")
   (target
    :initarg :target
    :accessor get-target
    :documentation
    "The GIT-COMMIT proxy this branch currently points to (possibly
still unloaded)."))
  (:documentation
   "A mutable reference onto a GIT-COMMIT, mirroring a Git branch ref
under `refs/heads/`. See RESOLVE-BRANCH and UPDATE-BRANCH."))

(defun %branch-ref-name (name)
  "Return the full Git ref path (\"refs/heads/<NAME>\") for the
branch named NAME."
  (format nil "refs/heads/~A" name))

(defun git-show-ref-sha (repository name)
  "Shell out to `git show-ref --verify --hash refs/heads/<NAME>`
against REPOSITORY (a pathname naming a Git directory) and return
the 40-character hexadecimal SHA that branch currently points to, or
NIL if no branch named NAME exists in REPOSITORY."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "git"
                               (format nil "--git-dir=~A" (uiop:native-namestring repository))
                               "show-ref" "--verify" "--hash"
                               (%branch-ref-name name))
                         :output :string
                         :ignore-error-status t)
    (declare (ignore error-output))
    (and (zerop exit-code)
         (string-trim '(#\Space #\Newline #\Return) output))))

(defun git-update-ref (repository name sha)
  "Shell out to `git update-ref refs/heads/<NAME> <SHA>` against
REPOSITORY (a pathname naming a Git directory), safely and
atomically creating or advancing that branch's ref to point at SHA.
Returns SHA."
  (uiop:run-program (list "git"
                          (format nil "--git-dir=~A" (uiop:native-namestring repository))
                          "update-ref"
                          (%branch-ref-name name)
                          sha)
                     :output :string)
  sha)

(defun resolve-branch (repository name)
  "Return a new GIT-BRANCH naming NAME in REPOSITORY, whose TARGET
slot holds a lazily-loaded GIT-COMMIT proxy (via INFLATE-GIT-PROXY)
for the SHA that branch currently points to, as reported by
GIT-SHOW-REF-SHA. Signals an error if no such branch exists."
  (let ((sha (git-show-ref-sha repository name)))
    (unless sha
      (error "No branch named ~S in repository ~A." name repository))
    (make-instance 'git-branch
                   :repository repository
                   :name name
                   :target (inflate-git-proxy repository sha))))

(defun update-branch (branch)
  "Force Git to advance BRANCH's ref (refs/heads/<name>) to the SHA
of the GIT-COMMIT currently held in its TARGET slot, via
GIT-UPDATE-REF. Signals an error if TARGET has no SHA (not yet
persisted). Returns BRANCH."
  (let ((sha (get-sha (get-target branch))))
    (unless sha
      (error "Cannot update branch ~S: its TARGET commit has no SHA (not yet persisted)."
             (get-name branch)))
    (git-update-ref (get-repository branch) (get-name branch) sha)
    branch))
