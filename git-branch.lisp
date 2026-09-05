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

(define-condition concurrent-modification-error (githack-error)
  ((repository :initarg :repository :reader get-repository)
   (name :initarg :name :reader get-name)
   (expected-sha :initarg :expected-sha :reader get-expected-sha)
   (new-sha :initarg :new-sha :reader get-new-sha)
   (detail :initarg :detail :initform nil :reader get-detail))
  (:report
   (lambda (condition stream)
     (format stream "Concurrent modification detected: branch ~S in ~A was expected to be at ~A, but another writer already advanced it (attempted to update it to ~A).~@[~%~A~]"
             (get-name condition)
             (get-repository condition)
             (or (get-expected-sha condition) "no commit yet")
             (get-new-sha condition)
             (get-detail condition))))
  (:documentation
   "Signaled by GIT-UPDATE-REF (and, transitively, UPDATE-BRANCH and
CALL-WITH-GIT-TRANSACTION's own commit path) when it is called with
an EXPECTED-SHA compare-and-swap check -- either a specific SHA, or
NIL meaning \"the ref must not exist yet\" -- and Git's own
`update-ref' refuses the update because some other writer already
advanced (or created) the ref in the meantime. This is GitHack's
'Lost Update' detection: a caller using CALL-WITH-GIT-TRANSACTION's
:CONFLICT-RESOLUTION :RETRY mode should catch this condition and
re-attempt its transaction from scratch against the ref's new
state; :ERROR mode (the default) instead lets it propagate."))

(defun git-update-ref (repository name sha &key (expected-sha :unconditional))
  "Shell out to `git update-ref refs/heads/<NAME> <SHA> [<EXPECTED-SHA>]`
against REPOSITORY (a pathname naming a Git directory), safely and
atomically creating or advancing that branch's ref to point at SHA.

EXPECTED-SHA controls Git's own compare-and-swap semantics:
* :UNCONDITIONAL (the default) performs an ordinary, unconditional
  update: the ref is created or advanced to SHA regardless of
  whatever it may currently point at.
* A 40-character hexadecimal SHA string requires the ref to
  currently point at exactly that commit; Git refuses the update
  (atomically, without any race window) otherwise.
* NIL requires the ref to not exist yet at all (the initial-commit
  case); Git refuses the update if it already exists.

In either compare-and-swap case (a string or NIL EXPECTED-SHA),
signals CONCURRENT-MODIFICATION-ERROR if Git's own check fails,
i.e. if some other writer already advanced (or, for NIL, created)
the ref out from under us. Returns SHA on success."
  (let ((args (append (list "git"
                             (format nil "--git-dir=~A" (uiop:native-namestring repository))
                             "update-ref"
                             (%branch-ref-name name)
                             sha)
                       (unless (eq expected-sha :unconditional)
                         (list (or expected-sha ""))))))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program args :output :string :error-output :string :ignore-error-status t)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'concurrent-modification-error
               :repository repository :name name
               :expected-sha (and (not (eq expected-sha :unconditional)) expected-sha)
               :new-sha sha
               :detail error-output))
      sha)))

(defun resolve-branch (repository name &key (if-does-not-exist :error))
  "Return a new GIT-BRANCH naming NAME in REPOSITORY, whose TARGET
slot holds a lazily-loaded GIT-COMMIT proxy (via INFLATE-GIT-PROXY)
for the SHA that branch currently points to, as reported by
GIT-SHOW-REF-SHA.

IF-DOES-NOT-EXIST controls what happens when no such branch exists
yet: :ERROR (the default) signals an error; any other value (e.g.
NIL) instead returns a GIT-BRANCH whose TARGET slot is NIL, letting
callers distinguish a not-yet-existing branch (an empty repository,
awaiting its initial commit) from one whose commit failed to load."
  (let ((sha (git-show-ref-sha repository name)))
    (unless (or sha (not (eq if-does-not-exist :error)))
      (error 'branch-not-found-error :repository repository :name name))
    (make-instance 'git-branch
                   :repository repository
                   :name name
                   :target (and sha (inflate-git-proxy repository sha)))))

(defun update-branch (branch &key (expected-sha :unconditional))
  "Force Git to advance BRANCH's ref (refs/heads/<name>) to the SHA
of the GIT-COMMIT currently held in its TARGET slot, via
GIT-UPDATE-REF. EXPECTED-SHA is passed through unchanged to
GIT-UPDATE-REF's own compare-and-swap argument of the same name:
:UNCONDITIONAL (the default) for an ordinary unconditional update, a
40-character SHA string to require the ref currently point at that
commit, or NIL to require the ref not yet exist. Signals an error if
TARGET has no SHA (not yet persisted), or
CONCURRENT-MODIFICATION-ERROR if EXPECTED-SHA's compare-and-swap
check fails against Git's own current state for the ref. Returns
BRANCH."
  (let ((sha (sha (get-target branch))))
    (unless sha
      (error 'unpersisted-object-error
             :format-control "Cannot update branch ~S: its TARGET commit has no SHA (not yet persisted)."
             :format-arguments (list (get-name branch))))
    (git-update-ref (get-repository branch) (get-name branch) sha :expected-sha expected-sha)
    branch))
