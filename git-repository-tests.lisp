;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-repository-suite
  :in githack-suite
  :description "Tests for the GIT-REPOSITORY context object and CALL-WITH-REPOSITORY.")

(in-suite git-repository-suite)

(defparameter +repo-path+ #p"/fake/repo/"
  "A syntactically valid, arbitrary pathname standing in for a Git directory in tests -- never actually touched, since these tests never shell out to Git.")

(test call-with-repository-invokes-receiver-with-a-git-repository
  "CALL-WITH-REPOSITORY builds a GIT-REPOSITORY holding PATHNAME and
the supplied defaults, and passes it to RECEIVER."
  (call-with-repository +repo-path+
                         :branch "main" :author "The Boss <boss@githack.local>"
                         :message "default message" :mode :read-write
                         :receiver
                         (lambda (repository)
                           (is (typep repository 'git-repository))
                           (is (equal +repo-path+ (get-pathname repository)))
                           (is (string= "main" (get-branch repository)))
                           (is (string= "The Boss <boss@githack.local>" (get-author repository)))
                           (is (string= "default message" (get-message repository)))
                           (is (eq :read-write (get-mode repository))))))

(test call-with-repository-defaults-committer-to-author
  "COMMITTER defaults to AUTHOR when not explicitly supplied."
  (call-with-repository +repo-path+
                         :author "The Boss <boss@githack.local>"
                         :receiver
                         (lambda (repository)
                           (is (string= "The Boss <boss@githack.local>" (get-committer repository))))))

(test call-with-repository-respects-explicit-committer
  "An explicitly supplied COMMITTER is not overridden by AUTHOR."
  (call-with-repository +repo-path+
                         :author "The Boss <boss@githack.local>"
                         :committer "The Intern <intern@githack.local>"
                         :receiver
                         (lambda (repository)
                           (is (string= "The Intern <intern@githack.local>" (get-committer repository))))))

(test call-with-repository-defaults-mode-to-read-only
  "MODE defaults to :READ-ONLY when not explicitly supplied."
  (call-with-repository +repo-path+
                         :author "The Boss <boss@githack.local>"
                         :receiver (lambda (repository) (is (eq :read-only (get-mode repository))))))

(test call-with-repository-returns-receivers-value
  "CALL-WITH-REPOSITORY returns whatever RECEIVER returns."
  (is (eq :the-receivers-value
          (call-with-repository +repo-path+
                                 :author "The Boss <boss@githack.local>"
                                 :receiver (lambda (repository) (declare (ignore repository)) :the-receivers-value)))))

(test star-repository-is-unbound-outside-call-with-repository
  "*REPOSITORY* is unbound at the top level, outside the dynamic
extent of any CALL-WITH-REPOSITORY call."
  (is (not (boundp '*repository*))))

(test call-with-repository-binds-star-repository-to-the-same-instance
  "CALL-WITH-REPOSITORY dynamically binds *REPOSITORY* to the exact
GIT-REPOSITORY instance passed to RECEIVER, for the duration of the
call, and *REPOSITORY* reverts to unbound once the call returns."
  (call-with-repository +repo-path+
                         :author "The Boss <boss@githack.local>"
                         :receiver (lambda (repository) (is (eq repository *repository*))))
  (is (not (boundp '*repository*))))

(test with-repository-expands-into-call-with-repository
  "WITH-REPOSITORY binds REPOSITORY-VAR to the same GIT-REPOSITORY
CALL-WITH-REPOSITORY's RECEIVER would receive, passing
BRANCH/AUTHOR/COMMITTER/MESSAGE/MODE through unchanged, and returns
whatever BODY returns."
  (is (eq :the-bodys-value
          (with-repository (repository)
              (+repo-path+ :branch "main" :author "The Boss <boss@githack.local>"
                            :message "default message" :mode :read-write)
            (is (typep repository 'git-repository))
            (is (equal +repo-path+ (get-pathname repository)))
            (is (string= "main" (get-branch repository)))
            (is (string= "The Boss <boss@githack.local>" (get-author repository)))
            (is (string= "The Boss <boss@githack.local>" (get-committer repository)))
            (is (string= "default message" (get-message repository)))
            (is (eq :read-write (get-mode repository)))
            (is (eq repository *repository*))
            :the-bodys-value))))

(test with-repository-defaults-mode-to-read-only
  "WITH-REPOSITORY defaults MODE to :READ-ONLY when not explicitly
supplied, exactly as CALL-WITH-REPOSITORY's own default does."
  (with-repository (repository) (+repo-path+ :author "The Boss <boss@githack.local>")
    (is (eq :read-only (get-mode repository)))))
