;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-transaction-suite
  :in githack-suite
  :description "Tests for GIT-TRANSACTION, CALL-WITH-GIT-TRANSACTION, COMMIT-GIT-TRANSACTION, and ABORT-GIT-TRANSACTION.")

(in-suite git-transaction-suite)

(defparameter +head-sha+ "1111111111111111111111111111111111111111"
  "An arbitrary, syntactically valid fake commit SHA standing in for a branch's current head in tests.")

(defun %make-test-repository (mode)
  "Return a fresh GIT-REPOSITORY (via CALL-WITH-REPOSITORY) rooted
at +REPO-PATH+, defaulting to branch \"main\" and a fixed
author/message, opened in MODE."
  (call-with-repository +repo-path+
                         :branch "main"
                         :author "The Boss <boss@githack.local>"
                         :message "default message"
                         :mode mode
                         :receiver #'identity))

(defmacro with-fake-head-resolution (() &body body)
  "Within BODY, resolving branch \"main\" of +REPO-PATH+ returns a
GIT-BRANCH targeting the (unloaded) commit +HEAD-SHA+."
  `(with-fake-git-show-ref-sha ((list (cons (cons +repo-path+ "main") +head-sha+)))
     (with-fake-git-type ((list (cons +head-sha+ "commit")))
       ,@body)))

(test call-with-git-transaction-signals-error-for-read-write-on-read-only-repository
  "Opening a :READ-WRITE transaction against a :READ-ONLY repository signals an error."
  (let ((repository (%make-test-repository :read-only)))
    (signals error
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (error "RECEIVER should never run."))))))

(test call-with-git-transaction-cascades-defaults-and-resolves-head
  "BRANCH/AUTHOR/COMMITTER/MESSAGE not explicitly supplied are
inherited from the repository, and the branch's current head is
resolved and passed to RECEIVER."
  (let ((repository (%make-test-repository :read-write)))
    (with-fake-head-resolution ()
      (let (captured-transaction captured-head)
        (call-with-git-transaction repository :read-only
                                    :receiver (lambda (tx head)
                                                (setf captured-transaction tx
                                                      captured-head head)
                                                (abort-git-transaction tx)))
        (is (string= "The Boss <boss@githack.local>" (get-author captured-transaction)))
        (is (string= "The Boss <boss@githack.local>" (get-committer captured-transaction)))
        (is (string= "default message" (get-message captured-transaction)))
        (is (typep captured-head 'git-commit))
        (is (string= +head-sha+ (sha captured-head)))
        (is (equal (list captured-head) (get-parents captured-transaction)))))))

(test call-with-git-transaction-respects-explicit-overrides
  "Explicitly supplied BRANCH/AUTHOR/COMMITTER/MESSAGE/PARENTS override the repository's defaults."
  (let ((repository (%make-test-repository :read-write))
        (explicit-parents (list :a-fake-parent-commit)))
    (with-fake-git-show-ref-sha ((list (cons (cons +repo-path+ "other") +head-sha+)))
      (with-fake-git-type ((list (cons +head-sha+ "commit")))
        (let (captured-transaction)
          (call-with-git-transaction repository :read-only
                                      :branch "other"
                                      :author "Someone Else <else@githack.local>"
                                      :committer "Committer <c@githack.local>"
                                      :message "override message"
                                      :parents explicit-parents
                                      :receiver (lambda (tx head)
                                                  (declare (ignore head))
                                                  (setf captured-transaction tx)
                                                  (abort-git-transaction tx)))
          (is (string= "Someone Else <else@githack.local>" (get-author captured-transaction)))
          (is (string= "Committer <c@githack.local>" (get-committer captured-transaction)))
          (is (string= "override message" (get-message captured-transaction)))
          (is (equal explicit-parents (get-parents captured-transaction))))))))

(test abort-git-transaction-writes-nothing
  "ABORT-GIT-TRANSACTION discards the transaction: its status becomes
:ABORTED, RESULT stays NIL, and nothing is written to Git."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '())
        (ran-after-abort nil))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-git-transaction repository :read-write
                                              :receiver (lambda (tx head)
                                                          (declare (ignore head))
                                                          (abort-git-transaction tx)
                                                          (setf ran-after-abort t)
                                                          (make-instance 'git-tree :repository +repo-path+ :entries '())))))
            (is (eq :aborted (get-status transaction)))
            (is (null (get-result transaction)))
            (is (null ran-after-abort))
            (is (null update-calls))))))))

(test error-in-receiver-writes-nothing
  "An error signaled inside RECEIVER propagates out of
CALL-WITH-GIT-TRANSACTION and leaves nothing written."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (signals error
            (call-with-git-transaction repository :read-write
                                        :receiver (lambda (tx head)
                                                    (declare (ignore tx head))
                                                    (error "Deliberate test failure."))))
          (is (null update-calls)))))))

(test call-with-git-transaction-read-only-normal-exit-writes-nothing
  "A :READ-ONLY transaction's normal exit never persists or advances the branch."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-git-transaction repository :read-only
                                              :receiver (lambda (tx head)
                                                          (declare (ignore tx head))
                                                          (make-instance 'git-tree :repository +repo-path+ :entries '())))))
            (is (eq :committed (get-status transaction)))
            (is (null (get-result transaction)))
            (is (null update-calls))))))))

(test call-with-git-transaction-auto-commits-on-normal-read-write-exit
  "A :READ-WRITE transaction whose RECEIVER returns normally with a
GIT-TREE is automatically committed: unpersisted children are
persisted, a new GIT-COMMIT is created and persisted, and the branch
is advanced to point at it."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let* ((unsaved-blob (make-instance 'git-blob :repository +repo-path+ :payload "hello"))
                 (tree (make-instance 'git-tree :repository +repo-path+
                                                 :entries (list (cons "f.txt" unsaved-blob))))
                 (transaction
                   (call-with-git-transaction repository :read-write
                                               :receiver (lambda (tx head)
                                                           (declare (ignore tx head))
                                                           tree))))
            (is (eq :committed (get-status transaction)))
            (is (stringp (sha unsaved-blob)))
            (is (stringp (sha tree)))
            (let ((commit (get-result transaction)))
              (is (typep commit 'git-commit))
              (is (stringp (sha commit)))
              (is (eq tree (get-tree commit)))
              (is (string= "The Boss <boss@githack.local>" (get-author commit)))
              (is (string= "The Boss <boss@githack.local>" (get-committer commit)))
              (is (string= "default message" (get-message commit)))
              (is (= 1 (length (get-parents commit))))
              (is (string= +head-sha+ (sha (first (get-parents commit)))))
              (is (= 1 (length update-calls)))
              (destructuring-bind (called-repository name sha) (first update-calls)
                (is (equal +repo-path+ called-repository))
                (is (string= "main" name))
                (is (string= (sha commit) sha))))))))))

(test call-with-git-transaction-wraps-an-atomic-root-in-an-atomic-wrapper-tree
  "A :READ-WRITE transaction whose RECEIVER returns a bare atomic
GIT-BLOB (rather than any kind of GIT-TREE) is automatically wrapped
in an ATOMIC-WRAPPER-TREE before being committed, since Git itself
requires every commit to point at a tree; RESOLVE-COMMIT-ROOT then
transparently retrieves the original blob back out again, making the
wrapper invisible to application code."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let* ((unsaved-blob (make-instance 'git-blob :repository +repo-path+ :payload 42))
                 (transaction
                   (call-with-git-transaction repository :read-write
                                               :receiver (lambda (tx head)
                                                           (declare (ignore tx head))
                                                           unsaved-blob))))
            (is (eq :committed (get-status transaction)))
            (is (stringp (sha unsaved-blob)))
            (let* ((commit (get-result transaction))
                   (tree (get-tree commit)))
              (is (typep tree 'git-tree))
              (is (equal (list ".meta" "README.md" "value")
                         (mapcar #'car (get-entries tree))))
              (is (eq unsaved-blob (cdr (assoc "value" (get-entries tree) :test #'string=))))
              (is (eq unsaved-blob (resolve-commit-root commit))))))))))

(test commit-git-transaction-commits-immediately-and-skips-later-receiver-code
  "COMMIT-GIT-TRANSACTION, called explicitly inside RECEIVER, commits
immediately and unwinds so any subsequent code in RECEIVER never runs."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '())
        (ran-after-commit nil))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let* ((tree (make-instance 'git-tree :repository +repo-path+ :entries '()))
                 (transaction
                   (call-with-git-transaction repository :read-write
                                               :receiver (lambda (tx head)
                                                           (declare (ignore head))
                                                           (commit-git-transaction tx tree)
                                                           (setf ran-after-commit t)))))
            (is (eq :committed (get-status transaction)))
            (is (null ran-after-commit))
            (is (not (null (get-result transaction))))
            (is (= 1 (length update-calls)))))))))

(test commit-git-transaction-signals-error-for-read-only-transaction
  "COMMIT-GIT-TRANSACTION signals an error if TRANSACTION is not :READ-WRITE."
  (let ((repository (%make-test-repository :read-write)))
    (with-fake-head-resolution ()
      (signals error
        (call-with-git-transaction repository :read-only
                                    :receiver (lambda (tx head)
                                                (declare (ignore head))
                                                (commit-git-transaction
                                                 tx (make-instance 'git-tree :repository +repo-path+ :entries '()))))))))
