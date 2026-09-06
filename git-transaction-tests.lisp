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

(test call-with-git-transaction-signals-error-for-non-git-repository
  "CALL-WITH-GIT-TRANSACTION rejects a REPOSITORY that is not a
GIT-REPOSITORY instance."
  (signals invalid-argument-error
    (call-with-git-transaction :not-a-git-repository :read-only
                                :receiver (lambda (tx head) (declare (ignore tx head))))))

(test call-with-git-transaction-signals-error-for-invalid-mode
  "CALL-WITH-GIT-TRANSACTION rejects a MODE other than :READ-ONLY or
:READ-WRITE."
  (let ((repository (%make-test-repository :read-only)))
    (signals invalid-argument-error
      (call-with-git-transaction repository :bogus-mode
                                  :receiver (lambda (tx head) (declare (ignore tx head)))))))

(test call-with-git-transaction-signals-error-for-non-callable-receiver
  "CALL-WITH-GIT-TRANSACTION rejects a RECEIVER that is not a
callable function or fbound symbol."
  (let ((repository (%make-test-repository :read-only)))
    (signals invalid-argument-error
      (call-with-git-transaction repository :read-only :receiver :not-a-function))))

(test call-with-git-transaction-signals-error-for-empty-branch-name
  "CALL-WITH-GIT-TRANSACTION rejects an effective BRANCH name (after
cascading from the repository's own default) that is not a
non-empty string."
  (let ((repository (call-with-repository +repo-path+
                                           :branch ""
                                           :author "The Boss <boss@githack.local>"
                                           :message "default message"
                                           :mode :read-only
                                           :receiver #'identity)))
    (signals invalid-argument-error
      (call-with-git-transaction repository :read-only
                                  :receiver (lambda (tx head) (declare (ignore tx head)))))))

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
              (destructuring-bind (called-repository name sha old-sha) (first update-calls)
                (is (equal +repo-path+ called-repository))
                (is (string= "main" name))
                (is (string= (sha commit) sha))
                (is (string= +head-sha+ old-sha))))))))))

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

(test with-git-transaction-expands-into-call-with-git-transaction
  "WITH-GIT-TRANSACTION binds its TRANSACTION-VAR and HEAD-COMMIT-VAR
to the same values an explicit RECEIVER function passed to
CALL-WITH-GIT-TRANSACTION would receive, cascades BRANCH/AUTHOR/
COMMITTER/MESSAGE/PARENTS through unchanged, and honors the same
auto-commit-on-normal-exit semantics."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let* ((unsaved-blob (make-instance 'git-blob :repository +repo-path+ :payload "hello"))
                 (tree (make-instance 'git-tree :repository +repo-path+
                                                 :entries (list (cons "f.txt" unsaved-blob))))
                 (seen-head nil)
                 (transaction
                   (with-git-transaction (tx head) (repository :read-write)
                     (setf seen-head head)
                     (is (typep tx 'git-transaction))
                     tree)))
            (is (typep seen-head 'git-commit))
            (is (string= +head-sha+ (sha seen-head)))
            (is (eq :committed (get-status transaction)))
            (let ((commit (get-result transaction)))
              (is (typep commit 'git-commit))
              (is (eq tree (get-tree commit)))
              (is (= 1 (length update-calls))))))))))

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

(test star-git-transaction-is-unbound-outside-call-with-git-transaction
  "*GIT-TRANSACTION* is unbound at the top level, outside the
dynamic extent of any CALL-WITH-GIT-TRANSACTION call."
  (is (not (boundp '*git-transaction*))))

(test call-with-git-transaction-binds-star-git-transaction-to-the-same-instance
  "CALL-WITH-GIT-TRANSACTION dynamically binds *GIT-TRANSACTION* to
the exact GIT-TRANSACTION passed to RECEIVER, for the duration of
the call, and *GIT-TRANSACTION* reverts to unbound once the call
returns."
  (let ((repository (%make-test-repository :read-write)))
    (with-fake-head-resolution ()
      (call-with-git-transaction repository :read-only
                                  :receiver (lambda (tx head)
                                              (declare (ignore head))
                                              (is (eq tx *git-transaction*))
                                              (abort-git-transaction tx)))))
  (is (not (boundp '*git-transaction*))))

(test call-with-git-transaction-defaults-conflict-resolution-to-error
  "CALL-WITH-GIT-TRANSACTION's TRANSACTION defaults
CONFLICT-RESOLUTION to :ERROR when not explicitly supplied."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-git-transaction repository :read-write
                                              :receiver (lambda (tx head)
                                                          (declare (ignore tx head))
                                                          (make-instance 'git-tree :repository +repo-path+ :entries '())))))
            (is (eq :error (get-conflict-resolution transaction)))))))))

(test call-with-git-transaction-error-mode-propagates-concurrent-modification-error
  "With the default :CONFLICT-RESOLUTION :ERROR, a compare-and-swap
failure on the branch update (some other writer already having
advanced it) propagates a CONCURRENT-MODIFICATION-ERROR straight out
of CALL-WITH-GIT-TRANSACTION, without any retry."
  (let ((repository (%make-test-repository :read-write))
        (attempts 0))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-cas-failing-git-update-ref ()
          (signals concurrent-modification-error
            (call-with-git-transaction repository :read-write
                                        :receiver (lambda (tx head)
                                                    (declare (ignore tx head))
                                                    (incf attempts)
                                                    (make-instance 'git-tree :repository +repo-path+ :entries '()))))
          (is (= 1 attempts)))))))

(test call-with-git-transaction-retry-mode-retries-until-successful
  "With :CONFLICT-RESOLUTION :RETRY, a compare-and-swap failure is
caught and the entire transaction re-attempted from scratch --
re-invoking RECEIVER -- until GIT-UPDATE-REF's own compare-and-swap
check finally succeeds."
  (let ((repository (%make-test-repository :read-write))
        (attempts 0))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-cas-failing-git-update-ref (:fail-count 2)
          (let ((transaction
                  (call-with-git-transaction repository :read-write
                                              :conflict-resolution :retry
                                              :receiver (lambda (tx head)
                                                          (declare (ignore tx head))
                                                          (incf attempts)
                                                          (make-instance 'git-tree :repository +repo-path+ :entries '())))))
            (is (eq :committed (get-status transaction)))
            (is (eq :retry (get-conflict-resolution transaction)))
            (is (= 3 attempts))))))))

(test call-with-git-transaction-rebase-mode-commits-normally-with-no-conflict
  "With :CONFLICT-RESOLUTION :REBASE, and no concurrent writer at all
(the common case), CALL-WITH-GIT-TRANSACTION commits exactly as
:ERROR would -- the rebase machinery is never even invoked -- and
GET-REBASE-FALLBACK defaults to :ERROR when not supplied."
  (let ((repository (%make-test-repository :read-write))
        (attempts 0)
        (update-calls '()))
    (with-fake-head-resolution ()
      (with-fake-git-hash-object ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-git-transaction repository :read-write
                                              :conflict-resolution :rebase
                                              :receiver (lambda (tx head)
                                                          (declare (ignore tx head))
                                                          (incf attempts)
                                                          (make-instance 'git-tree :repository +repo-path+ :entries '())))))
            (is (eq :committed (get-status transaction)))
            (is (eq :rebase (get-conflict-resolution transaction)))
            (is (eq :error (get-rebase-fallback transaction)))
            (is (= 1 attempts))))))))

(test call-with-git-transaction-lock-mode-holds-the-repository-lock-across-receiver
  "With :CONFLICT-RESOLUTION :LOCK, CALL-WITH-GIT-TRANSACTION holds
the repository's own transaction lock file for the entire dynamic
extent of RECEIVER's call, and releases it again once the
transaction has committed."
  (with-temporary-git-repository (git-dir)
    (let ((repository (make-instance 'git-repository
                                      :pathname git-dir
                                      :branch "main"
                                      :author "The Boss <boss@githack.local>"
                                      :committer "The Boss <boss@githack.local>"
                                      :message "default message"
                                      :mode :read-write))
          lock-held-during-receiver)
      (let ((transaction
              (call-with-git-transaction repository :read-write
                                          :conflict-resolution :lock
                                          :receiver (lambda (tx head)
                                                      (declare (ignore tx head))
                                                      (setf lock-held-during-receiver
                                                            (and (probe-file (%transaction-lock-pathname git-dir)) t))
                                                      (make-instance 'git-tree :repository git-dir :entries '())))))
        (is (eq :committed (get-status transaction)))
        (is (eq t lock-held-during-receiver))
        (is (not (probe-file (%transaction-lock-pathname git-dir))))))))

(test call-with-git-transaction-creates-an-orphan-root-commit-for-a-brand-new-branch
  "The very first CALL-WITH-GIT-TRANSACTION against a branch name
that does not exist yet produces a genuine orphan root commit: its
real, raw Git object text (fetched via `git cat-file -p`, bypassing
GitHack's own read path entirely) has no \"parent\" header line at
all, and the branch's ref did not exist before this transaction
created it -- exercising the same HEAD-COMMIT-is-NIL /
EXPECTED-BRANCH-SHA-is-NIL path GITHACK-EXAMPLE-LIBRARY's
ENSURE-LIBRARY-INITIALIZED relies on for the \"database-example\"
branch."
  (with-temporary-git-repository (git-dir)
    (is (null (git-show-ref-sha git-dir "orphan-example")))
    (let ((repository (make-instance 'git-repository
                                      :pathname git-dir
                                      :branch "orphan-example"
                                      :author "The Boss <boss@githack.local>"
                                      :committer "The Boss <boss@githack.local>"
                                      :message "first commit"
                                      :mode :read-write)))
      (let ((transaction
              (call-with-git-transaction repository :read-write
                                          :receiver (lambda (tx head)
                                                      (declare (ignore tx head))
                                                      (make-instance 'git-tree :repository git-dir :entries '())))))
        (is (eq :committed (get-status transaction)))
        (let* ((commit-sha (git-show-ref-sha git-dir "orphan-example"))
               (raw-commit-text
                 (uiop:run-program (list "git" (format nil "--git-dir=~A" (uiop:native-namestring git-dir))
                                          "cat-file" "-p" commit-sha)
                                    :output :string)))
          (is (not (null commit-sha)))
          (is (not (search "parent " raw-commit-text))))))))

(test call-with-git-transaction-nested-merges-its-root-into-the-parent-without-its-own-commit
  "A nested CALL-WITH-GIT-TRANSACTION -- opened from within an
already-active enclosing transaction's RECEIVER, against the same
REPOSITORY -- records the enclosing transaction as its own
GET-PARENT-TRANSACTION, never creates its own GIT-COMMIT (its own
GET-RESULT stays NIL) or advances any branch, and on a normal exit
copies its own final root up into the enclosing transaction's
GET-CURRENT-ROOT, which alone is committed -- exactly once -- when
the outermost transaction itself exits."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '())
        captured-outer-transaction
        captured-nested-transaction)
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let* ((nested-tree (make-instance 'git-tree :repository +repo-path+ :entries '()))
                 (outer-transaction
                   (call-with-git-transaction
                    repository :read-write
                    :receiver (lambda (outer-tx outer-head)
                                (declare (ignore outer-head))
                                (setf captured-outer-transaction outer-tx)
                                (setf captured-nested-transaction
                                      (call-with-git-transaction
                                       repository :read-write
                                       :receiver (lambda (inner-tx inner-head)
                                                   (declare (ignore inner-head))
                                                   (is (eq outer-tx (get-parent-transaction inner-tx)))
                                                   nested-tree)))
                                (get-current-root outer-tx)))))
            (is (eq :committed (get-status outer-transaction)))
            (is (eq :committed (get-status captured-nested-transaction)))
            (is (null (get-parent-transaction captured-outer-transaction)))
            (is (eq captured-outer-transaction (get-parent-transaction captured-nested-transaction)))
            (is (null (get-result captured-nested-transaction)))
            (is (eq nested-tree (get-current-root captured-nested-transaction)))
            (is (eq nested-tree (get-tree (get-result outer-transaction))))
            (is (= 1 (length update-calls)))))))))

(test call-with-git-transaction-nested-abort-leaves-the-parents-current-root-untouched
  "If a nested transaction is explicitly aborted (or exits via an
error), the enclosing transaction's GET-CURRENT-ROOT is left
completely untouched -- none of the nested transaction's own writes
percolate up -- and the enclosing transaction's own eventual commit
reflects only its pre-nesting state."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let* ((outer-tree (make-instance 'git-tree :repository +repo-path+ :entries '()))
                 (discarded-tree (make-instance 'git-tree :repository +repo-path+
                                                           :entries (list (cons "discard-me.txt"
                                                                                 (make-instance 'git-blob
                                                                                                 :repository +repo-path+
                                                                                                 :payload "gone")))))
                 (outer-transaction
                   (call-with-git-transaction
                    repository :read-write
                    :receiver (lambda (outer-tx outer-head)
                                (declare (ignore outer-head))
                                (call-with-git-transaction
                                 repository :read-write
                                 :receiver (lambda (inner-tx inner-head)
                                             (declare (ignore inner-head))
                                             (abort-git-transaction inner-tx)
                                             discarded-tree))
                                (is (null (get-current-root outer-tx)))
                                outer-tree))))
            (is (eq :committed (get-status outer-transaction)))
            (is (eq outer-tree (get-tree (get-result outer-transaction))))
            (is (= 1 (length update-calls)))))))))

(test call-with-git-transaction-signals-error-for-nested-read-write-inside-read-only-parent
  "A nested :READ-WRITE transaction cannot be opened from within an
enclosing :READ-ONLY transaction's RECEIVER."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (call-with-git-transaction
           repository :read-only
           :receiver (lambda (outer-tx outer-head)
                       (declare (ignore outer-head))
                       (signals transaction-state-error
                         (call-with-git-transaction
                          repository :read-write
                          :receiver (lambda (inner-tx inner-head)
                                      (declare (ignore inner-tx inner-head))
                                      (error "RECEIVER should never run."))))
                       (abort-git-transaction outer-tx))))))))

(test call-with-git-transaction-signals-error-for-nested-transaction-against-a-different-repository
  "A nested transaction's REPOSITORY must match its enclosing
transaction's own; otherwise CALL-WITH-GIT-TRANSACTION signals
INVALID-ARGUMENT-ERROR before ever invoking RECEIVER."
  (let ((repository (%make-test-repository :read-write))
        (other-repository (call-with-repository "/fake/other-repo/"
                                                  :branch "main"
                                                  :author "The Boss <boss@githack.local>"
                                                  :message "default message"
                                                  :mode :read-write
                                                  :receiver #'identity))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (call-with-git-transaction
           repository :read-write
           :receiver (lambda (outer-tx outer-head)
                       (declare (ignore outer-head))
                       (signals invalid-argument-error
                         (call-with-git-transaction
                          other-repository :read-write
                          :receiver (lambda (inner-tx inner-head)
                                      (declare (ignore inner-tx inner-head))
                                      (error "RECEIVER should never run."))))
                       (abort-git-transaction outer-tx))))))))

(test call-with-git-transaction-binds-star-git-transaction-to-the-nested-instance
  "While a nested transaction's RECEIVER is running, *GIT-TRANSACTION*
is bound to the nested GIT-TRANSACTION, not the enclosing one; once
the nested call returns, *GIT-TRANSACTION* reverts to the enclosing
transaction for the remainder of its own RECEIVER."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (call-with-git-transaction
           repository :read-write
           :receiver (lambda (outer-tx outer-head)
                       (declare (ignore outer-head))
                       (is (eq outer-tx *git-transaction*))
                       (call-with-git-transaction
                        repository :read-write
                        :receiver (lambda (inner-tx inner-head)
                                    (declare (ignore inner-head))
                                    (is (eq inner-tx *git-transaction*))
                                    (is (not (eq outer-tx inner-tx)))
                                    (make-instance 'git-tree :repository +repo-path+ :entries '())))
                       (is (eq outer-tx *git-transaction*))
                       (abort-git-transaction outer-tx)))))))) 
