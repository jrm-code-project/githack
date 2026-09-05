;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite transaction-suite
  :in githack-suite
  :description "Tests for CALL-WITH-TRANSACTION and WITH-TRANSACTION.")

(in-suite transaction-suite)

(test call-with-transaction-auto-commits-an-initial-atom-for-an-empty-branch
  "CALL-WITH-TRANSACTION passes NIL to RECEIVER when BRANCH does not
exist yet (an empty repository awaiting its initial commit), and
automatically wraps RECEIVER's returned atom in an ATOMIC-WRAPPER-
TREE for the repository's very first commit, whose PARENTS list is
empty."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-transaction repository :read-write
                                          :receiver (lambda (value)
                                                      (is (null value))
                                                      42))))
            (is (eq :committed (get-status transaction)))
            (let* ((commit (get-result transaction))
                   (tree (get-tree commit)))
              (is (typep tree 'git-tree))
              (is (equal (list ".meta" "README.md" "value") (mapcar #'car (get-entries tree))))
              (is (= 42 (get-payload (cdr (assoc "value" (get-entries tree) :test #'string=)))))
              (is (null (get-parents commit)))
              (is (= 1 (length update-calls))))))))))

(test call-with-transaction-round-trips-an-existing-atomic-state
  "CALL-WITH-TRANSACTION rehydrates an existing atomic-wrapped commit
root back into the plain decoded Lisp atom RECEIVER expects
(transparently unwrapping the ATOMIC-WRAPPER-TREE and the GIT-BLOB's
own payload), and automatically re-wraps RECEIVER's new atom into a
fresh ATOMIC-WRAPPER-TREE for the next commit, with the old head
recorded as its sole parent."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-object-store ()
      (let* ((blob (make-instance 'git-blob :repository +repo-path+ :payload 10)))
        (setf (sha blob) (git-hash-object +repo-path+ "blob" (serialize-atom 10)))
        (let* ((wrapper (wrap-atomic-commit-root +repo-path+ blob))
               (wrapper-entries (get-entries wrapper))
               (meta-sha (sha (cdr (assoc ".meta" wrapper-entries :test #'string=))))
               (readme-sha (sha (cdr (assoc "README.md" wrapper-entries :test #'string=))))
               (old-commit (make-instance 'git-commit :repository +repo-path+
                                                       :tree wrapper :parents '()
                                                       :author "The Boss <boss@githack.local>"
                                                       :committer "The Boss <boss@githack.local>"
                                                       :timestamp 1000 :message "initial")))
          (setf (sha old-commit)
                (git-hash-object +repo-path+ "commit"
                                  (sb-ext:string-to-octets (serialize-commit old-commit) :external-format :utf-8)))
          (with-fake-git-show-ref-sha ((list (cons (cons +repo-path+ "main") (sha old-commit))))
            (with-fake-git-type ((list (cons (sha old-commit) "commit")
                                        (cons (sha wrapper) "tree")
                                        (cons meta-sha "blob")
                                        (cons readme-sha "blob")
                                        (cons (sha blob) "blob")))
              (with-recording-git-update-ref (update-calls)
                (let ((transaction
                        (call-with-transaction repository :read-write
                                                :receiver (lambda (value)
                                                            (is (= 10 value))
                                                            (1+ value)))))
                  (is (eq :committed (get-status transaction)))
                  (let* ((commit (get-result transaction))
                         (new-tree (get-tree commit)))
                    (is (typep new-tree 'git-tree))
                    (is (equal (list ".meta" "README.md" "value") (mapcar #'car (get-entries new-tree))))
                    (is (= 11 (get-payload (cdr (assoc "value" (get-entries new-tree) :test #'string=)))))
                    (is (= 1 (length (get-parents commit))))
                    (is (string= (sha old-commit) (sha (first (get-parents commit)))))
                    (is (= 1 (length update-calls)))))))))))))

(test call-with-transaction-passes-through-a-compound-tree-unchanged
  "When RECEIVER's returned value is already a GIT-OBJECT (e.g. a
GIT-TREE it built or mutated and returned directly), CALL-WITH-
TRANSACTION commits it as the root directly, without wrapping it in
any ATOMIC-WRAPPER-TREE."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let* ((tree (make-instance 'git-tree :repository +repo-path+
                                                  :entries (list (cons "f.txt"
                                                                        (make-instance 'git-blob
                                                                                        :repository +repo-path+
                                                                                        :payload "hi")))))
                 (transaction
                   (call-with-transaction repository :read-write
                                           :receiver (lambda (value)
                                                       (is (null value))
                                                       tree))))
            (is (eq :committed (get-status transaction)))
            (is (eq tree (get-tree (get-result transaction))))
            (is (= 1 (length update-calls)))))))))

(test with-transaction-expands-into-call-with-transaction
  "WITH-TRANSACTION binds VALUE-VAR to the same plain Lisp value
CALL-WITH-TRANSACTION's RECEIVER would receive, and honors the same
auto-commit semantics for BODY's return value."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '()))
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (with-transaction (value) (repository :read-write)
                    (is (null value))
                    99)))
            (is (eq :committed (get-status transaction)))
            (is (= 99 (get-payload
                       (cdr (assoc "value" (get-entries (get-tree (get-result transaction)))
                                   :test #'string=)))))))))))

(test star-transaction-is-unbound-outside-call-with-transaction
  "*TRANSACTION* is unbound at the top level, outside the dynamic
extent of any CALL-WITH-TRANSACTION call."
  (is (not (boundp '*transaction*))))

(test call-with-transaction-binds-star-transaction-to-the-same-instance
  "CALL-WITH-TRANSACTION dynamically binds *TRANSACTION* to the
exact GIT-TRANSACTION CALL-WITH-GIT-TRANSACTION constructs, for the
duration of RECEIVER's call, and *TRANSACTION* reverts to unbound
once the call returns."
  (let ((repository (%make-test-repository :read-write))
        (update-calls '())
        captured-transaction)
    (with-fake-git-show-ref-sha ('())
      (with-fake-git-object-store ()
        (with-recording-git-update-ref (update-calls)
          (let ((transaction
                  (call-with-transaction repository :read-write
                                          :receiver (lambda (value)
                                                      (declare (ignore value))
                                                      (setf captured-transaction *transaction*)
                                                      42))))
            (is (eq transaction captured-transaction))))))
    (is (not (boundp '*transaction*)))))
