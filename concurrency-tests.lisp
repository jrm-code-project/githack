;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite concurrency-suite
  :in githack-suite
  :description "Multi-thread tests confirming that concurrently loading/navigating a single, shared, in-memory GIT-OBJECT/PERSISTENT-OBJECT proxy graph is safe -- see git-object.lisp's own THREAD SAFETY commentary and git-transaction.lisp's CONCURRENCY POLICY comment.")

(in-suite concurrency-suite)

(defun run-concurrently (thread-count thunk)
  "Run THUNK (a function of zero arguments) simultaneously in
THREAD-COUNT separate SB-THREAD threads, all released at once via a
shared semaphore (to maximize the chance of an actual race, rather
than each thread finishing before the next even starts), and return
a fresh list of THREAD-COUNT results, each either THUNK's own return
value for that thread, or the CONDITION it signaled, captured via
HANDLER-CASE so one thread's error can neither silently abort the
others nor this function's own JOIN-THREAD calls."
  (let ((start (sb-thread:make-semaphore :count 0)))
    (flet ((run-one ()
             (sb-thread:wait-on-semaphore start)
             (handler-case (funcall thunk)
               (error (c) c))))
      (let ((threads (loop repeat thread-count collect (sb-thread:make-thread #'run-one))))
        (sb-thread:signal-semaphore start thread-count)
        (mapcar #'sb-thread:join-thread threads)))))

(defun no-errors-p (results)
  "Return true if none of RESULTS (as returned by RUN-CONCURRENTLY)
is a CONDITION."
  (notany (lambda (r) (typep r 'condition)) results))

(test concurrent-ensure-blob-loaded-installs-a-consistent-payload
  "Many threads calling %ENSURE-BLOB-LOADED on the very same, shared,
initially-hollow GIT-BLOB instance all observe the same, correctly
decoded PAYLOAD, none signals an error, and the blob ends up marked
loaded."
  (with-fake-git-repository ()
    (let* ((sha (git-hash-object :dummy-repo "blob" (serialize-atom 424242)))
           (blob (make-instance 'git-blob :repository :dummy-repo :sha sha))
           (results (run-concurrently 24 (lambda () (get-payload (%ensure-blob-loaded blob))))))
      (is (no-errors-p results))
      (is (every (lambda (r) (eql r 424242)) results))
      (is (get-loaded? blob)))))

(test concurrent-ensure-tree-entries-loaded-installs-consistent-entries
  "Many threads calling %ENSURE-TREE-ENTRIES-LOADED on the very same,
shared, initially-hollow GIT-TREE instance all observe the same
correctly decoded ENTRIES, and none signals an error."
  (with-fake-git-repository ()
    (let* ((blob-sha (git-hash-object :dummy-repo "blob" (serialize-atom "hello")))
           (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
           (tree-sha (let ((scratch (make-instance 'git-tree :repository :dummy-repo
                                                              :entries (list (cons "greeting" blob)))))
                       (git-hash-object :dummy-repo "tree" (serialize-tree scratch))))
           (tree (make-instance 'git-tree :repository :dummy-repo :sha tree-sha))
           (results (run-concurrently
                     24
                     (lambda ()
                       (%ensure-tree-entries-loaded :dummy-repo tree)
                       (get-payload (%ensure-blob-loaded (cdr (assoc "greeting" (get-entries tree) :test #'string=))))))))
      (is (no-errors-p results))
      (is (every (lambda (r) (string= r "hello")) results))
      (is (get-loaded? tree)))))

(test concurrent-ensure-persistent-cons-loaded-retypes-and-loads-safely
  "Many threads calling %ENSURE-PERSISTENT-CONS-LOADED on the very
same, shared GIT-TREE instance (deliberately not yet retyped into a
PERSISTENT-CONS) all end up observing a single, fully and correctly
loaded PERSISTENT-CONS -- exercising WITH-OBJECT-LOAD-LOCK's own
serialization of the CL:CHANGE-CLASS retyping step, which is not
safe to race directly."
  (with-fake-git-repository ()
    (let* ((car-blob (make-instance 'git-blob :repository :dummy-repo :payload :the-car :loaded? t))
           (cdr-blob (make-instance 'git-blob :repository :dummy-repo :payload :the-cdr :loaded? t))
           (original (make-instance 'persistent-cons :repository :dummy-repo
                                                       :persistent-car car-blob
                                                       :persistent-cdr cdr-blob))
           (cons-sha (serialize-persistent-cons original))
           (hollow (make-instance 'git-tree :repository :dummy-repo :sha cons-sha))
           (results (run-concurrently
                     24
                     (lambda ()
                       (let ((loaded (%ensure-persistent-cons-loaded hollow)))
                         (list (typep loaded 'persistent-cons)
                               (get-payload (%ensure-blob-loaded (persistent-car loaded)))
                               (get-payload (%ensure-blob-loaded (persistent-cdr loaded)))))))))
      (is (no-errors-p results))
      (is (every (lambda (r) (equal r (list t :the-car :the-cdr))) results))
      (is (typep hollow 'persistent-cons))
      (is (get-loaded? hollow)))))

(test concurrent-ensure-persistent-wttree-node-loaded-retypes-and-loads-safely
  "Many threads calling %ENSURE-PERSISTENT-WTTREE-NODE-LOADED on the
very same, shared GIT-TREE instance (deliberately not yet retyped
into a PERSISTENT-WTTREE) all end up observing a single, fully and
correctly loaded PERSISTENT-WTTREE node."
  (with-fake-git-repository ()
    (let* ((original (wt-singleton :dummy-repo :a-key :a-value))
           (node-sha (serialize-persistent-wttree-node original))
           (hollow (make-instance 'git-tree :repository :dummy-repo :sha node-sha))
           (results (run-concurrently
                     24
                     (lambda ()
                       (let ((loaded (%ensure-persistent-wttree-node-loaded hollow)))
                         (list (typep loaded 'persistent-wttree)
                               (wt-node-key loaded)
                               (wt-node-value loaded)))))))
      (is (no-errors-p results))
      (is (every (lambda (r) (equal r (list t :a-key :a-value))) results))
      (is (typep hollow 'persistent-wttree))
      (is (get-loaded? hollow)))))

(test concurrent-persistent-vector-ref-caches-every-index-consistently
  "Many threads, each calling PERSISTENT-VECTOR-REF for every index of
the very same, shared, initially-hollow PERSISTENT-VECTOR instance
(in an interleaved, cache-array-allocating order), all observe
exactly the original elements, and none signals an error."
  (with-fake-git-repository ()
    (let* ((original-values (loop for i from 0 below 30 collect (* i i)))
           (vector-sha (let ((original (collect-persistent-vector :dummy-repo original-values)))
                         (serialize-persistent-vector original)))
           (vector (make-instance 'persistent-vector :repository :dummy-repo :sha vector-sha))
           (results (run-concurrently
                     24
                     (lambda ()
                       (loop for i from 0 below 30 collect (persistent-vector-ref vector i))))))
      (is (no-errors-p results))
      (is (every (lambda (r) (equal r original-values)) results)))))

(test cas-install-once-installs-only-the-first-racing-value
  "%CAS-INSTALL-ONCE returns NEW (and installs it) when PLACE still
holds OLD, or the already-installed value from a prior winning call
when it does not, and every subsequent call, regardless of which NEW
it was itself passed, converges on that same single winning value."
  (let ((holder (make-instance 'git-blob :repository :dummy-repo :payload nil)))
    (is (eql :first (%cas-install-once (slot-value holder 'githack::payload) nil :first)))
    (is (eql :first (%cas-install-once (slot-value holder 'githack::payload) nil :second)))
    (is (eql :first (get-payload holder)))))

(test publish-loaded-is-idempotent-and-only-ever-transitions-nil-to-t
  "%PUBLISH-LOADED! transitions a fresh, unloaded GIT-OBJECT's own
LOADED? slot from NIL to T exactly once, is a harmless no-op on any
later call, and always returns OBJECT."
  (let ((blob (make-instance 'git-blob :repository :dummy-repo :payload 1)))
    (is (not (get-loaded? blob)))
    (is (eq blob (%publish-loaded! blob)))
    (is (get-loaded? blob))
    (is (eq blob (%publish-loaded! blob)))
    (is (get-loaded? blob))))
