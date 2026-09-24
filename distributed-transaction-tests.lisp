;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests for GitHack's distributed (multi-repository)
;;; Two-Phase-Commit transaction layer (distributed-transaction.lisp
;;; / distributed-transaction-context.lisp): WITH-GITHACK-
;;; TRANSACTION's own 0/1/>1-participant no-op/Fast-Path/2PC
;;; dispatch, Phase 1 (Prepare) failure/rollback, and RUN-GITHACK-
;;; EXORCIST's crash recovery for both the "already committed" and
;;; "never committed" cases. One test below kills a child SBCL between
;;; Phase 1 and the ledger write. Another kills a child after the
;;; ledger blob is on disk and after participant 1 has rolled
;;; forward, before participant 2's branch moves. The remaining
;;; crash cases reconstruct that on-disk state in-process. Like
;;; END-TO-END-SUITE, every test here
;;; genuinely shells out to real `git` executables against real,
;;; temporary bare repositories (via WITH-TEMPORARY-GIT-REPOSITORY)
;;; -- no GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/
;;; GIT-UPDATE-REF! fake is ever installed here, since the whole
;;; point of this feature is its use of genuine Git plumbing
;;; (`mktag`, `update-ref --stdin`, `for-each-ref`, `rev-parse`).

(def-suite distributed-transaction-suite
  :in githack-suite
  :description "End-to-end tests for GitHack's distributed (multi-repository) Two-Phase-Commit transaction layer, against real temporary bare Git repositories.")

(in-suite distributed-transaction-suite)

(defparameter +dtx-author+ "Distributed Test <dtx@githack.local>")

(defun dtx-write! (repository branch value)
  "Open an ordinary, single-repository, real :READ-WRITE transaction
against REPOSITORY/BRANCH (both real, via WITH-REPOSITORY/WITH-
TRANSACTION) and commit VALUE as its new root. Whether this actually
advances BRANCH immediately, or is instead silently deferred as a
pending write against some enclosing WITH-GITHACK-TRANSACTION,
depends entirely on whether *CURRENT-TRANSACTION* happens to be
bound at the time -- exactly the transparency this whole feature is
about."
  (with-repository (repo) (repository :branch branch :author +dtx-author+ :committer +dtx-author+
                                       :message "dtx" :mode :read-write)
    (with-transaction (v) (repo :read-write)
      (declare (ignore v))
      value)))

(defun dtx-read (repository branch)
  "Return the current plain Lisp value of REPOSITORY/BRANCH's own
root, via a real, read-only WITH-REPOSITORY/WITH-TRANSACTION round
trip, or NIL if BRANCH does not exist yet."
  (let ((result nil))
    (with-repository (repo) (repository :branch branch :mode :read-only)
      (with-transaction (v) (repo :read-only)
        (setf result v)
        v))
    result))

(test zero-participant-githack-transaction-is-a-no-op
  "A WITH-GITHACK-TRANSACTION body that opens no ordinary
transaction against any repository at all commits nothing and
signals nothing."
  (with-temporary-git-repository (repository)
    (finishes
      (with-githack-transaction ()
        (+ 1 2)))
    (is (null (git-show-ref-sha repository "main")))))

(test one-participant-githack-transaction-takes-the-fast-path
  "A WITH-GITHACK-TRANSACTION body that writes to exactly one
repository/branch commits it directly via the Fast Path (no Ledger,
no prepare/ledger refs of any kind), and the branch ends up holding
the expected value."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (dtx-write! repository "main" "solo-value"))
    (is (equal "solo-value" (dtx-read repository "main")))
    (is (null (%git-for-each-ref repository "refs/githack/")))))

(test two-participant-githack-transaction-commits-both-via-2pc
  "A WITH-GITHACK-TRANSACTION body that writes to two distinct
repositories drives the full Two-Phase-Commit protocol: both
branches end up advanced to their own expected values, and no
`refs/githack/prepare/...` tracking ref is left stranded in either
repository afterward (the Ledger's own `refs/githack/ledger/<tx-id>`
ref, by contrast, is permanent -- see its own docstring -- and so is
expected to remain)."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-githack-transaction ()
        (dtx-write! repository-1 "main" "value-one")
        (dtx-write! repository-2 "main" "value-two"))
      (is (equal "value-one" (dtx-read repository-1 "main")))
      (is (equal "value-two" (dtx-read repository-2 "main")))
      (is (null (remove nil (%git-for-each-ref repository-1 "refs/githack/")
                         :key (lambda (entry) (search "refs/githack/ledger/" (third entry)))
                         :test-not #'eq)))
      (is (null (%git-for-each-ref repository-2 "refs/githack/"))))))

(test three-participant-githack-transaction-commits-all-three-via-2pc
  "A WITH-GITHACK-TRANSACTION body spanning three distinct
repositories advances all three branches, exercising Phase 1/Phase 2
across more than the minimal two-participant case."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-temporary-git-repository (repository-3)
        (with-githack-transaction ()
          (dtx-write! repository-1 "main" "one")
          (dtx-write! repository-2 "main" "two")
          (dtx-write! repository-3 "main" "three"))
        (is (equal "one" (dtx-read repository-1 "main")))
        (is (equal "two" (dtx-read repository-2 "main")))
        (is (equal "three" (dtx-read repository-3 "main")))))))

(test githack-transaction-error-in-body-leaves-every-participant-untouched
  "If a WITH-GITHACK-TRANSACTION body signals an error after already
writing to two distinct repositories, neither repository's branch is
ever advanced (nothing reaches Phase 1 at all -- %FINISH-GITHACK-
TRANSACTION! is simply never called), and the error propagates to
the caller unchanged."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (signals simple-error
        (with-githack-transaction ()
          (dtx-write! repository-1 "main" "should-not-stick-1")
          (dtx-write! repository-2 "main" "should-not-stick-2")
          (error "simulated failure in body")))
      (is (null (git-show-ref-sha repository-1 "main")))
      (is (null (git-show-ref-sha repository-2 "main")))
      (is (null (%git-for-each-ref repository-1 "refs/githack/")))
      (is (null (%git-for-each-ref repository-2 "refs/githack/"))))))

(test githack-transaction-second-two-phase-commit-does-not-collide-with-the-first
  "Two entirely separate, sequential WITH-GITHACK-TRANSACTIONs, each
spanning the same two repositories, both succeed and each leaves the
branches at its own final value -- confirms TX-ID-namespaced
prepare/ledger refs from the first transaction never interfere with
the second."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (with-githack-transaction ()
        (dtx-write! repository-1 "main" "first-round-1")
        (dtx-write! repository-2 "main" "first-round-2"))
      (with-githack-transaction ()
        (dtx-write! repository-1 "main" "second-round-1")
        (dtx-write! repository-2 "main" "second-round-2"))
      (is (equal "second-round-1" (dtx-read repository-1 "main")))
      (is (equal "second-round-2" (dtx-read repository-2 "main"))))))

(test phase-1-prepare-failure-rolls-back-already-prepared-participants
  "If Phase 1's own Prepare step fails against a later participant
(simulated here by a stranded, colliding prepare ref already
occupying that participant's own `refs/githack/prepare/<tx-id>/
<branch-name>` path before Phase 1 even begins), DISTRIBUTED-
TRANSACTION-ERROR is signaled, the earlier participant's own already-
created prepare ref is deleted again (rolled back), and neither
branch is ever advanced."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (let ((tx-id (generate-transaction-id)))
        ;; Pre-occupy repository-2's own prepare ref path for TX-ID with
        ;; a bogus blob, so %PREPARE-PARTICIPANT!'s own %GIT-RAW-UPDATE-REF!
        ;; (which requires the ref not already exist) fails for it.
        (let ((bogus-sha (git-hash-object repository-2 "blob" (sb-ext:string-to-octets "bogus" :external-format :utf-8))))
          (%git-raw-update-ref! repository-2 (prepare-ref-path tx-id "main") bogus-sha :expected-sha nil))
        (let ((txn (%make-githack-transaction tx-id)))
          (let ((*current-transaction* txn))
            (dtx-write! repository-1 "main" "will-be-rolled-back-1")
            (dtx-write! repository-2 "main" "will-be-rolled-back-2"))
          (signals distributed-transaction-error
            (%finish-githack-transaction! txn)))
        ;; repository-1's own prepare ref (created before repository-2's
        ;; own Prepare step failed) must have been rolled back again.
        (is (null (remove nil (%git-for-each-ref repository-1 "refs/githack/")
                           :key (lambda (entry) (search "prepare" (third entry)))
                           :test #'eq)))
        (is (null (git-show-ref-sha repository-1 "main")))
        (is (null (git-show-ref-sha repository-2 "main")))
        (is (not (probe-file (%branch-ref-lock-pathname repository-1 "main"))))
        (is (not (probe-file (%branch-ref-lock-pathname repository-2 "main"))))))))

(test exorcist-rolls-back-a-stranded-prepare-ref-with-no-ledger
  "RUN-GITHACK-EXORCIST!, run against a repository holding a stranded
`refs/githack/prepare/<tx-id>/<branch-name>` ref whose own Ledger
repository never received its `refs/githack/ledger/<tx-id>` marker
(simulating a crash between Phase 1 and Phase 2's own Point-of-No-
Return), simply deletes the stranded ref and leaves the branch
untouched."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "never-committed"))
      (let* ((pw (first (%githack-transaction/pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw) (pending-write/git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text))
      ;; Crash simulated here: the Ledger ref (in this same repository,
      ;; since it is the sole, and so its own elected, participant) is
      ;; never written.
      (let ((results (run-githack-exorcist! repository)))
        (is (equal (list (list tx-id "main" :rolled-back)) results)))
      (is (null (git-show-ref-sha repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/")))
      (is (not (probe-file (%branch-ref-lock-pathname repository "main")))))))

(test exorcist-rolls-forward-a-stranded-prepare-ref-with-a-written-ledger
  "RUN-GITHACK-EXORCIST!, run against a repository holding a stranded
`refs/githack/prepare/<tx-id>/<branch-name>` ref whose own Ledger
repository DID already receive its `refs/githack/ledger/<tx-id>`
marker (simulating a crash after Phase 2's own Point-of-No-Return
but before its roll-forward loop reached this participant), fast-
forwards the branch to the prepared commit and deletes the stranded
ref."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "should-be-committed"))
      (let* ((pw (first (%githack-transaction/pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw) (pending-write/git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text)
        ;; Point of no return reached, then crash simulated before roll-forward.
        (%write-ledger-commit-point! (pending-write/git-repository pw) tx-id))
      (let ((results (run-githack-exorcist! repository)))
        (is (equal (list (list tx-id "main" :committed)) results)))
      (is (equal "should-be-committed" (dtx-read repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/"))))))

(test exorcist-is-a-no-op-for-a-repository-with-no-stranded-refs
  "RUN-GITHACK-EXORCIST! against a repository with no stranded
`refs/githack/prepare/...` refs at all (the overwhelmingly common
case) returns the empty list and touches nothing."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (dtx-write! repository "main" "ordinary-value"))
    (is (null (run-githack-exorcist! repository)))
    (is (equal "ordinary-value" (dtx-read repository "main")))))

(test exorcist-is-idempotent-once-a-stranded-ref-is-already-resolved
  "Calling RUN-GITHACK-EXORCIST! a second time, immediately after it
already resolved a stranded ref, finds nothing left to do."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "resolved-once"))
      (let* ((pw (first (%githack-transaction/pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw) (pending-write/git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text)
        (%write-ledger-commit-point! (pending-write/git-repository pw) tx-id))
      (run-githack-exorcist! repository)
      (is (null (run-githack-exorcist! repository))))))

(defun %exorcist-kill-poll (predicate seconds)
  "Return true once PREDICATE is true, checking every tenth of a
second for up to SECONDS. Used to wait on a child process without
blocking the test forever."
  (dotimes (i (round (* seconds 10)) nil)
    (when (funcall predicate)
      (return t))
    (sleep 0.1)))

(defun %exorcist-kill-tail (path)
  "Return up to the last 4000 characters of PATH, or a short note
when the file cannot be read."
  (or (ignore-errors
        (with-open-file (stream path :direction :input :if-does-not-exist nil)
          (when stream
            (let* ((size (file-length stream))
                   (start (max 0 (- size 4000))))
              (file-position stream start)
              (let ((text (make-string (- size start))))
                (read-sequence text stream)
                text)))))
      "<log unreadable>"))

(defun %write-exorcist-kill-line (stream control &rest args)
  "Write one line of the child script. CONTROL is a FORMAT control
with no embedded newlines; each call ends the line itself."
  (apply #'format stream control args)
  (terpri stream))

(defun %write-exorcist-kill-child-script (script ready repository-1 repository-2)
  "Write a standalone SBCL script that runs a real two-repository
WITH-GITHACK-TRANSACTION against REPOSITORY-1 and REPOSITORY-2, and
blocks inside %WRITE-LEDGER-COMMIT-POINT! -- the call %FINISH-TWO-
PHASE-COMMIT! makes immediately after every Prepare has returned --
after closing Git sessions and creating READY."
  (with-open-file (stream script :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
          (asd (asdf:system-source-file (asdf:find-system :githack))))
      (%write-exorcist-kill-line stream "(load ~S)" quicklisp)
      (%write-exorcist-kill-line stream "(asdf:load-asd ~S)" asd)
      (%write-exorcist-kill-line stream "(ql:quickload :githack :silent t)")
      (%write-exorcist-kill-line stream "(in-package \"GITHACK\")")
      (%write-exorcist-kill-line stream "(setf (fdefinition '%write-ledger-commit-point!)")
      (%write-exorcist-kill-line stream "      (lambda (&rest args)")
      (%write-exorcist-kill-line stream "        (declare (ignore args))")
      (%write-exorcist-kill-line stream "        (close-git-io-sessions)")
      (%write-exorcist-kill-line stream "        (with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create)" ready)
      (%write-exorcist-kill-line stream "          (write-line \"prepared\" stream)")
      (%write-exorcist-kill-line stream "          (finish-output stream))")
      (%write-exorcist-kill-line stream "        (dotimes (i 1000000)")
      (%write-exorcist-kill-line stream "          (sleep 60))))")
      (%write-exorcist-kill-line stream "(with-githack-transaction ()")
      (%write-exorcist-kill-line stream "  (with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-1 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "    (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "      (declare (ignore v))")
      (%write-exorcist-kill-line stream "      \"should-not-survive-1\"))")
      (%write-exorcist-kill-line stream "  (with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-2 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "    (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "      (declare (ignore v))")
      (%write-exorcist-kill-line stream "      \"should-not-survive-2\")))")
      (%write-exorcist-kill-line stream "(write-line \"ledger write returned; the process was not killed\")")
      (%write-exorcist-kill-line stream "(sb-ext:exit :code 2)"))))

(test exorcist-rolls-back-when-the-process-is-killed-between-prepare-and-ledger-write
  "A real child SBCL runs WITH-GITHACK-TRANSACTION across two
repositories. Phase 1 Prepare finishes, both prepare refs are on
disk, and the child then blocks inside the production call that
would write the ledger ref. The parent kills that process with
taskkill /F (UIOP:TERMINATE-PROCESS :URGENT T) before the ledger
ref exists. RUN-GITHACK-EXORCIST!, in this process, rolls both
participants back: the branches stay absent and the prepare refs
are gone."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (let* ((suffix (format nil "~(~36,8,'0R~)" (random (expt 36 8) (make-random-state t))))
             (ready (merge-pathnames (format nil "githack-kill-~A-ready" suffix)
                                     (uiop:default-temporary-directory)))
             (script (merge-pathnames (format nil "githack-kill-~A-child.lisp" suffix)
                                      (uiop:default-temporary-directory)))
             (log (merge-pathnames (format nil "githack-kill-~A-child.log" suffix)
                                   (uiop:default-temporary-directory)))
             (child nil))
        (unwind-protect
             (progn
               (%write-exorcist-kill-child-script script ready repository-1 repository-2)
               (setf child (uiop:launch-program
                            (list "sbcl" "--noinform" "--disable-debugger" "--load" (namestring script))
                            :output log :error-output :output))
               (let ((reached (%exorcist-kill-poll (lambda () (probe-file ready)) 120)))
                 (unless reached
                   (when (ignore-errors (uiop:process-alive-p child))
                     (ignore-errors (uiop:terminate-process child :urgent t))
                     (ignore-errors (%exorcist-kill-poll (lambda () (not (uiop:process-alive-p child))) 15))
                     (ignore-errors (uiop:wait-process child)))
                   (sleep 0.2))
                 (is (not (null reached))
                     "Child never reached the ledger write.~%~A"
                     (%exorcist-kill-tail log))
                 (when reached
                   (let* ((prepared-1 (%git-for-each-ref repository-1 "refs/githack/prepare/"))
                          (prepared-2 (%git-for-each-ref repository-2 "refs/githack/prepare/"))
                          (tx-id (and prepared-1 (prepare-tx-id-from-ref (third (first prepared-1))))))
                     (is (= 1 (length prepared-1)))
                     (is (= 1 (length prepared-2)))
                     (is (string= tx-id (prepare-tx-id-from-ref (third (first prepared-2)))))
                     (is (null (%git-for-each-ref repository-1 "refs/githack/ledger/")))
                     (is (null (%git-for-each-ref repository-2 "refs/githack/ledger/")))
                     (is (null (git-show-ref-sha repository-1 "main")))
                     (is (null (git-show-ref-sha repository-2 "main")))
                     (is (uiop:process-alive-p child))
                     (uiop:terminate-process child :urgent t)
                     (is (%exorcist-kill-poll (lambda () (not (uiop:process-alive-p child))) 15)
                         "taskkill /F did not kill the child.~%~A"
                         (%exorcist-kill-tail log))
                     (ignore-errors (uiop:wait-process child))
                     (is (equal (list (list tx-id "main" :rolled-back))
                                (run-githack-exorcist! repository-1)))
                     (is (equal (list (list tx-id "main" :rolled-back))
                                (run-githack-exorcist! repository-2)))
                     (is (null (dtx-read repository-1 "main")))
                     (is (null (dtx-read repository-2 "main")))
                     (is (null (%git-for-each-ref repository-1 "refs/githack/")))
                     (is (null (%git-for-each-ref repository-2 "refs/githack/")))))))
          (when (and child (ignore-errors (uiop:process-alive-p child)))
            (ignore-errors (uiop:terminate-process child :urgent t))
            (ignore-errors (uiop:wait-process child)))
          (ignore-errors (delete-file ready))
          (ignore-errors (delete-file script))
          (ignore-errors (delete-file log)))))))

(defun %write-exorcist-kill-after-ledger-script (script ready repository-1 repository-2)
  "Write a standalone SBCL script that seeds both repositories on
\"main\" with different commits, then runs a real WITH-GITHACK-
TRANSACTION. %FINISH-TWO-PHASE-COMMIT! writes the ledger blob and
rolls participant 1 forward with the real %ROLL-FORWARD-PARTICIPANT!.
The wrapper calls that real function for every participant except
REPOSITORY-2. For REPOSITORY-2 it closes Git sessions, creates READY,
and sleeps, without touching REPOSITORY-2's branch. %WRITE-LEDGER-
COMMIT-POINT! is not replaced."
  (with-open-file (stream script :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
          (asd (asdf:system-source-file (asdf:find-system :githack))))
      (%write-exorcist-kill-line stream "(load ~S)" quicklisp)
      (%write-exorcist-kill-line stream "(asdf:load-asd ~S)" asd)
      (%write-exorcist-kill-line stream "(ql:quickload :githack :silent t)")
      (%write-exorcist-kill-line stream "(in-package \"GITHACK\")")
      (%write-exorcist-kill-line stream "(let ((real (fdefinition '%roll-forward-participant!))")
      (%write-exorcist-kill-line stream "      (target (uiop:native-namestring ~S)))" repository-2)
      (%write-exorcist-kill-line stream "  (setf (fdefinition '%roll-forward-participant!)")
      (%write-exorcist-kill-line stream "        (lambda (pw)")
      (%write-exorcist-kill-line stream "          (if (equal target (uiop:native-namestring (get-pathname (pending-write/git-repository pw))))")
      (%write-exorcist-kill-line stream "              (progn")
      (%write-exorcist-kill-line stream "                (close-git-io-sessions)")
      (%write-exorcist-kill-line stream "                (with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create)" ready)
      (%write-exorcist-kill-line stream "                  (write-line \"ledger-written\" stream)")
      (%write-exorcist-kill-line stream "                  (finish-output stream))")
      (%write-exorcist-kill-line stream "                (dotimes (i 1000000)")
      (%write-exorcist-kill-line stream "                  (sleep 60)))")
      (%write-exorcist-kill-line stream "              (funcall real pw)))))")
      (%write-exorcist-kill-line stream "(with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-1 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "  (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "    (declare (ignore v))")
      (%write-exorcist-kill-line stream "    \"seed-1\"))")
      (%write-exorcist-kill-line stream "(with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-2 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "  (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "    (declare (ignore v))")
      (%write-exorcist-kill-line stream "    \"seed-2\"))")
      (%write-exorcist-kill-line stream "(with-githack-transaction ()")
      (%write-exorcist-kill-line stream "  (with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-1 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "    (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "      (declare (ignore v))")
      (%write-exorcist-kill-line stream "      \"final-1\"))")
      (%write-exorcist-kill-line stream "  (with-repository (repo) (~S :branch \"main\" :author ~S :committer ~S :message \"dtx\" :mode :read-write)"
                                 repository-2 +dtx-author+ +dtx-author+)
      (%write-exorcist-kill-line stream "    (with-transaction (v) (repo :read-write)")
      (%write-exorcist-kill-line stream "      (declare (ignore v))")
      (%write-exorcist-kill-line stream "      \"final-2\")))")
      (%write-exorcist-kill-line stream "(write-line \"repository 2 roll-forward returned; the process was not killed\")")
      (%write-exorcist-kill-line stream "(sb-ext:exit :code 2)"))))

(test exorcist-rolls-forward-repo-2-after-the-process-is-killed-with-the-ledger-on-disk
  "A real child SBCL seeds both repositories on \"main\" with
different commits, then runs WITH-GITHACK-TRANSACTION. The real
%WRITE-LEDGER-COMMIT-POINT! writes the ledger blob. The real
%ROLL-FORWARD-PARTICIPANT! advances repository 1. The child then
blocks at the start of repository 2's roll-forward, before that
branch moves, and the parent kills it with taskkill /F. While the
child is still alive the ledger ref is a blob, repository 1 already
reads \"final-1\", and repository 2 is still \"seed-2\" with its
prepare ref intact. RUN-GITHACK-EXORCIST! on repository 2 only then
advances that branch to the commit the prepare ref already pointed
at, and the prepare ref is gone."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (let* ((suffix (format nil "~(~36,8,'0R~)" (random (expt 36 8) (make-random-state t))))
             (ready (merge-pathnames (format nil "githack-kill-ledger-~A-ready" suffix)
                                     (uiop:default-temporary-directory)))
             (script (merge-pathnames (format nil "githack-kill-ledger-~A-child.lisp" suffix)
                                      (uiop:default-temporary-directory)))
             (log (merge-pathnames (format nil "githack-kill-ledger-~A-child.log" suffix)
                                   (uiop:default-temporary-directory)))
             (child nil))
        (unwind-protect
             (progn
               (%write-exorcist-kill-after-ledger-script script ready repository-1 repository-2)
               (setf child (uiop:launch-program
                            (list "sbcl" "--noinform" "--disable-debugger" "--load" (namestring script))
                            :output log :error-output :output))
               (let ((reached (%exorcist-kill-poll (lambda () (probe-file ready)) 180)))
                 (unless reached
                   (when (ignore-errors (uiop:process-alive-p child))
                     (ignore-errors (uiop:terminate-process child :urgent t))
                     (ignore-errors (%exorcist-kill-poll (lambda () (not (uiop:process-alive-p child))) 15))
                     (ignore-errors (uiop:wait-process child)))
                   (sleep 0.2))
                 (is (not (null reached))
                     "Child never blocked before repository 2's roll-forward.~%~A"
                     (%exorcist-kill-tail log))
                 (when reached
                   (let* ((ledgers (%git-for-each-ref repository-1 "refs/githack/ledger/"))
                          (prepared-2 (%git-for-each-ref repository-2 "refs/githack/prepare/"))
                          (prepare-ref (and prepared-2 (third (first prepared-2))))
                          (prepared-sha (and prepare-ref
                                             (%git-rev-parse repository-2 (format nil "~A^{commit}" prepare-ref))))
                          (stuck-sha (git-show-ref-sha repository-2 "main"))
                          (tx-id (and prepare-ref (prepare-tx-id-from-ref prepare-ref))))
                     (is (= 1 (length ledgers)))
                     (is (string= "blob" (second (first ledgers))))
                     (is (null (%git-for-each-ref repository-1 "refs/githack/prepare/")))
                     (is (equal "final-1" (dtx-read repository-1 "main")))
                     (is (= 1 (length prepared-2)))
                     (is (stringp prepared-sha))
                     (is (not (string= prepared-sha stuck-sha)))
                     (is (equal "seed-2" (dtx-read repository-2 "main")))
                     (is (uiop:process-alive-p child))
                     (uiop:terminate-process child :urgent t)
                     (is (%exorcist-kill-poll (lambda () (not (uiop:process-alive-p child))) 15)
                         "taskkill /F did not kill the child.~%~A"
                         (%exorcist-kill-tail log))
                     (ignore-errors (uiop:wait-process child))
                     (is (equal (list (list tx-id "main" :committed))
                                (run-githack-exorcist! repository-2)))
                     (is (string= prepared-sha (git-show-ref-sha repository-2 "main")))
                     (is (null (%git-for-each-ref repository-2 "refs/githack/prepare/")))
                     (is (equal "final-2" (dtx-read repository-2 "main")))
                     (is (equal "final-1" (dtx-read repository-1 "main")))))))
          (when (and child (ignore-errors (uiop:process-alive-p child)))
            (ignore-errors (uiop:terminate-process child :urgent t))
            (ignore-errors (uiop:wait-process child)))
          (ignore-errors (delete-file ready))
          (ignore-errors (delete-file script))
          (ignore-errors (delete-file log)))))))

(test branch-ref-lock-rejects-an-ordinary-update-until-publish
  "After Prepare, Git's own refs/heads/<branch>.lock is on disk, so
an ordinary git update-ref of that branch fails and the tip stays
on the parent commit. Publishing the lock file then makes the
prepared commit the tip and removes the lock. The ordinary update
does not land."
  (with-temporary-git-repository (repository)
    (dtx-write! repository "main" "seed")
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "final"))
      (let* ((pw (first (%githack-transaction/pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw)
                                                          (pending-write/git-repository pw))))
             (lock (%branch-ref-lock-pathname repository "main")))
        (%prepare-participant! pw tx-id manifest-text)
        (is (probe-file lock))
        (multiple-value-bind (output error-output code)
            (uiop:run-program
             (list "git" (format nil "--git-dir=~A" (uiop:native-namestring repository))
                   "update-ref" "refs/heads/main" (pending-write/new-commit-sha pw))
             :output :string :error-output :string :ignore-error-status t)
          (declare (ignore output))
          (is (not (zerop code)))
          (is (search "lock" error-output)))
        (is (string= (pending-write/old-sha pw) (git-show-ref-sha repository "main")))
        (%write-ledger-commit-point! (pending-write/git-repository pw) tx-id)
        (%roll-forward-participant! pw)
        (is (not (probe-file lock)))
        (is (string= (pending-write/new-commit-sha pw) (git-show-ref-sha repository "main")))
        (is (equal "final" (dtx-read repository "main")))))))

(test exorcist-decodes-feature/foo-from-one-prepare-ref-segment
  "The prepare ref for branch \"feature/foo\" is
refs/githack/prepare/<tx-id>/feature%2Ffoo: one segment after the
tx-id, not a directory named feature. The manifest keeps the raw
name. After the ledger write, RUN-GITHACK-EXORCIST! reports the
branch as \"feature/foo\" and advances refs/heads/feature/foo to the
prepared commit. The unencoded path TX/feature/foo fails the segment
assertion, and a decode that leaves \"%2F\" in the name fails the
branch assertion."
  (with-temporary-git-repository (repository)
    (dtx-write! repository "feature/foo" "seed")
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "feature/foo" "final"))
      (let* ((pw (first (%githack-transaction/pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw)
                                                          (pending-write/git-repository pw))))
             (ref (prepare-ref-path tx-id "feature/foo"))
             (suffix (subseq ref (length "refs/githack/prepare/"))))
        (is (string= (format nil "~A/feature%2Ffoo" tx-id) suffix))
        (is (= 1 (count #\/ suffix)))
        (%prepare-participant! pw tx-id manifest-text)
        (is (equal (list ref)
                   (mapcar #'third (%git-for-each-ref repository "refs/githack/prepare/"))))
        (%write-ledger-commit-point! (pending-write/git-repository pw) tx-id)
        (is (equal (list (list tx-id "feature/foo" :committed))
                   (run-githack-exorcist! repository)))
        (is (string= (pending-write/new-commit-sha pw)
                     (git-show-ref-sha repository "feature/foo")))
        (is (equal "final" (dtx-read repository "feature/foo")))
        (is (null (%git-for-each-ref repository "refs/githack/prepare/")))))))

(defun %strand-second-participant-after-ledger (repository-1 repository-2)
  "Leave a two-repository transaction crashed after the ledger write
and after participant 1's roll-forward, before participant 2's.
Both branches are \"main\" and both already have a commit, and those
two parent SHAs differ. Returns (VALUES TX-ID PW1 PW2) with PW1 the
ledger participant (enlisted first) and PW2 the stranded one.
PENDING-WRITES is pushed, so the list is reversed to restore
first-enlisted order, the same order %FINISH-GITHACK-TRANSACTION!
uses when it builds the manifest. A branch-only FIND therefore
selects PW1 for a recovery of PW2."
  (dtx-write! repository-1 "main" "seed-1")
  (dtx-write! repository-2 "main" "seed-2")
  (let* ((tx-id (generate-transaction-id))
         (txn (%make-githack-transaction tx-id)))
    (let ((*current-transaction* txn))
      (dtx-write! repository-1 "main" "final-1")
      (dtx-write! repository-2 "main" "final-2"))
    (let* ((pending (reverse (%githack-transaction/pending-writes txn)))
           (pw1 (first pending))
           (pw2 (second pending))
           (manifest-text (format-transaction-manifest
                            (build-transaction-manifest tx-id pending (pending-write/git-repository pw1)))))
      (%prepare-participant! pw1 tx-id manifest-text)
      (%prepare-participant! pw2 tx-id manifest-text)
      (%write-ledger-commit-point! (pending-write/git-repository pw1) tx-id)
      (%roll-forward-participant! pw1)
      (values tx-id pw1 pw2))))

(test exorcist-rolls-forward-repo-2-using-its-own-old-sha-not-repo-1s
  "Two repositories, both on \"main\", each with its own non-NIL
parent commit. The transaction is crashed after the ledger write and
after participant 1 rolls forward, before participant 2 does.
RUN-GITHACK-EXORCIST! is invoked on repository 2 only. Repository 2's
branch must advance to its own prepared SHA and its prepare ref must
be gone. A lookup keyed only by branch name selects participant 1,
whose old SHA is a different commit, and the compare-and-swap
refuses."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (multiple-value-bind (tx-id pw1 pw2)
          (%strand-second-participant-after-ledger repository-1 repository-2)
        (is (not (string= (pending-write/old-sha pw1) (pending-write/old-sha pw2))))
        (is (string= (pending-write/old-sha pw2) (git-show-ref-sha repository-2 "main")))
        (is (equal (list (list tx-id "main" :committed))
                   (run-githack-exorcist! repository-2)))
        (is (string= (pending-write/new-commit-sha pw2)
                     (git-show-ref-sha repository-2 "main")))
        (is (null (%git-for-each-ref repository-2 "refs/githack/prepare/")))
        (is (equal "final-2" (dtx-read repository-2 "main")))
        (is (equal "final-1" (dtx-read repository-1 "main")))))))

(test stolen-old-sha-makes-the-roll-forward-batch-a-no-op
  "Feeding participant 1's old SHA into participant 2's roll-forward
batch -- the SHA a branch-only FIND returns, because participant 1
is the first manifest entry named \"main\" -- must not move
participant 2. Git's compare-and-swap refuses, and because the
branch update and the prepare-ref delete are one --stdin batch, the
prepare ref stays."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (multiple-value-bind (tx-id pw1 pw2)
          (%strand-second-participant-after-ledger repository-1 repository-2)
        (declare (ignore tx-id))
        (let ((stolen-old-sha (pending-write/old-sha pw1))
              (own-old-sha (pending-write/old-sha pw2))
              (prepare-ref (pending-write/prepare-ref pw2)))
          (is (not (string= stolen-old-sha own-old-sha)))
          ;; Phase 1 is holding Git's branch lock. Drop it so this
          ;; batch reaches the compare-and-swap instead of failing
          ;; because the lock file exists.
          (%release-branch-ref-lock! repository-2 "main")
          (signals concurrent-modification-error
            (%git-update-ref-stdin! repository-2
                                    (list (list :update "refs/heads/main"
                                                (pending-write/new-commit-sha pw2)
                                                stolen-old-sha)
                                          (list :delete prepare-ref))))
          (is (string= own-old-sha (git-show-ref-sha repository-2 "main")))
          (is (equal (list prepare-ref)
                     (mapcar #'third (%git-for-each-ref repository-2 "refs/githack/prepare/")))))))))
