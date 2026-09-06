;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; A runnable demonstration script for GitHack's ADVANCED-FEATURE
;;; ARCHITECTURE SPEC work: NESTED TRANSACTIONS (see
;;; GITHACK-EXAMPLE-BANK::TRANSFER, in bank.lisp) and every
;;; CONFLICT-RESOLUTION strategy (:ERROR/:RETRY/:LOCK/:REBASE). Load
;;; the system first, then LOAD this file:
;;;   (asdf:load-system :githack/example)
;;;   (load "examples/concurrency-demo.lisp")
;;;
;;; Every section below is self-verifying: it prints what it expects
;;; to see, what it actually observed, and a PASS/FAIL verdict --
;;; there is no FiveAM/assertion framework involved (this is example
;;; code, meant to be read and experimented with interactively, not
;;; part of the test suite in test-package.lisp/end-to-end-tests.lisp),
;;; but every demonstration still genuinely proves its own claim
;;; against real Git commits rather than merely asserting it.
;;;
;;; A NOTE ON HOW THIS SCRIPT PRODUCES ITS CONCURRENCY, AND WHY:
;;; GitHack's own *GIT-IO-SESSIONS* subprocess cache (git-io.lisp) is
;;; keyed only by repository pathname and is not itself synchronized
;;; (see TECHNICAL_DEBT.md) -- so genuinely overlapping Git I/O from
;;; two real OS threads *within a single Lisp image*, racing the same
;;; repository, can corrupt that shared cache, entirely independent of
;;; whatever CONFLICT-RESOLUTION strategy is in play. GitHack's own
;;; test suite (end-to-end-tests.lisp) works around this the same way
;;; this script does:
;;;   * For :ERROR/:RETRY/:REBASE, no real OS-level concurrency is
;;;     needed to prove the point: since the compare-and-swap race
;;;     these strategies resolve is only ever detected once RECEIVER
;;;     itself returns and this transaction attempts its own commit,
;;;     a single Lisp thread can simulate a perfectly real concurrent
;;;     writer just by having RECEIVER itself -- deterministically,
;;;     with no timing dependency at all -- force a real second
;;;     commit onto the branch (via %DEMO-HIJACK-ORPHAN!/
;;;     %DEMO-HIJACK-WITH-TREE!, using the very same low-level
;;;     GIT-HASH-OBJECT/GIT-UPDATE-REF primitives GitHack's own commit
;;;     path itself uses) before returning. GitHack's own commit logic
;;;     then discovers that real race exactly as it would against any
;;;     genuinely independent process, with no mock anywhere in the
;;;     picture.
;;;   * For :LOCK, real OS-level concurrency (SB-THREAD) is not just
;;;     safe but the entire point: because :LOCK's own exclusive
;;;     OS-level lock file (transaction-lock.lisp) guarantees the two
;;;     threads' own Git I/O never actually overlaps in wall-clock
;;;     time, the *GIT-IO-SESSIONS* hazard above never has a chance to
;;;     manifest -- a real race would either flake or corrupt data,
;;;     which is exactly what makes two real concurrent threads a
;;;     meaningful proof of :LOCK's own mutual-exclusion guarantee
;;;     rather than a liability.

(in-package "GITHACK-EXAMPLE-BANK")

(defun %demo-header (title)
  (format t "~2%=== ~A ===~%" title))

(defun %demo-line (control-string &rest args)
  (format t "  ~?~%" control-string args))

;;; ------------------------------------------------------------------
;;; SECTION 1: NESTED TRANSACTIONS
;;;
;;; TRANSFER composes WITHDRAW and DEPOSIT -- each a complete,
;;; independent WITH-TRANSACTION on its own -- into a single outer
;;; commit. This section proves both halves of that claim: a
;;; successful transfer produces exactly one new commit moving funds
;;; atomically between two accounts, and a transfer that would
;;; overdraw the source account leaves BOTH accounts completely
;;; untouched (not even a partial debit survives), because the error
;;; WITHDRAW signals unwinds out of every nested transaction and the
;;; outer one together.
;;; ------------------------------------------------------------------

(defun demo-nested-transactions ()
  (%demo-header "Nested transactions: TRANSFER composes WITHDRAW + DEPOSIT")
  (ensure-bank-initialized)
  (handler-case (open-account "alice" "Alice Anderson" 10000) (error () nil))
  (handler-case (open-account "bob" "Bob Brown" 5000) (error () nil))
  (let ((alice-before (get-balance "alice"))
        (bob-before (get-balance "bob")))
    (%demo-line "Before transfer: alice=~D cents, bob=~D cents." alice-before bob-before)
    (transfer "alice" "bob" 1500)
    (let ((alice-after (get-balance "alice"))
          (bob-after (get-balance "bob")))
      (%demo-line "After transferring 1500 cents alice->bob: alice=~D cents, bob=~D cents." alice-after bob-after)
      (%demo-line "~:[FAIL~;PASS~]: exactly 1500 cents moved, total balance conserved."
                  (and (= alice-after (- alice-before 1500))
                       (= bob-after (+ bob-before 1500))))))
  ;; Now attempt a transfer that would overdraw "bob" (whose balance
  ;; is far smaller than the requested amount): WITHDRAW's own
  ;; INSUFFICIENT-FUNDS-ERROR must unwind out of TRANSFER's entire
  ;; nested transaction chain, leaving both accounts exactly as they
  ;; were -- not even a lone debit from the nested WITHDRAW survives.
  (let ((alice-before (get-balance "alice"))
        (bob-before (get-balance "bob")))
    (%demo-line "Before an overdrawing transfer: alice=~D cents, bob=~D cents." alice-before bob-before)
    (handler-case
        (progn (transfer "bob" "alice" 999999999)
               (%demo-line "FAIL: transfer of 999999999 cents from bob should have signaled INSUFFICIENT-FUNDS-ERROR."))
      (insufficient-funds-error (condition)
        (%demo-line "Caught expected error: ~A" condition)))
    (let ((alice-after (get-balance "alice"))
          (bob-after (get-balance "bob")))
      (%demo-line "After the aborted transfer: alice=~D cents, bob=~D cents." alice-after bob-after)
      (%demo-line "~:[FAIL~;PASS~]: both accounts completely untouched by the aborted nested transaction."
                  (and (= alice-after alice-before) (= bob-after bob-before))))))

;;; ------------------------------------------------------------------
;;; SECTIONS 2-3 SUPPORT: a disposable scratch repository, and a pair
;;; of "hijack" helpers that simulate a genuine concurrent external
;;; writer -- built from the very same low-level primitives GitHack's
;;; own commit path itself uses (GIT-HASH-OBJECT, GIT-UPDATE-REF,
;;; WRAP-ATOMIC-COMMIT-ROOT) -- exactly mirroring end-to-end-tests.lisp's
;;; own %E2E-HIJACK-BRANCH!/%E2E-HIJACK-BRANCH-WITH-TREE! helpers. Unlike
;;; SECTIONS 1 and 4, which read/write the self-hosting
;;; "database-example-bank" branch, SECTIONS 2 and 3 use their own
;;; scratch repository: they demonstrate GitHack's raw CONFLICT-
;;; RESOLUTION mechanics directly (a generic GitHack feature, not
;;; specifically a bank feature), and a disposable repository keeps
;;; that demonstration from leaving repeated racer commits behind on
;;; the bank's own history.
;;; ------------------------------------------------------------------

(defun %demo-temporary-bare-repository ()
  "Create and return the pathname of a brand-new, empty, real bare
Git repository (via `git init --bare`) inside a fresh temporary
directory. Reimplemented here, rather than depending on the
GITHACK-TEST system's own WITH-TEMPORARY-GIT-REPOSITORY, since
example code must stand on its own."
  (let ((path (merge-pathnames
               (make-pathname :directory (list :relative
                                                (format nil "githack-concurrency-demo-~36R-~36R"
                                                        (get-universal-time) (random (expt 36 6)))))
               (uiop:default-temporary-directory))))
    (ensure-directories-exist path)
    (uiop:run-program (list "git" "init" "--bare" (uiop:native-namestring path))
                       :output nil :error-output nil)
    path))

(defun %demo-delete-temporary-repository (path)
  "Close any persistent `git` subprocess sessions GitHack opened
against PATH (see GITHACK:CLOSE-GIT-IO-SESSIONS), then recursively
delete it."
  (ignore-errors (githack:close-git-io-sessions path))
  (ignore-errors (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore)))

(defparameter +demo-racer-signature+ "GitHack Concurrency Demo Racer <racer@githack.local>")

(defun %demo-decode-blob (git-object)
  "Decode GIT-OBJECT (a GIT-BLOB, fetched for real via
GITHACK::%ENSURE-BLOB-LOADED if not already loaded) into its real
Lisp PAYLOAD."
  (githack:get-payload (githack::%ensure-blob-loaded git-object)))

(defun %demo-hijack-orphan! (repository-path branch-name payload)
  "Simulate a genuine concurrent external writer: build and persist a
brand-new, real, orphan GIT-COMMIT wrapping PAYLOAD as its root, then
unconditionally force BRANCH-NAME to point at it via GIT-UPDATE-REF --
exactly as if some other, wholly independent process had already
advanced BRANCH-NAME out from under an in-flight transaction, between
its own read and its own commit. Used to force a genuine
CONCURRENT-MODIFICATION-ERROR/:RETRY, which never needs the racer to
share any real Git ancestry with the transaction it races."
  (let ((blob (make-instance 'githack:git-blob :repository repository-path :payload payload)))
    (setf (githack:sha blob) (githack:git-hash-object repository-path "blob" (githack:serialize-atom payload)))
    (let* ((tree (githack:wrap-atomic-commit-root repository-path blob))
           (commit (make-instance 'githack:git-commit
                                   :repository repository-path
                                   :tree tree
                                   :parents '()
                                   :author +demo-racer-signature+
                                   :committer +demo-racer-signature+
                                   :timestamp 0
                                   :message "racer"
                                   :loaded? t)))
      (setf (githack:sha commit)
            (githack:git-hash-object repository-path "commit"
                                      (sb-ext:string-to-octets (githack:serialize-commit commit) :external-format :utf-8)))
      (githack:git-update-ref repository-path branch-name (githack:sha commit)))))

(defun %demo-hijack-with-tree! (repository-path branch-name parent-sha entries)
  "Simulate a genuine concurrent external writer racing an in-flight
:REBASE transaction, but -- unlike %DEMO-HIJACK-ORPHAN! -- as a real
descendant of PARENT-SHA rather than a brand-new orphan, so that
`git merge-tree` can locate a genuine common merge base and either
auto-merge cleanly or report a real content conflict, exactly as two
independent, concurrent, ancestry-related writers would. ENTRIES is
an alist of (FILENAME . PAYLOAD), each PAYLOAD an arbitrary atomic
Lisp value persisted as its own real GIT-BLOB and referenced from a
single new real GIT-TREE built from all of ENTRIES."
  (let ((tree (make-instance 'githack:git-tree
                              :repository repository-path
                              :entries (mapcar (lambda (entry)
                                                 (let ((blob (make-instance 'githack:git-blob
                                                                            :repository repository-path
                                                                            :payload (cdr entry))))
                                                   (setf (githack:sha blob)
                                                         (githack:git-hash-object repository-path "blob"
                                                                                  (githack:serialize-atom (cdr entry))))
                                                   (cons (car entry) blob)))
                                               entries))))
    (setf (githack:sha tree) (githack:git-hash-object repository-path "tree" (githack:serialize-tree tree)))
    (let ((commit (make-instance 'githack:git-commit
                                  :repository repository-path
                                  :tree tree
                                  :parents (list (make-instance 'githack:git-commit :repository repository-path :sha parent-sha))
                                  :author +demo-racer-signature+
                                  :committer +demo-racer-signature+
                                  :timestamp 0
                                  :message "racer"
                                  :loaded? t)))
      (setf (githack:sha commit)
            (githack:git-hash-object repository-path "commit"
                                      (sb-ext:string-to-octets (githack:serialize-commit commit) :external-format :utf-8)))
      (githack:git-update-ref repository-path branch-name (githack:sha commit))
      (githack:sha commit))))

(defun %demo-tree-entry-payload (repository-path tree filename)
  "Return the decoded PAYLOAD of TREE's own FILENAME entry, loading
TREE's ENTRIES first if necessary."
  (%demo-decode-blob (cdr (assoc filename (githack::get-entries (githack::%ensure-tree-entries-loaded repository-path tree))
                                 :test #'string=))))

;;; ------------------------------------------------------------------
;;; SECTION 2: :RETRY under a real, deterministic race
;;;
;;; RECEIVER, on its first two attempts, forces a real racer commit
;;; onto the branch (via %DEMO-HIJACK-ORPHAN!) before returning its
;;; own candidate value; only its third attempt is allowed to land
;;; unopposed. :RETRY must therefore invoke RECEIVER exactly three
;;; times, and the final committed value must reflect the THIRD
;;; attempt's own read of the SECOND racer's value (300), not either
;;; of the first two, discarded attempts.
;;; ------------------------------------------------------------------

(defun demo-retry-under-a-real-race ()
  (%demo-header "CONFLICT-RESOLUTION :RETRY: a real, deterministic compare-and-swap race")
  (let ((repository-path (%demo-temporary-bare-repository)))
    (unwind-protect
        (let ((repository (make-instance 'githack:git-repository
                                          :pathname repository-path
                                          :branch "main"
                                          :author +demo-racer-signature+
                                          :committer +demo-racer-signature+
                                          :message "initial"
                                          :mode :read-write)))
          (githack:call-with-git-transaction
           repository :read-write
           :receiver (lambda (tx head) (declare (ignore tx head))
                       (make-instance 'githack:git-blob :repository repository-path :payload 1)))
          (let ((attempts 0))
            (githack:call-with-git-transaction
             repository :read-write
             :conflict-resolution :retry
             :receiver (lambda (tx head)
                         (declare (ignore tx))
                         (incf attempts)
                         (case attempts
                           (1 (%demo-hijack-orphan! repository-path "main" 100))
                           (2 (%demo-hijack-orphan! repository-path "main" 300)))
                         (make-instance 'githack:git-blob :repository repository-path
                                        :payload (1+ (%demo-decode-blob (githack:resolve-commit-root head))))))
            (let* ((final-commit-sha (githack:git-show-ref-sha repository-path "main"))
                   (final-commit (make-instance 'githack:git-commit :repository repository-path :sha final-commit-sha))
                   (final-value (%demo-decode-blob (githack:resolve-commit-root final-commit))))
              (%demo-line "RECEIVER ran ~D times (expected 3: the original attempt plus one retry per racer)." attempts)
              (%demo-line "Final committed value: ~D (expected 301 = second racer's 300 + 1)." final-value)
              (%demo-line "~:[FAIL~;PASS~]: :RETRY re-ran RECEIVER exactly as many times as real races occurred, with no lost update."
                          (and (= attempts 3) (= final-value 301))))))
      (%demo-delete-temporary-repository repository-path))))

;;; ------------------------------------------------------------------
;;; SECTION 3: :REBASE, both the fast clean-merge path and its
;;; REBASE-FALLBACK
;;;
;;; Part A: the racer and RECEIVER modify two DIFFERENT entries of the
;;; same tree ("left" vs. "right"): `git merge-tree` can always merge
;;; that cleanly, so :REBASE must commit successfully on RECEIVER's
;;; very first (and only) attempt, with the final tree reflecting
;;; BOTH edits at once.
;;;
;;; Part B: the racer and RECEIVER both modify the exact SAME entry
;;; ("shared"): no clean merge is possible, so :REBASE must fall back,
;;; per its own REBASE-FALLBACK :RETRY, to discarding RECEIVER's
;;; candidate and re-running it from scratch against the racer's own
;;; new HEAD -- proving the fallback path, not just the fast path.
;;; ------------------------------------------------------------------

(defun demo-rebase-clean-merge ()
  (%demo-header "CONFLICT-RESOLUTION :REBASE, part A: a real race that merges cleanly")
  (let ((repository-path (%demo-temporary-bare-repository)))
    (unwind-protect
        (let ((repository (make-instance 'githack:git-repository
                                          :pathname repository-path
                                          :branch "main"
                                          :author +demo-racer-signature+
                                          :committer +demo-racer-signature+
                                          :message "initial"
                                          :mode :read-write)))
          (githack:call-with-git-transaction
           repository :read-write
           :receiver (lambda (tx head) (declare (ignore tx head))
                       (make-instance 'githack:git-tree :repository repository-path
                                      :entries (list (cons "left" (make-instance 'githack:git-blob
                                                                                 :repository repository-path :payload "left-0"))
                                                      (cons "right" (make-instance 'githack:git-blob
                                                                                  :repository repository-path :payload "right-0"))))))
          (let ((original-head-sha (githack:git-show-ref-sha repository-path "main"))
                (attempts 0))
            (githack:call-with-git-transaction
             repository :read-write
             :conflict-resolution :rebase
             :receiver (lambda (tx head) (declare (ignore tx head))
                         (incf attempts)
                         (when (= attempts 1)
                           (%demo-hijack-with-tree! repository-path "main" original-head-sha
                                                     (list (cons "left" "left-0") (cons "right" "right-1-from-racer"))))
                         (make-instance 'githack:git-tree :repository repository-path
                                        :entries (list (cons "left" (make-instance 'githack:git-blob
                                                                                   :repository repository-path :payload "left-1-from-us"))
                                                        (cons "right" (make-instance 'githack:git-blob
                                                                                    :repository repository-path :payload "right-0"))))))
            (let* ((final-commit-sha (githack:git-show-ref-sha repository-path "main"))
                   (final-commit (make-instance 'githack:git-commit :repository repository-path :sha final-commit-sha))
                   (final-tree (githack:resolve-commit-root final-commit))
                   (left (%demo-tree-entry-payload repository-path final-tree "left"))
                   (right (%demo-tree-entry-payload repository-path final-tree "right")))
              (%demo-line "RECEIVER ran ~D time(s) (expected 1: the racer touched a different entry, so no fallback was needed)." attempts)
              (%demo-line "Final tree: left=~S, right=~S (expected our own \"left-1-from-us\" AND the racer's own \"right-1-from-racer\")."
                          left right)
              (%demo-line "~:[FAIL~;PASS~]: :REBASE auto-merged the real race with no RECEIVER re-run at all."
                          (and (= attempts 1) (string= left "left-1-from-us") (string= right "right-1-from-racer"))))))
      (%demo-delete-temporary-repository repository-path))))

(defun demo-rebase-conflict-falls-back-to-retry ()
  (%demo-header "CONFLICT-RESOLUTION :REBASE, part B: a real, unresolvable conflict falls back to :RETRY")
  (let ((repository-path (%demo-temporary-bare-repository)))
    (unwind-protect
        (let ((repository (make-instance 'githack:git-repository
                                          :pathname repository-path
                                          :branch "main"
                                          :author +demo-racer-signature+
                                          :committer +demo-racer-signature+
                                          :message "initial"
                                          :mode :read-write)))
          (githack:call-with-git-transaction
           repository :read-write
           :receiver (lambda (tx head) (declare (ignore tx head))
                       (make-instance 'githack:git-tree :repository repository-path
                                      :entries (list (cons "shared" (make-instance 'githack:git-blob
                                                                                   :repository repository-path :payload "shared-0"))))))
          (let ((original-head-sha (githack:git-show-ref-sha repository-path "main"))
                (attempts 0))
            (githack:call-with-git-transaction
             repository :read-write
             :conflict-resolution :rebase
             :rebase-fallback :retry
             :receiver (lambda (tx head)
                         (declare (ignore tx))
                         (incf attempts)
                         (when (= attempts 1)
                           (%demo-hijack-with-tree! repository-path "main" original-head-sha
                                                     (list (cons "shared" "shared-1-from-racer"))))
                         (let ((seen (%demo-tree-entry-payload repository-path (githack:resolve-commit-root head) "shared")))
                           (make-instance 'githack:git-tree :repository repository-path
                                          :entries (list (cons "shared"
                                                                (make-instance 'githack:git-blob
                                                                               :repository repository-path
                                                                               :payload (format nil "shared-1-from-us (attempt ~D, saw ~S)"
                                                                                                attempts seen))))))))
            (let* ((final-commit-sha (githack:git-show-ref-sha repository-path "main"))
                   (final-commit (make-instance 'githack:git-commit :repository repository-path :sha final-commit-sha))
                   (final-tree (githack:resolve-commit-root final-commit))
                   (shared (%demo-tree-entry-payload repository-path final-tree "shared")))
              (%demo-line "RECEIVER ran ~D time(s) (expected 2: the genuine conflict forced REBASE-FALLBACK :RETRY once)." attempts)
              (%demo-line "Final \"shared\" entry: ~S (expected to mention attempt 2, having seen the racer's own value first)." shared)
              (%demo-line "~:[FAIL~;PASS~]: :REBASE correctly detected an unresolvable conflict and fell back to :RETRY."
                          (and (= attempts 2) (search "attempt 2" shared) (search "shared-1-from-racer" shared))))))
      (%demo-delete-temporary-repository repository-path))))

;;; ------------------------------------------------------------------
;;; SECTION 4: :LOCK under real concurrency
;;;
;;; SETTLE-INTEREST touches every account in the bank at once, so it
;;; defaults to :LOCK. Two real concurrent threads both calling
;;; SETTLE-INTEREST prove true mutual exclusion.
;;;
;;; NOTE ON HOW MUTUAL EXCLUSION IS PROVEN HERE: an earlier version of
;;; this demo compared each thread's own wall-clock start/end interval
;;; and flagged an "overlap" as a failure. That measurement is
;;; unsound: a *correctly working* lock still produces overlapping
;;; start/end spans whenever one thread blocks waiting for the other
;;; to finish, because the blocked thread's own start time (recorded
;;; before it ever tries to acquire the lock) can fall inside the
;;; time the other thread is actually holding the lock -- that is
;;; exactly what the lock is supposed to make happen, not a bug. (This
;;; was confirmed directly: isolated stress tests of
;;; WITH-REPOSITORY-TRANSACTION-LOCK across dozens of acquisitions
;;; from two real threads never once let two holders overlap, even
;;; though naively-recorded thread lifetimes did.)
;;;
;;; Instead, this section proves mutual exclusion the way it actually
;;; matters for correctness: via the classic "LOST UPDATE" problem.
;;; SETTLE-INTEREST reads every account's current balance and then
;;; writes back balance * (1 + RATE). If two concurrent settlements on
;;; the same accounts were not genuinely serialized -- e.g. if :LOCK
;;; were a no-op -- then whichever thread's read happened first would
;;; have its write silently clobbered by the other thread's write
;;; (both compute their new balance from the *same* stale starting
;;; balance, so only one rate application would ever survive). With a
;;; working lock, each call sees the other's fully-committed result
;;; before it starts, so the RATE compounds twice: final balance must
;;; equal (initial-balance * (1 + RATE) * (1 + RATE)) exactly, for
;;; every account in the bank. Safe to use real OS threads here
;;; (unlike SECTIONS 2/3 above) precisely because :LOCK guarantees the
;;; two threads' own Git I/O never actually overlaps -- see this
;;; file's own header comment.
;;; ------------------------------------------------------------------

(defun demo-lock-mutual-exclusion ()
  (%demo-header "CONFLICT-RESOLUTION :LOCK: two real threads, both settling interest, proven via lost-update detection")
  (ensure-bank-initialized)
  (handler-case (open-account "alice" "Alice Anderson" 10000) (error () nil))
  (handler-case (open-account "bob" "Bob Brown" 5000) (error () nil))
  (handler-case (open-account "carol" "Carol Chen" 7500) (error () nil))
  (let* ((rate 1/100)
         (before (mapcar (lambda (id) (cons id (get-balance id))) '("alice" "bob" "carol"))))
    (%demo-line "Balances before either settlement: ~S" before)
    (let ((thread-a (sb-thread:make-thread (lambda () (settle-interest rate)) :name "demo-lock-settle-a"))
          (thread-b (sb-thread:make-thread (lambda () (settle-interest rate)) :name "demo-lock-settle-b")))
      (sb-thread:join-thread thread-a)
      (sb-thread:join-thread thread-b))
    (let* ((after (mapcar (lambda (id) (cons id (get-balance id))) '("alice" "bob" "carol")))
           ;; SETTLE-INTEREST computes each account's interest as
           ;; (ROUND (* RATE BALANCE)) and adds it via DEPOSIT (see
           ;; BANK.LISP), so the expected value after two settlements
           ;; must apply that same rounding twice, not compound RATE
           ;; as an exact rational.
           (expected (mapcar (lambda (entry)
                               (let ((balance (cdr entry)))
                                 (dotimes (i 2)
                                   (incf balance (round (* rate balance))))
                                 (cons (car entry) balance)))
                             before))
           (ok? (every (lambda (a e) (and (equal (car a) (car e)) (= (cdr a) (cdr e))))
                       after expected)))
      (%demo-line "Balances after both settlements:   ~S" after)
      (%demo-line "Expected (both settlements applied, no lost update): ~S" expected)
      (%demo-line "~:[FAIL~;PASS~]: both concurrent :LOCK-mode SETTLE-INTEREST calls were genuinely serialized (no lost update)."
                  ok?))))

(defun run-concurrency-demo ()
  "Run every section of this file in turn. SECTIONS 1 and 4 act
against the real, self-hosting \"database-example-bank\" branch and
are safe to call repeatedly -- every account is opened idempotently
(see the HANDLER-CASE wrapping each OPEN-ACCOUNT call), and each
computes its own expected result relative to whatever balance it
observes beforehand, rather than assuming any particular starting
balance. SECTIONS 2 and 3 each create and destroy their own disposable
scratch repository, so they never touch the bank's own history at
all."
  (demo-nested-transactions)
  (demo-retry-under-a-real-race)
  (demo-rebase-clean-merge)
  (demo-rebase-conflict-falls-back-to-retry)
  (demo-lock-mutual-exclusion)
  (%demo-header "Done. Inspect the real bank commit history with: git log database-example-bank")
  (values))

(run-concurrency-demo)
