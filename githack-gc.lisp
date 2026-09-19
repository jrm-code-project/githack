;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; RUN-GITHACK-GC! is a dedicated garbage-collection utility for a
;;; long-running GitHack deployment: it (1) sweeps every stranded
;;; `refs/githack/prepare/<tx-id>/<branch-name>` ref left behind by a
;;; coordinator crash (via RUN-GITHACK-EXORCIST! -- see distributed-
;;; transaction.lisp), (2) reconciles now-unneeded Transaction
;;; Manifests by deleting any `refs/githack/ledger/<tx-id>` ref no
;;; stranded prepare ref anywhere could still possibly consult, and
;;; (3) triggers `git gc` to actually reclaim the now-fully-
;;; unreferenced commits, tags, and blobs those first two steps (and
;;; any ordinary aborted GIT-TRANSACTION) leave behind as loose
;;; objects.
;;;
;;; STEP 3 NEVER STALLS CONCURRENT READERS: an ordinary :READ-ONLY
;;; GIT-TRANSACTION takes no lock of any kind (see git-transaction.
;;; lisp's own CONCURRENCY POLICY commentary) and `git gc` is itself
;;; designed to run safely alongside concurrent readers -- it builds
;;; its new packfile(s) first and only atomically swaps them in via
;;; ordinary ref-safe rename(2)/replace semantics, exactly as `git
;;; gc` running against any other Git repository with concurrent
;;; readers always has. RUN-GITHACK-GC! itself never acquires
;;; WITH-REPOSITORY-TRANSACTION-LOCK either, for steps 1, 2, OR 3: an
;;; in-flight :LOCK-mode :READ-WRITE transaction could only ever
;;; contend, briefly, with this same lock a competing :LOCK-mode
;;; writer would already contend with, and never with `git gc`
;;; itself, which touches no GitHack lock file at all.

(defun ref-suffix-after-prefix (ref-path prefix)
  "Return the portion of REF-PATH (e.g. a `refs/githack/prepare/...`
or `refs/githack/ledger/...` ref) following PREFIX, or NIL if
REF-PATH does not begin with PREFIX."
  (and (<= (length prefix) (length ref-path))
       (string= prefix ref-path :end2 (length prefix))
       (subseq ref-path (length prefix))))

(defun ledger-tx-id-from-ref (ref-path)
  "Return the TX-ID string encoded in REF-PATH (a
`refs/githack/ledger/<tx-id>` ref path), or NIL if REF-PATH is not
shaped like one."
  (ref-suffix-after-prefix ref-path "refs/githack/ledger/"))

(defun prepare-tx-id-from-ref (ref-path)
  "Return the TX-ID string encoded in REF-PATH (a
`refs/githack/prepare/<tx-id>/<branch-name>` ref path), or NIL if
REF-PATH is not shaped like one."
  (let ((suffix (ref-suffix-after-prefix ref-path "refs/githack/prepare/")))
    (and suffix (subseq suffix 0 (position #\/ suffix)))))

(defun tx-ids-with-ledger-refs (repository)
  "Return a fresh list of every TX-ID string REPOSITORY currently
holds a `refs/githack/ledger/<tx-id>` ref for."
  (mapcar (lambda (entry) (ledger-tx-id-from-ref (third entry)))
          (%git-for-each-ref repository "refs/githack/ledger/")))

(defun tx-ids-with-stranded-prepare-refs (repositories)
  "Return a fresh, duplicate-free list of every TX-ID string any
repository in REPOSITORIES (a list of Git directory pathnames)
currently still holds at least one `refs/githack/prepare/<tx-id>/
<branch-name>` ref for."
  (delete-duplicates
   (mapcan (lambda (repository)
             (mapcar (lambda (entry) (prepare-tx-id-from-ref (third entry)))
                     (%git-for-each-ref repository "refs/githack/prepare/")))
           repositories)
   :test #'string=))

(defun sweep-stranded-prepare-refs! (participant-repositories)
  "Run RUN-GITHACK-EXORCIST! against every repository in PARTICIPANT-
REPOSITORIES in turn, tolerating (rather than aborting the whole
sweep over) a single repository's own failure: returns a fresh list
of (REPOSITORY . OUTCOME) pairs, one per PARTICIPANT-REPOSITORIES
entry, OUTCOME being either RUN-GITHACK-EXORCIST!'s own return value
(a list of (TX-ID BRANCH-NAME ACTION)) or, if it signaled an error
resolving that one repository's own stranded refs, the CONDITION
itself -- so one unreachable or corrupt participant can never
prevent RUN-GITHACK-GC! from still sweeping, reconciling, and
repacking every other repository it was given."
  (mapcar (lambda (repository)
            (cons repository
                  (handler-case (run-githack-exorcist! repository)
                    (error (condition) condition))))
          participant-repositories))

(defun reconcile-ledger-refs! (repository participant-repositories dry-run)
  "Delete every now-unneeded `refs/githack/ledger/<tx-id>` ref in
REPOSITORY: one whose TX-ID no longer appears in any `refs/githack/
prepare/<tx-id>/...` ref across PARTICIPANT-REPOSITORIES (which
SWEEP-STRANDED-PREPARE-REFS! should already have been run against,
immediately before this, so that set reflects only genuinely
unresolvable-right-now strandings, e.g. an unreachable participant).
If DRY-RUN is true, no ref is actually deleted -- this merely
computes and returns the same list RUN-GITHACK-GC! would otherwise
have deleted. Returns a fresh list of every TX-ID reconciled (i.e.
deleted, or that would have been under DRY-RUN).

SAFETY: this is only correct if PARTICIPANT-REPOSITORIES genuinely
lists every repository that ever was, or ever could be, a
participant of any distributed transaction whose elected Ledger is
REPOSITORY -- see RUN-GITHACK-GC!'s own docstring. Reconciling (i.e.
deleting) a Ledger ref that some OTHER, unlisted repository still
holds a stranded prepare ref referencing would strand that
participant permanently: a later RUN-GITHACK-EXORCIST! pass over it
would find no Ledger ref, conclude the transaction was never
committed, and roll it back even if it had, in fact, already reached
its Point of No Return."
  (let* ((ledger-tx-ids (tx-ids-with-ledger-refs repository))
         (still-stranded (tx-ids-with-stranded-prepare-refs participant-repositories))
         (reconcilable (remove nil ledger-tx-ids
                                :key (lambda (tx-id) (member tx-id still-stranded :test #'string=))
                                :test-not #'eq)))
    (unless dry-run
      (dolist (tx-id reconcilable)
        (%git-raw-delete-ref! repository (ledger-ref-path tx-id))))
    reconcilable))

(defun repack-repository! (repository prune-expire aggressive quiet)
  "Shell out to `git gc --prune=PRUNE-EXPIRE [--aggressive] [--quiet]`
against REPOSITORY, reclaiming every loose object no ref (or other
loose object reachable from one) still references -- exactly the
commits, tags, and blobs an aborted GIT-TRANSACTION, a rolled-back
Two-Phase-Commit participant, or RECONCILE-LEDGER-REFS!'s own
deleted Ledger refs leave behind, none of which any GitHack code
ever deletes directly (see git-transaction.lisp's own CONCURRENCY
POLICY commentary: rollback is defined as \"just never make it
reachable,\" not \"delete it\"). Returns (VALUES OUTPUT EXIT-CODE).
Signals GARBAGE-COLLECTION-ERROR if `git gc` itself exits non-zero."
  (multiple-value-bind (output error-output exit-code)
      (%git-run repository (append (list "gc" (format nil "--prune=~A" prune-expire))
                                    (and aggressive (list "--aggressive"))
                                    (and quiet (list "--quiet"))))
    (unless (zerop exit-code)
      (error 'garbage-collection-error :repository repository :detail error-output))
    (values output exit-code)))

(defun run-githack-gc! (repository &key (participant-repositories (list repository))
                                        (repack t)
                                        (prune-expire "now")
                                        (aggressive nil)
                                        (quiet t)
                                        (dry-run nil))
  "Run GitHack's own dedicated garbage-collection pass against
REPOSITORY (a pathname naming a Git directory), in three steps:

  1. SWEEP: RUN-GITHACK-EXORCIST! is run against every repository in
     PARTICIPANT-REPOSITORIES (defaulting to the single-element list
     (LIST REPOSITORY)), resolving every stranded `refs/githack/
     prepare/<tx-id>/<branch-name>` ref found -- rolling it forward
     or back exactly as it always does -- tolerantly: one
     unreachable or corrupt participant cannot prevent the others
     from being swept, or prevent steps 2/3 below from still running
     against REPOSITORY itself.

  2. RECONCILE: every `refs/githack/ledger/<tx-id>` ref remaining in
     REPOSITORY whose TX-ID no longer appears in any stranded
     prepare ref across PARTICIPANT-REPOSITORIES (post-sweep) is
     deleted -- it can never again be consulted by any future
     RUN-GITHACK-EXORCIST! call, so keeping it around forever (as
     ordinary Two-Phase-Commit tests document Ledger refs otherwise
     doing) serves no purpose but ref-namespace clutter.

  3. REPACK (unless REPACK is NIL): `git gc --prune=PRUNE-EXPIRE`
     (plus `--aggressive` if AGGRESSIVE, `--quiet` if QUIET, the
     default) is run against REPOSITORY, physically reclaiming every
     object steps 1/2 (or any ordinary aborted GIT-TRANSACTION, ever
     since REPOSITORY's very first commit) left as unreferenced
     loose Git garbage. This step never stalls a concurrent
     :READ-ONLY transaction -- see this file's own top-of-file
     commentary -- and, like Git's own `gc.pruneExpire` default,
     PRUNE-EXPIRE defaults to \"now\" rather than Git's usual 2-week
     grace window, since GitHack's own object lifecycle already
     guarantees an unreferenced object was never reachable from any
     ref in the first place (no concurrently-running GIT-TRANSACTION
     could be relying on it staying around a little longer -- it was
     never linked in to begin with).

If DRY-RUN is true, steps 1 and 3 do not run at all (sweeping and
repacking are not idempotent-to-preview operations -- inspect this
function's own TX-IDS-WITH-STRANDED-PREPARE-REFS/RUN-GITHACK-
EXORCIST directly first if a genuinely read-only preview of step 1
matters) and step 2 only computes, without deleting, the Ledger refs
it would otherwise have reconciled.

Returns a plist: :SWEPT (one (REPOSITORY . OUTCOME) pair per
PARTICIPANT-REPOSITORIES entry, as SWEEP-STRANDED-PREPARE-REFS!
returns), :RECONCILED (the list of TX-IDs whose Ledger ref was
deleted, or would have been under DRY-RUN), and :REPACKED (T if step
3 actually ran, NIL if REPACK or DRY-RUN suppressed it).

SAFETY: PARTICIPANT-REPOSITORIES must genuinely enumerate every
repository that ever was, or ever could be, a participant of any
distributed transaction whose elected Ledger is REPOSITORY, or step
2 can incorrectly reconcile away a Ledger ref some other, unlisted
repository's own stranded prepare ref still needs -- see
RECONCILE-LEDGER-REFS!'s own docstring for the precise failure
mode. The default, (LIST REPOSITORY) alone, is only safe when
REPOSITORY is itself the sole possible participant of any of its own
distributed transactions (e.g. every WITH-GITHACK-TRANSACTION body
in this deployment ever only touches this one repository, so the
Two-Phase-Commit \">1 participant\" path, and hence any OTHER
repository's prepare ref, never arises for it)."
  (let* ((swept (unless dry-run (sweep-stranded-prepare-refs! participant-repositories)))
         (reconciled (reconcile-ledger-refs! repository participant-repositories dry-run))
         (repacked (and repack (not dry-run))))
    (when repacked
      (repack-repository! repository prune-expire aggressive quiet))
    (list :swept swept :reconciled reconciled :repacked repacked)))
