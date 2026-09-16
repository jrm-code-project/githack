;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests for GitHack's dedicated garbage-collection
;;; utility, RUN-GITHACK-GC! (githack-gc.lisp): sweeping stranded
;;; `refs/githack/prepare/...` refs, reconciling now-unneeded
;;; `refs/githack/ledger/<tx-id>` refs, and triggering a real `git
;;; gc`. Like DISTRIBUTED-TRANSACTION-SUITE, every test here
;;; genuinely shells out to real `git` executables against real,
;;; temporary bare repositories.

(def-suite githack-gc-suite
  :in githack-suite
  :description "End-to-end tests for RUN-GITHACK-GC!'s sweep/reconcile/repack pipeline, against real temporary bare Git repositories.")

(in-suite githack-gc-suite)

(test run-githack-gc-rolls-back-a-stranded-prepare-ref-with-no-ledger
  "RUN-GITHACK-GC!'s own SWEEP step rolls back a stranded prepare ref
whose Ledger never received its commit-point marker (simulating a
crash between Phase 1 and Phase 2's own Point-of-No-Return), exactly
as RUN-GITHACK-EXORCIST! alone would, and its own REPACK step (left
at its default of T) still runs successfully to completion."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "never-committed"))
      (let* ((pw (first (%githack-transaction-pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw) (pending-write-git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text))
      (let ((report (run-githack-gc! repository)))
        (is (equal (list (list tx-id "main" :rolled-back))
                   (cdr (assoc repository (getf report :swept) :test #'equal))))
        (is (null (getf report :reconciled)))
        (is (eq t (getf report :repacked))))
      (is (null (git-show-ref-sha repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/"))))))

(test run-githack-gc-rolls-forward-and-reconciles-a-stranded-prepare-ref-with-a-written-ledger
  "RUN-GITHACK-GC!'s SWEEP step rolls a stranded prepare ref forward
when its own Ledger DID already receive its commit-point marker, and
its RECONCILE step then deletes that same now-unreferenced
`refs/githack/ledger/<tx-id>` ref in the very same pass, since the
sweep it just performed leaves no stranded prepare ref anywhere
still needing it."
  (with-temporary-git-repository (repository)
    (let* ((tx-id (generate-transaction-id))
           (txn (%make-githack-transaction tx-id)))
      (let ((*current-transaction* txn))
        (dtx-write! repository "main" "should-be-committed"))
      (let* ((pw (first (%githack-transaction-pending-writes txn)))
             (manifest-text (format-transaction-manifest
                              (build-transaction-manifest tx-id (list pw) (pending-write-git-repository pw)))))
        (%prepare-participant! pw tx-id manifest-text)
        (%write-ledger-commit-point! (pending-write-git-repository pw) tx-id))
      (let ((report (run-githack-gc! repository :repack nil)))
        (is (equal (list (list tx-id "main" :committed))
                   (cdr (assoc repository (getf report :swept) :test #'equal))))
        (is (equal (list tx-id) (getf report :reconciled)))
        (is (null (getf report :repacked))))
      (is (equal "should-be-committed" (dtx-read repository "main")))
      (is (null (%git-for-each-ref repository "refs/githack/prepare/")))
      (is (null (%git-for-each-ref repository "refs/githack/ledger/"))))))

(test run-githack-gc-reconciles-a-ledger-ref-with-no-remaining-stranded-prepare-ref
  "A `refs/githack/ledger/<tx-id>` ref with no corresponding stranded
prepare ref anywhere -- the ordinary steady-state left behind by a
Two-Phase-Commit transaction that fully completed its own Phase 2
Roll Forward loop without ever crashing -- is reconciled (deleted)
by RUN-GITHACK-GC! even though RUN-GITHACK-EXORCIST! itself has
nothing at all to sweep."
  (with-temporary-git-repository (repository)
    (let ((tx-id (generate-transaction-id)))
      (%write-ledger-commit-point! (make-instance 'git-repository :pathname repository) tx-id)
      (let ((report (run-githack-gc! repository :repack nil)))
        (is (null (cdr (assoc repository (getf report :swept) :test #'equal))))
        (is (equal (list tx-id) (getf report :reconciled))))
      (is (null (%git-for-each-ref repository "refs/githack/ledger/"))))))

(test run-githack-gc-does-not-reconcile-a-ledger-ref-whose-participant-could-not-be-swept
  "If a participant repository's own stranded prepare ref cannot be
swept at all (here, because its own annotated tag's Manifest is
corrupt and unreadable -- RUN-GITHACK-EXORCIST! itself signals
DISTRIBUTED-TRANSACTION-ERROR for it), RUN-GITHACK-GC!'s own RECONCILE
step must NOT delete the Ledger ref that stranded ref's own tx-id
still names: doing so would later make a genuinely-committed
transaction impossible to roll forward, should its Manifest ever
become readable again (e.g. after a manual repair)."
  (with-temporary-git-repository (ledger-repository)
    (with-temporary-git-repository (participant-repository)
      (let* ((tx-id (generate-transaction-id))
             (branch "main"))
        (%write-ledger-commit-point! (make-instance 'git-repository :pathname ledger-repository) tx-id)
        (dtx-write! participant-repository branch "seed")
        (let* ((commit-sha (git-show-ref-sha participant-repository branch))
               (tag-name (format nil "githack-prepare-~A-~A" tx-id branch))
               (tag-content (format-annotated-tag-content
                             commit-sha tag-name "GC Test <gc@githack.local>"
                             "(this is not valid readable Lisp"))
               (tag-sha (%git-mktag participant-repository tag-content)))
          (%git-raw-update-ref! participant-repository (prepare-ref-path tx-id branch) tag-sha :expected-sha nil))
        (let ((report (run-githack-gc! ledger-repository
                                       :participant-repositories (list ledger-repository participant-repository)
                                       :repack nil)))
          (is (typep (cdr (assoc participant-repository (getf report :swept) :test #'equal)) 'condition))
          (is (null (getf report :reconciled))))
        (is (equal (list tx-id) (tx-ids-with-ledger-refs ledger-repository)))
        (is (equal (list (prepare-ref-path tx-id branch))
                   (mapcar #'third (%git-for-each-ref participant-repository "refs/githack/prepare/"))))))))

(test run-githack-gc-dry-run-mutates-nothing
  "With :DRY-RUN T, RUN-GITHACK-GC! neither sweeps stranded prepare
refs nor deletes any Ledger ref nor runs `git gc` at all -- it only
reports, via :RECONCILED, which Ledger ref(s) it would otherwise
have reconciled."
  (with-temporary-git-repository (repository)
    (let ((tx-id (generate-transaction-id)))
      (%write-ledger-commit-point! (make-instance 'git-repository :pathname repository) tx-id)
      (let ((report (run-githack-gc! repository :dry-run t)))
        (is (null (getf report :swept)))
        (is (equal (list tx-id) (getf report :reconciled)))
        (is (null (getf report :repacked))))
      (is (equal (list tx-id) (tx-ids-with-ledger-refs repository))))))

(test run-githack-gc-is-a-no-op-for-a-repository-with-nothing-to-collect
  "RUN-GITHACK-GC! against an ordinary repository with no stranded
prepare refs and no Ledger refs at all sweeps and reconciles nothing,
but its REPACK step still runs (and succeeds) against the otherwise
untouched repository."
  (with-temporary-git-repository (repository)
    (with-githack-transaction ()
      (dtx-write! repository "main" "ordinary-value"))
    (let ((report (run-githack-gc! repository)))
      (is (null (cdr (assoc repository (getf report :swept) :test #'equal))))
      (is (null (getf report :reconciled)))
      (is (eq t (getf report :repacked))))
    (is (equal "ordinary-value" (dtx-read repository "main")))))
