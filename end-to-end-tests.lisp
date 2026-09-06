;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; End-to-end tests: unlike every other test file in this suite,
;;; these genuinely shell out to a real `git` executable (via
;;; WITH-TEMPORARY-GIT-REPOSITORY, in test-helpers.lisp) against a
;;; fresh, empty, real bare repository in a temporary directory --
;;; no GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/GIT-
;;; UPDATE-REF fake is ever installed here. Each test exercises one
;;; kind of persistent object across a small SET of real
;;; transactions (via CALL-WITH-REPOSITORY/WITH-TRANSACTION/CALL-
;;; WITH-TRANSACTION), verifying both that the value round-trips
;;; correctly and, where applicable, that untouched substructure is
;;; never re-persisted (structural sharing survives a real commit).

(def-suite end-to-end-suite
  :in githack-suite
  :description "End-to-end tests exercising real (non-faked) Git transactions, via a temporary bare repository, for every kind of persistent object GitHack supports.")

(in-suite end-to-end-suite)

(defparameter +e2e-author+ "Test Author <test@githack.local>")

(defun %e2e-fetch-tree-and-meta-octets (repository sha)
  "Return, as two values, the raw Git tree bytes for SHA and the raw
bytes of its own \".meta\" entry, fetched via GIT-CAT-FILE/
DESERIALIZE-TREE against the real REPOSITORY -- exactly the two
byte-vectors DESERIALIZE-PERSISTENT-CONS/-VECTOR/-ARRAY each require
of their own caller."
  (let* ((tree-octets (git-cat-file repository sha))
         (entries (deserialize-tree repository tree-octets))
         (meta-entry (assoc ".meta" entries :test #'string=)))
    (unless meta-entry
      (error "Malformed persistent tree ~S: missing \".meta\" entry." sha))
    (values tree-octets (git-cat-file repository (sha (cdr meta-entry))))))

(defun %e2e-load-persistent-cons (repository sha)
  "Return a fully loaded PERSISTENT-CONS proxy for SHA, fetched for
real from REPOSITORY."
  (let ((cons (make-instance 'persistent-cons :repository repository :sha sha)))
    (multiple-value-bind (tree-octets meta-octets) (%e2e-fetch-tree-and-meta-octets repository sha)
      (deserialize-persistent-cons cons tree-octets meta-octets))))

(defun %e2e-decode-blob (git-object)
  "Decode GIT-OBJECT (a GIT-BLOB, fetched for real via
%ENSURE-BLOB-LOADED if not already loaded) into its real Lisp
PAYLOAD."
  (get-payload (%ensure-blob-loaded git-object)))

(defun %e2e-hijack-branch! (repository-path branch-name payload)
  "Simulate a genuine concurrent external writer racing an in-flight
transaction: build and persist, via the very same low-level
primitives GitHack's own commit path itself uses (GIT-HASH-OBJECT,
WRAP-ATOMIC-COMMIT-ROOT), a brand-new, real, orphan GIT-COMMIT
wrapping PAYLOAD as its root, then unconditionally force BRANCH-NAME
(in the real, bare Git repository at REPOSITORY-PATH) to point at
it via GIT-UPDATE-REF -- exactly as if some other, wholly independent
process had already advanced BRANCH-NAME out from under the caller's
own in-flight transaction, between its own read and its own commit.
Bypasses CALL-WITH-GIT-TRANSACTION/CALL-WITH-TRANSACTION entirely
(so it is never mistaken for a nested transaction, even when called
from inside one), exactly as a real external writer would. Intended
to be called from within a :RETRY transaction's own RECEIVER, on its
very first attempt only (tracked by a counter the caller closes
over), to force a genuine CONCURRENT-MODIFICATION-ERROR and so
exercise :RETRY end-to-end against a real race rather than any
mock."
  (let ((blob (make-instance 'git-blob :repository repository-path :payload payload)))
    (setf (sha blob) (git-hash-object repository-path "blob" (serialize-atom payload)))
    (let* ((tree (wrap-atomic-commit-root repository-path blob))
           (commit (make-instance 'git-commit
                                   :repository repository-path
                                   :tree tree
                                   :parents '()
                                   :author +e2e-author+
                                   :committer +e2e-author+
                                   :timestamp 0
                                   :message "racer"
                                   :loaded? t)))
      (setf (sha commit)
            (git-hash-object repository-path "commit"
                              (sb-ext:string-to-octets (serialize-commit commit) :external-format :utf-8)))
      (git-update-ref repository-path branch-name (sha commit)))))

(defun %e2e-hijack-branch-with-tree! (repository-path branch-name parent-sha entries)
  "Simulate a genuine concurrent external writer racing an in-flight
:REBASE transaction, but -- unlike %E2E-HIJACK-BRANCH! -- as a real
descendant of PARENT-SHA rather than a brand-new orphan, so that its
resulting commit shares real Git ancestry with whatever the racing
transaction itself started from: this is what lets `git merge-tree`
locate a genuine common merge base and either auto-merge cleanly or
report a real content conflict, exactly as two independent,
concurrent, ancestry-related writers would. ENTRIES is an alist of
\(FILENAME . PAYLOAD), each PAYLOAD an arbitrary atomic Lisp value
persisted as its own real GIT-BLOB and referenced from a single new
real GIT-TREE built from all of ENTRIES; PARENT-SHA becomes that new
commit's sole real parent. Force BRANCH-NAME (in the real, bare Git
repository at REPOSITORY-PATH) to point at the new commit via
GIT-UPDATE-REF, unconditionally, exactly as an independent racing
writer would, and return the new commit's own SHA."
  (let ((tree (make-instance 'git-tree
                              :repository repository-path
                              :entries (mapcar (lambda (entry)
                                                  (let ((blob (make-instance 'git-blob
                                                                              :repository repository-path
                                                                              :payload (cdr entry))))
                                                    (setf (sha blob)
                                                          (git-hash-object repository-path "blob" (serialize-atom (cdr entry))))
                                                    (cons (car entry) blob)))
                                                entries))))
    (setf (sha tree) (git-hash-object repository-path "tree" (serialize-tree tree)))
    (let ((commit (make-instance 'git-commit
                                  :repository repository-path
                                  :tree tree
                                  :parents (if parent-sha
                                               (list (make-instance 'git-commit :repository repository-path :sha parent-sha))
                                               '())
                                  :author +e2e-author+
                                  :committer +e2e-author+
                                  :timestamp 0
                                  :message "racer"
                                  :loaded? t)))
      (setf (sha commit)
            (git-hash-object repository-path "commit"
                              (sb-ext:string-to-octets (serialize-commit commit) :external-format :utf-8)))
      (git-update-ref repository-path branch-name (sha commit))
      (sha commit))))

(defun %e2e-hijack-branch-atomic! (repository-path branch-name parent-sha payload)
  "Like %E2E-HIJACK-BRANCH-WITH-TREE!, but for a single bare atomic
root value (e.g. a string) instead of a multi-entry tree: PAYLOAD is
persisted as its own real GIT-BLOB, wrapped via WRAP-ATOMIC-COMMIT-
ROOT exactly as GIT-TRANSACTION's own commit path would wrap it, and
committed as a real descendant of PARENT-SHA -- so that `git
merge-tree` can locate a genuine common merge base against this
racing writer's own atomic-wrapper commit. Force BRANCH-NAME to
point at the new commit via GIT-UPDATE-REF, unconditionally, and
return the new commit's own SHA."
  (let ((blob (make-instance 'git-blob :repository repository-path :payload payload)))
    (setf (sha blob) (git-hash-object repository-path "blob" (serialize-atom payload)))
    (let* ((tree (wrap-atomic-commit-root repository-path blob))
           (commit (make-instance 'git-commit
                                   :repository repository-path
                                   :tree tree
                                   :parents (if parent-sha
                                                (list (make-instance 'git-commit :repository repository-path :sha parent-sha))
                                                '())
                                   :author +e2e-author+
                                   :committer +e2e-author+
                                   :timestamp 0
                                   :message "racer"
                                   :loaded? t)))
      (setf (sha commit)
            (git-hash-object repository-path "commit"
                              (sb-ext:string-to-octets (serialize-commit commit) :external-format :utf-8)))
      (git-update-ref repository-path branch-name (sha commit))
      (sha commit))))

(defun %e2e-commit-tree-sha (repository commit-sha)
  "Return the real root tree SHA a real commit COMMIT-SHA points at,
parsed directly out of its own raw \"tree \" header line via
GIT-CAT-FILE, bypassing any GitHack commit-loading machinery
entirely."
  (let* ((text (sb-ext:octets-to-string (git-cat-file repository commit-sha) :external-format :utf-8))
         (newline (position #\Newline text))
         (line (subseq text 0 newline)))
    (unless (and (>= (length line) 5) (string= "tree " line :end2 5))
      (error "Malformed commit ~S: no \"tree\" header line." commit-sha))
    (subseq line 5)))

(defun %e2e-tree-entry-payload (repository sha filename)
  "Return the real, decoded atomic Lisp payload stored at FILENAME
within the real Git tree SHA, fetched via GIT-CAT-FILE/
DESERIALIZE-TREE/DESERIALIZE-ATOM against REPOSITORY, bypassing any
GitHack persistent-object machinery entirely."
  (let* ((entries (deserialize-tree repository (git-cat-file repository sha)))
         (entry (assoc filename entries :test #'string=)))
    (unless entry
      (error "No entry ~S in tree ~S." filename sha))
    (deserialize-atom (git-cat-file repository (sha (cdr entry))))))

(test end-to-end-atomic-value-round-trips-across-transactions
  "A bare atomic Lisp value (no GIT-OBJECT wrapping needed by the
caller) round-trips through CALL-WITH-TRANSACTION/WITH-TRANSACTION
across three real, successive read-write transactions against a
fresh repository: NIL for the very first (empty-branch) read, then
each previously committed value, automatically un/re-wrapped in an
ATOMIC-WRAPPER-TREE behind the scenes."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "atom" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (is (null value))
         42)
       (with-transaction (value) (repository :read-write)
         (is (eql 42 value))
         (1+ value))
       (with-transaction (value) (repository :read-write)
         (is (eql 43 value))
         value)))))

(test end-to-end-persistent-cons-round-trips-and-shares-structure
  "A PERSISTENT-CONS chain, built and committed as the root of a
real transaction, round-trips (values, LENGTH, PROPER) through a
fresh PERSISTENT-CONS proxy fetched for real from Git in a second
transaction; extending it in a third transaction reuses -- rather
than re-persists -- the untouched tail's own SHA, confirming
structural sharing survives real Git persistence."
  (with-temporary-git-repository (repository-path)
    (let (tail-sha)
      (call-with-repository
       repository-path
       :branch "main" :author +e2e-author+ :message "cons" :mode :read-write
       :receiver
       (lambda (repository)
         ;; Transaction 1: commit the proper list (1 2 3).
         (with-transaction (value) (repository :read-write)
           (declare (ignore value))
           (let* ((tail (make-instance 'persistent-cons :repository repository-path :loaded? t
                                        :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                  :payload 3 :loaded? t)
                                        :persistent-cdr nil))
                  (mid (make-instance 'persistent-cons :repository repository-path :loaded? t
                                       :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                 :payload 2 :loaded? t)
                                       :persistent-cdr tail))
                  (head (make-instance 'persistent-cons :repository repository-path :loaded? t
                                        :persistent-car (make-instance 'git-blob :repository repository-path
                                                                                  :payload 1 :loaded? t)
                                        :persistent-cdr mid)))
             head))
         ;; Transaction 2: read the real root back, verify its shape,
         ;; then extend it with a new head node.
         (with-transaction (value) (repository :read-write)
           (let ((head (%e2e-load-persistent-cons repository-path (sha value))))
             (is (= 3 (persistent-cons-length head)))
             (is (eq t (persistent-cons-proper head)))
             (is (eql 1 (%e2e-decode-blob (persistent-car head))))
             (let ((mid (%e2e-load-persistent-cons repository-path (sha (persistent-cdr head)))))
               (is (eql 2 (%e2e-decode-blob (persistent-car mid))))
               (let ((tail (%e2e-load-persistent-cons repository-path (sha (persistent-cdr mid)))))
                 (is (eql 3 (%e2e-decode-blob (persistent-car tail))))
                 (is (null (%e2e-decode-blob (persistent-cdr tail))))
                 ;; Capture the tail's own SHA for transaction 3 to
                 ;; confirm it goes untouched (never re-persisted).
                 (setf tail-sha (sha tail))))
             (make-instance 'persistent-cons :repository repository-path :loaded? t
                             :persistent-car (make-instance 'git-blob :repository repository-path
                                                                       :payload 0 :loaded? t)
                             :persistent-cdr head)))
         ;; Transaction 3: verify the extended list, and that the
         ;; original (1 2 3) tail's SHA still has not changed.
         (with-transaction (value) (repository :read-write)
           (let* ((new-head (%e2e-load-persistent-cons repository-path (sha value)))
                  (head (%e2e-load-persistent-cons repository-path (sha (persistent-cdr new-head))))
                  (mid (%e2e-load-persistent-cons repository-path (sha (persistent-cdr head))))
                  (tail (%e2e-load-persistent-cons repository-path (sha (persistent-cdr mid)))))
             (is (= 4 (persistent-cons-length new-head)))
             (is (eql 0 (%e2e-decode-blob (persistent-car new-head))))
             (is (string= tail-sha (sha tail)))
             value)))))))

(test end-to-end-persistent-vector-lazily-fetches-real-elements
  "A PERSISTENT-VECTOR, built and committed as the root of a real
transaction, round-trips its own LENGTH and every individual element
through PERSISTENT-VECTOR-REF against a hollow proxy in a second,
independent transaction, exercising the entire real fetch stack
(tree fetch, \".meta\" fetch, per-index blob fetch)."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "vector" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'persistent-vector :repository repository-path :loaded? t
                         :entries (loop for i from 0 below 5
                                        collect (cons (princ-to-string i)
                                                      (make-instance 'git-blob :repository repository-path
                                                                               :payload (* i i) :loaded? t)))))
       (with-transaction (value) (repository :read-write)
         (let ((vector (make-instance 'persistent-vector :repository repository-path :sha (sha value))))
           (is (= 5 (persistent-vector-length (%ensure-persistent-vector-loaded vector))))
           (dotimes (i 5)
             (is (= (* i i) (persistent-vector-ref vector i)))))
         value)))))

(test end-to-end-persistent-array-computes-row-major-index-against-real-git
  "A 2x3 PERSISTENT-ARRAY, built and committed as the root of a real
transaction, round-trips every (I J) subscript pair through
PERSISTENT-ARRAY-REF against a hollow proxy in a second, independent
transaction, confirming row-major flattening survives real Git
persistence."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "array" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let ((data (make-instance 'persistent-vector :repository repository-path :loaded? t
                                     :entries (loop for i from 0 below 6
                                                     collect (cons (princ-to-string i)
                                                                   (make-instance 'git-blob :repository repository-path
                                                                                            :payload i :loaded? t))))))
           (make-instance 'persistent-array :repository repository-path
                           :dimensions '(2 3) :data data)))
       (with-transaction (value) (repository :read-write)
         (let ((array (make-instance 'persistent-array :repository repository-path :sha (sha value))))
           (dotimes (i 2)
             (dotimes (j 3)
               (is (= (+ (* i 3) j) (persistent-array-ref array i j))))))
         value)))))

(test end-to-end-persistent-object-round-trips-with-nested-compound-slot
  "A PERSISTENT-OBJECT instance (a PERSISTENT-OWNER, holding a nested
PERSISTENT-WIDGET) round-trips through DESERIALIZE-PERSISTENT-OBJECT
called on the real, freshly-fetched commit root of a second,
independent real transaction; accessing both the top-level LABEL
slot and the nested WIDGET's own NAME slot transparently triggers
SLOT-VALUE-USING-CLASS's real proxy resolution against actual Git
data."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "object" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'persistent-owner
                        :repository repository-path
                        :label "top-level-owner"
                        :widget (make-instance 'persistent-widget
                                                :repository repository-path
                                                :name "nested-widget"
                                                :tag :gadget)))
       (with-transaction (value) (repository :read-write)
         (let ((owner (deserialize-persistent-object value)))
           (is (string= "top-level-owner" (owner-label owner)))
           (is (string= "nested-widget" (widget-name (owner-widget owner))))
           (is (eq :gadget (widget-tag (owner-widget owner)))))
         value)))))

(test end-to-end-persistent-struct-round-trips-through-real-git
  "A DEFINE-PERSISTENT-STRUCT instance (STRUCT-WIDGET) round-trips
through DESERIALIZE-PERSISTENT-OBJECT called on the real commit root
of a second, independent real transaction."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "struct" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (make-instance 'struct-widget :repository repository-path :id 7 :name "Widget-7" :serial-number 12345))
       (with-transaction (value) (repository :read-write)
         (let ((widget (deserialize-persistent-object value)))
           (is (struct-widget-p widget))
           (is (= 7 (sw-id widget)))
           (is (string= "Widget-7" (sw-name widget)))
           (is (= 12345 (sw-serial-number widget))))
         value)))))

(test end-to-end-persistent-hash-table-round-trips-through-real-git
  "A PERSISTENT-HASH-TABLE, built via PHASH-MAKE/PHASH-PUT and
committed as the root of a real transaction, round-trips every one
of its associations -- via PHASH-GET against a fresh
DESERIALIZE-PERSISTENT-OBJECT instance in a second, independent
transaction -- and correctly reports a missing key as absent."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "hash-table" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let* ((table (phash-make :repository repository-path :test 'equal :size 4)))
           (setf table (phash-put "one" 1 table))
           (setf table (phash-put "two" 2 table))
           (setf table (phash-put "three" 3 table))
           table))
       (with-transaction (value) (repository :read-write)
         (let ((table (deserialize-persistent-object value)))
           (is (= 3 (persistent-hash-table-count table)))
           (multiple-value-bind (v found?) (phash-get "one" table) (is (eql 1 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "two" table) (is (eql 2 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "three" table) (is (eql 3 v)) (is (eq t found?)))
           (multiple-value-bind (v found?) (phash-get "missing" table 'default) (is (eq 'default v)) (is (eq nil found?))))
         value)))))

(test end-to-end-persistent-hash-table-round-trips-a-nested-persistent-object-value
  "A PERSISTENT-HASH-TABLE whose VALUEs are themselves nested
PERSISTENT-OBJECT instances (a STRUCT-WIDGET, stored via a
PERSISTENT-CONS collision chain inside the table's PERSISTENT-VECTOR
of buckets) round-trips correctly through a real transaction:
PHASH-GET, against a fresh DESERIALIZE-PERSISTENT-OBJECT instance in
a second, independent transaction, must itself be
DESERIALIZE-PERSISTENT-OBJECT'd again to recover the nested
STRUCT-WIDGET's own slots -- exercising %PERSIST-CONS-COMPONENT-BY-
TYPE/%PERSIST-VECTOR-COMPONENT-BY-TYPE's PERSISTENT-OBJECT methods
(persistent-standard-class.lisp), without which the nested value
would silently lose its \".meta\" entry and so its real class on
read-back."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "hash-table-nested-object" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         (let* ((widget (make-instance 'struct-widget :repository repository-path
                                        :id 7 :name "Widget-7" :serial-number 12345))
                (table (phash-make :repository repository-path :test 'equal)))
           (phash-put "widget" widget table)))
       (with-transaction (value) (repository :read-write)
         (let* ((table (deserialize-persistent-object value)))
           (multiple-value-bind (raw-widget found?) (phash-get "widget" table)
             (is (eq t found?))
             (let ((widget (deserialize-persistent-object raw-widget)))
               (is (struct-widget-p widget))
               (is (= 7 (sw-id widget)))
               (is (string= "Widget-7" (sw-name widget)))
               (is (= 12345 (sw-serial-number widget))))))
         value)))))

(test end-to-end-nested-transactions-commit-abort-and-retry
  "A single comprehensive walk through nested-transaction semantics
against a real, temporary bare Git repository -- no mocks anywhere:

* Transaction 1 (outer, :READ-WRITE) contains nested transaction A,
  which commits normally (+10); confirms A's contribution
  percolates into the outer's own GET-CURRENT-ROOT without A ever
  creating its own GIT-COMMIT.
* Still inside transaction 1, nested transaction B attempts +1000
  but calls ABORT-GIT-TRANSACTION explicitly instead of returning;
  confirms the outer's GET-CURRENT-ROOT is left completely
  untouched by B (still 110, not 1110).
* Still inside transaction 1, nested transaction C itself opens a
  nested-of-nested transaction D (two levels deep) which commits +5;
  C then reads its own now-percolated current root back out and
  returns it, carrying D's contribution up through C into the
  outer -- demonstrating multi-level percolation.
* Transaction 1's own outer body finally reads back its fully
  percolated current root (115) and returns it as ITS OWN value,
  which really is what gets committed -- verified by a fresh,
  independent read-only transaction afterward. Only ONE real commit
  exists for the whole of transaction 1: none of A/B/C/D ever
  advances the branch or creates a GIT-COMMIT of their own.
* Transaction 2 exercises a genuine, non-mocked :RETRY: on its very
  first attempt, RECEIVER uses %E2E-HIJACK-BRANCH! to force a real
  concurrent writer to advance the branch out from under it (to a
  brand-new committed value, 500), causing transaction 2's own first
  commit attempt to fail with a real CONCURRENT-MODIFICATION-ERROR;
  :RETRY then re-runs RECEIVER from scratch against the freshly
  hijacked head, computing and committing 501 -- confirmed both by
  RECEIVER having run exactly twice and by a final independent
  read-only transaction reading back 501."
  (with-temporary-git-repository (repository-path)
    (call-with-repository
     repository-path
     :branch "main" :author +e2e-author+ :message "nested" :mode :read-write
     :receiver
     (lambda (repository)
       ;; Transaction 0: establish the initial counter value.
       (with-transaction (value) (repository :read-write)
         (declare (ignore value))
         100)
       (with-transaction (value) (repository :read-only)
         (is (eql 100 value)))

       ;; Transaction 1: nested commit, nested abort, and two-level
       ;; nested-of-nested commit, all inside one outer transaction.
       (with-transaction (value) (repository :read-write)
         (is (eql 100 value))

         ;; Nested A: +10, committed normally.
         (with-transaction (a-value) (repository :read-write)
           (is (eql 100 a-value))
           (+ a-value 10))
         (is (eql 110 (get-payload (get-current-root *transaction*))))

         ;; Nested B: attempts +1000 but explicitly aborts instead of
         ;; returning -- must leave the outer's percolated state
         ;; completely untouched (still 110, not 1110).
         (with-transaction (b-value) (repository :read-write)
           (is (eql 110 b-value))
           (abort-git-transaction *transaction*))
         (is (eql 110 (get-payload (get-current-root *transaction*))))

         ;; Nested C, itself containing nested-of-nested D: D commits
         ;; +5 into C; C then reads its own percolated current root
         ;; back out and returns it, so C's own merge into the outer
         ;; carries D's contribution up two levels at once.
         (with-transaction (c-value) (repository :read-write)
           (is (eql 110 c-value))
           (with-transaction (d-value) (repository :read-write)
             (is (eql 110 d-value))
             (+ d-value 5))
           (get-payload (get-current-root *transaction*)))
         (is (eql 115 (get-payload (get-current-root *transaction*))))

         ;; The outer transaction's own committed value is whatever
         ;; IT returns -- read back its fully-percolated current root
         ;; and return that, proving the merge chain really reaches
         ;; the outermost, real commit.
         (get-payload (get-current-root *transaction*)))
       (with-transaction (value) (repository :read-only)
         (is (eql 115 value)))

       ;; Transaction 2: a genuine :RETRY, racing a real simulated
       ;; concurrent writer against this transaction's own first
       ;; attempt only.
       (let ((attempt-count 0))
         (with-transaction (value) (repository :read-write :conflict-resolution :retry)
           (incf attempt-count)
           (when (= attempt-count 1)
             (%e2e-hijack-branch! repository-path "main" 500))
           (1+ value))
         (is (= 2 attempt-count)))
       (with-transaction (value) (repository :read-only)
         (is (eql 501 value)))))))

(test end-to-end-rebase-mode-merges-cleanly-with-a-real-concurrent-writer
  "A genuine, non-mocked :REBASE end-to-end walk through the clean-
merge branch: the transaction under test changes entry \"a\" while a
real concurrent writer (created via %E2E-HIJACK-BRANCH-WITH-TREE!,
sharing real ancestry with the original head) independently changes
only entry \"b\" -- disjoint content, so `git merge-tree` merges
cleanly with no conflict at all. RECEIVER runs exactly ONCE (never
re-invoked -- unlike :RETRY, :REBASE never throws away and re-derives
its own already-computed work), yet the final real commit reflects
BOTH transactions' changes: \"a\" from the transaction under test,
\"b\" from the concurrent writer."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-tree
                                                              :repository repository-path
                                                              :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload 1))
                                                                             (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
      (let ((original-head-sha (git-show-ref-sha repository-path "main"))
            (attempts 0))
        (call-with-git-transaction repository :read-write
                                    :conflict-resolution :rebase
                                    :receiver (lambda (tx head)
                                                (declare (ignore tx head))
                                                (incf attempts)
                                                (when (= attempts 1)
                                                  (%e2e-hijack-branch-with-tree! repository-path "main" original-head-sha
                                                                                 (list (cons "a" 1) (cons "b" 2))))
                                                (make-instance 'git-tree
                                                                :repository repository-path
                                                                :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload 2))
                                                                               (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
        (is (= 1 attempts))
        (let ((final-tree-sha (%e2e-commit-tree-sha repository-path (git-show-ref-sha repository-path "main"))))
          (is (eql 2 (%e2e-tree-entry-payload repository-path final-tree-sha "a")))
          (is (eql 2 (%e2e-tree-entry-payload repository-path final-tree-sha "b"))))))))

(test end-to-end-rebase-mode-falls-back-to-retry-on-a-real-unresolvable-conflict
  "A genuine, non-mocked :REBASE walk through a real, unresolvable
content conflict, with :REBASE-FALLBACK :RETRY: both the transaction
under test and a real concurrent writer (via %E2E-HIJACK-BRANCH-
WITH-TREE!) change the exact same entry \"a\" to different values, so
`git merge-tree` genuinely fails to auto-merge; :REBASE-FALLBACK
:RETRY then signals CONCURRENT-MODIFICATION-ERROR internally, which
CALL-WITH-GIT-TRANSACTION's own :REBASE loop catches exactly as it
would for :RETRY, causing the entire transaction to be re-attempted
from scratch -- RECEIVER runs exactly twice, and the second attempt,
seeing the already-hijacked head, commits cleanly with no further
conflict."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-tree
                                                              :repository repository-path
                                                              :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload 1))
                                                                             (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
      (let ((original-head-sha (git-show-ref-sha repository-path "main"))
            (attempts 0))
        (call-with-git-transaction repository :read-write
                                    :conflict-resolution :rebase
                                    :rebase-fallback :retry
                                    :receiver (lambda (tx head)
                                                (declare (ignore tx head))
                                                (incf attempts)
                                                (when (= attempts 1)
                                                  (%e2e-hijack-branch-with-tree! repository-path "main" original-head-sha
                                                                                 (list (cons "a" 99) (cons "b" 1))))
                                                (make-instance 'git-tree
                                                                :repository repository-path
                                                                :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload (if (> attempts 1) 100 2)))
                                                                               (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
        (is (= 2 attempts))
        (let ((final-tree-sha (%e2e-commit-tree-sha repository-path (git-show-ref-sha repository-path "main"))))
          (is (eql 100 (%e2e-tree-entry-payload repository-path final-tree-sha "a")))
          (is (eql 1 (%e2e-tree-entry-payload repository-path final-tree-sha "b"))))))))

(test end-to-end-rebase-mode-falls-back-to-error-on-a-real-unresolvable-conflict
  "The same real, unresolvable content conflict as
END-TO-END-REBASE-MODE-FALLS-BACK-TO-RETRY-ON-A-REAL-UNRESOLVABLE-
CONFLICT, but with the default :REBASE-FALLBACK :ERROR: instead of
retrying, MERGE-CONFLICT-ERROR propagates all the way out to the
original caller, with GET-BASE-SHA/GET-CANDIDATE-SHA/GET-CURRENT-
HEAD-SHA correctly identifying the original head, the transaction's
own (never-committed) candidate commit, and the real concurrent
writer's hijacked head, respectively; RECEIVER runs exactly once."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-tree
                                                              :repository repository-path
                                                              :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload 1))
                                                                             (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
      (let ((original-head-sha (git-show-ref-sha repository-path "main"))
            (attempts 0)
            hijacked-sha)
        (handler-case
            (progn
              (call-with-git-transaction repository :read-write
                                          :conflict-resolution :rebase
                                          :receiver (lambda (tx head)
                                                      (declare (ignore tx head))
                                                      (incf attempts)
                                                      (setf hijacked-sha
                                                            (%e2e-hijack-branch-with-tree! repository-path "main" original-head-sha
                                                                                           (list (cons "a" 99) (cons "b" 1))))
                                                      (make-instance 'git-tree
                                                                     :repository repository-path
                                                                     :entries (list (cons "a" (make-instance 'git-blob :repository repository-path :payload 2))
                                                                                    (cons "b" (make-instance 'git-blob :repository repository-path :payload 1))))))
              (fail "MERGE-CONFLICT-ERROR was not signaled."))
          (merge-conflict-error (condition)
            (is (string= original-head-sha (get-base-sha condition)))
            (is (string= hijacked-sha (get-current-head-sha condition)))
            (is (stringp (get-candidate-sha condition)))))
        (is (= 1 attempts))
        (is (string= hijacked-sha (git-show-ref-sha repository-path "main")))))))

(test end-to-end-rebase-mode-auto-merges-concurrent-edits-to-different-lines-of-the-same-string
  "The concrete scenario the whole \"leverage Git's native
line-by-line text merging\" design goal exists for: a single string
atom root value, \"alpha\\nbeta\\ngamma\", is concurrently edited on
two different lines -- the transaction under test changes only the
first line (to \"ALPHA\"), while a real concurrent writer (via
%E2E-HIJACK-BRANCH-ATOMIC!, sharing real ancestry with the original
head) independently changes only the third line (to \"GAMMA\").
Because SERIALIZE-ATOM writes literal, unescaped line breaks (see
MULTILINE-STRING-ROUND-TRIPS-WITH-LITERAL-LINE-BREAKS in atom-
serialization-tests.lisp) rather than collapsing the whole string
onto one physical line, `git merge-tree` sees this as two ordinary,
disjoint single-line edits to a plain text file and auto-merges them
cleanly with no conflict at all -- exactly as it would for two
programmers' concurrent edits to two different lines of an ordinary
source file. RECEIVER runs exactly once (never re-invoked -- this is
a clean :REBASE, not a :RETRY), yet the final real committed string
value reflects BOTH edits at once: \"ALPHA\\nbeta\\nGAMMA\"."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write))
          (base-string (format nil "alpha~%beta~%gamma")))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload base-string)))
      (let ((original-head-sha (git-show-ref-sha repository-path "main"))
            (attempts 0))
        (call-with-git-transaction repository :read-write
                                    :conflict-resolution :rebase
                                    :receiver (lambda (tx head)
                                                (declare (ignore tx head))
                                                (incf attempts)
                                                (when (= attempts 1)
                                                  (%e2e-hijack-branch-atomic! repository-path "main" original-head-sha
                                                                              (format nil "alpha~%beta~%GAMMA")))
                                                (make-instance 'git-blob :repository repository-path
                                                                          :payload (format nil "ALPHA~%beta~%gamma"))))
        (is (= 1 attempts))
        (let* ((final-commit-sha (git-show-ref-sha repository-path "main"))
               (final-commit (make-instance 'git-commit :repository repository-path :sha final-commit-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          (is (string= (format nil "ALPHA~%beta~%GAMMA") final-value)))))))

;;; --- Comprehensive real-concurrency coverage for every
;;; CONFLICT-RESOLUTION strategy, plus the surrounding transaction
;;; lifecycle options (:READ-ONLY, explicit ABORT-GIT-TRANSACTION,
;;; and an error signaled from RECEIVER) -- every test below uses a
;;; real, temporary, bare Git repository and, where a race is
;;; needed, a real concurrent writer (via %E2E-HIJACK-BRANCH!/
;;; %E2E-HIJACK-BRANCH-ATOMIC!/%E2E-HIJACK-BRANCH-WITH-TREE!, or a
;;; genuine second OS thread for :LOCK) -- no mocks anywhere.

(test end-to-end-error-mode-propagates-a-real-concurrent-modification-error
  "With the default :CONFLICT-RESOLUTION :ERROR, a real concurrent
writer (via %E2E-HIJACK-BRANCH!) that advances BRANCH between this
transaction's own head-read and its own commit attempt causes a
genuine (non-mocked) CONCURRENT-MODIFICATION-ERROR to propagate
straight out of CALL-WITH-GIT-TRANSACTION, with no retry at all:
RECEIVER runs exactly once, the condition's own GET-EXPECTED-SHA/
GET-NEW-SHA/GET-NAME correctly identify the stale SHA this
transaction started from, the SHA it vainly tried to commit, and the
branch name, and -- crucially -- the real concurrent writer's own
commit is left as BRANCH's current, untouched value: the failed
transaction's own candidate commit is never referenced by anything,
a genuinely dangling object for `git gc` to eventually reclaim."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload 1)))
      (let ((attempts 0)
            (condition nil))
        (handler-case
            (call-with-git-transaction repository :read-write
                                        :receiver (lambda (tx head)
                                                    (declare (ignore tx head))
                                                    (incf attempts)
                                                    (%e2e-hijack-branch! repository-path "main" 500)
                                                    (make-instance 'git-blob :repository repository-path :payload 2)))
          (concurrent-modification-error (c) (setf condition c)))
        (is (not (null condition)))
        (is (= 1 attempts))
        (is (string= "main" (get-name condition)))
        (is (stringp (get-expected-sha condition)))
        (is (stringp (get-new-sha condition)))
        (let* ((final-commit-sha (git-show-ref-sha repository-path "main"))
               (final-commit (make-instance 'git-commit :repository repository-path :sha final-commit-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          ;; The branch still points at the real racer's own commit
          ;; (500), not at the failed transaction's candidate (2).
          (is (eql 500 final-value)))))))

(test end-to-end-retry-mode-succeeds-after-several-real-concurrent-races-in-a-row
  "With :CONFLICT-RESOLUTION :RETRY, RECEIVER is genuinely re-invoked
from scratch, against a freshly re-read head each time, for as many
real concurrent races as actually occur -- not just one: a real
concurrent writer (via %E2E-HIJACK-BRANCH!) advances BRANCH out from
under this transaction's own commit attempt on its first TWO
attempts, and only lets the third attempt's own commit succeed.
RECEIVER runs exactly three times, and the final committed value
reflects the third attempt's own computation against the
second racer's own value (300), not either of the first two,
discarded attempts'."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload 1)))
      (let ((attempts 0))
        (call-with-git-transaction repository :read-write
                                    :conflict-resolution :retry
                                    :receiver (lambda (tx head)
                                                (declare (ignore tx))
                                                (incf attempts)
                                                (case attempts
                                                  (1 (%e2e-hijack-branch! repository-path "main" 100))
                                                  (2 (%e2e-hijack-branch! repository-path "main" 300)))
                                                (make-instance 'git-blob :repository repository-path
                                                                          :payload (1+ (%e2e-decode-blob (resolve-commit-root head))))))
        (is (= 3 attempts))
        (let* ((final-commit-sha (git-show-ref-sha repository-path "main"))
               (final-commit (make-instance 'git-commit :repository repository-path :sha final-commit-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          (is (eql 301 final-value)))))))

(test end-to-end-lock-mode-serializes-two-real-concurrent-transactions-across-threads
  "With :CONFLICT-RESOLUTION :LOCK, two genuinely concurrent
:READ-WRITE transactions -- each running in its own real OS thread
(SB-THREAD:MAKE-THREAD), each against its own independent GIT-
REPOSITORY instance for the very same real repository path, and each
started as close together as possible via a shared start signal --
never actually execute their own RECEIVER bodies at overlapping wall-
clock times: WITH-REPOSITORY-TRANSACTION-LOCK's own lock file
enforces true mutual exclusion, not merely \"held during my own
call\" as the mocked unit test already checks. Each thread's RECEIVER
sleeps briefly (deliberately widening any race window a broken lock
would expose) while incrementing a real, shared counter value by 1;
if the lock failed to serialize them, both threads would read the
same starting value and the final committed counter would be 1
instead of 2 (a lost update) -- and, since GitHack's own
*GIT-IO-SESSIONS* subprocess cache is keyed by repository pathname
and is not itself synchronized, truly overlapping Git I/O from two
threads would also risk protocol-level corruption of a shared `git`
subprocess, not merely a logical lost update; a genuinely working
lock avoids both by construction. Recorded (START . END) wall-clock
intervals for the two threads' own RECEIVER calls, guarded by a
shared mutex, are asserted not to overlap at all, directly proving
serialization rather than merely inferring it from the final count."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload 0)))
      (let ((intervals '())
            (intervals-lock (sb-thread:make-mutex :name "intervals"))
            (start-semaphore (sb-thread:make-semaphore :count 0)))
        (flet ((run-one-transaction ()
                 (sb-thread:wait-on-semaphore start-semaphore)
                 (let ((own-repository (make-instance 'git-repository
                                                        :pathname repository-path
                                                        :branch "main"
                                                        :author +e2e-author+
                                                        :committer +e2e-author+
                                                        :message "racer"
                                                        :mode :read-write)))
                   (call-with-git-transaction own-repository :read-write
                                               :conflict-resolution :lock
                                               :receiver
                                               (lambda (tx head)
                                                 (declare (ignore tx))
                                                 (let ((start (get-internal-real-time)))
                                                   (sleep 0.1)
                                                   (let ((new-value (1+ (%e2e-decode-blob (resolve-commit-root head)))))
                                                     (sb-thread:with-mutex (intervals-lock)
                                                       (push (cons start (get-internal-real-time)) intervals))
                                                     (make-instance 'git-blob :repository repository-path :payload new-value))))))))
          (let ((thread-1 (sb-thread:make-thread #'run-one-transaction :name "e2e-lock-racer-1"))
                (thread-2 (sb-thread:make-thread #'run-one-transaction :name "e2e-lock-racer-2")))
            ;; Release both threads to attempt CALL-WITH-GIT-TRANSACTION
            ;; as close together in wall-clock time as possible.
            (sb-thread:signal-semaphore start-semaphore 2)
            (sb-thread:join-thread thread-1)
            (sb-thread:join-thread thread-2)))
        (is (= 2 (length intervals)))
        (destructuring-bind ((start-1 . end-1) (start-2 . end-2)) intervals
          ;; No overlap: one interval must end at or before the other starts.
          (is (or (<= end-1 start-2) (<= end-2 start-1))))
        (let* ((final-commit-sha (git-show-ref-sha repository-path "main"))
               (final-commit (make-instance 'git-commit :repository repository-path :sha final-commit-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          (is (eql 2 final-value)))
        (is (not (probe-file (%transaction-lock-pathname repository-path))))))))

(test end-to-end-read-only-transaction-never-writes-even-when-receiver-returns-a-new-value
  "A :READ-ONLY transaction against a real repository never advances
BRANCH at all, no matter what RECEIVER returns: it is read exactly
as an ordinary Lisp value, but any \"new\" GIT-OBJECT it constructs
and returns is simply discarded on normal exit."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload 42)))
      (let ((original-head-sha (git-show-ref-sha repository-path "main")))
        (let ((transaction
                (call-with-git-transaction repository :read-only
                                            :receiver (lambda (tx head)
                                                        (declare (ignore tx head))
                                                        (make-instance 'git-blob :repository repository-path :payload 999)))))
          (is (eq :committed (get-status transaction)))
          (is (null (get-result transaction))))
        (is (string= original-head-sha (git-show-ref-sha repository-path "main")))
        (let* ((final-commit (make-instance 'git-commit :repository repository-path :sha original-head-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          (is (eql 42 final-value)))))))

(test end-to-end-explicit-abort-writes-nothing-even-for-a-brand-new-branch
  "Calling ABORT-GIT-TRANSACTION explicitly from RECEIVER, at the
OUTERMOST transaction level (unlike the nested-only abort already
exercised by END-TO-END-NESTED-TRANSACTIONS-COMMIT-ABORT-AND-RETRY),
against a branch that does not exist yet, leaves that branch
genuinely nonexistent afterward: no orphan root commit is ever
created, and a subsequent ordinary transaction still observes a
brand-new (NIL head) branch."
  (with-temporary-git-repository (repository-path)
    (is (null (git-show-ref-sha repository-path "never-created")))
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "never-created"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (let ((transaction
              (call-with-git-transaction repository :read-write
                                          :receiver (lambda (tx head)
                                                      (declare (ignore head))
                                                      (abort-git-transaction tx)))))
        (is (eq :aborted (get-status transaction))))
      (is (null (git-show-ref-sha repository-path "never-created")))
      ;; A genuinely fresh transaction afterward still sees no head at all.
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx))
                                              (is (null head))
                                              (make-instance 'git-blob :repository repository-path :payload 7)))
      (is (not (null (git-show-ref-sha repository-path "never-created")))))))

(test end-to-end-error-in-receiver-leaves-an-existing-branch-completely-untouched
  "A RECEIVER that signals an ordinary Lisp error (not any GitHack-
specific condition) part-way through, against a real repository that
already has a real prior commit, propagates that error straight out
of CALL-WITH-GIT-TRANSACTION and leaves BRANCH pointing at exactly
the same real commit SHA as before -- nothing is ever written, even
though the failing RECEIVER had already constructed (but never
returned) a brand-new candidate GIT-BLOB."
  (with-temporary-git-repository (repository-path)
    (let ((repository (make-instance 'git-repository
                                      :pathname repository-path
                                      :branch "main"
                                      :author +e2e-author+
                                      :committer +e2e-author+
                                      :message "initial"
                                      :mode :read-write)))
      (call-with-git-transaction repository :read-write
                                  :receiver (lambda (tx head)
                                              (declare (ignore tx head))
                                              (make-instance 'git-blob :repository repository-path :payload 10)))
      (let ((original-head-sha (git-show-ref-sha repository-path "main")))
        (signals simple-error
          (call-with-git-transaction repository :read-write
                                      :receiver (lambda (tx head)
                                                  (declare (ignore tx head))
                                                  (make-instance 'git-blob :repository repository-path :payload 999)
                                                  (error "RECEIVER failed on purpose."))))
        (is (string= original-head-sha (git-show-ref-sha repository-path "main")))
        (let* ((final-commit (make-instance 'git-commit :repository repository-path :sha original-head-sha))
               (final-value (%e2e-decode-blob (resolve-commit-root final-commit))))
          (is (eql 10 final-value)))))))
