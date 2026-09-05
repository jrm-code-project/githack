;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; TECHNICAL_DEBT.md item #12: GIT-IO.LISP and GIT-BRANCH.LISP rely
;;; on UIOP:NATIVE-NAMESTRING/UIOP:DEFAULT-TEMPORARY-DIRECTORY rather
;;; than hardcoded path separators, which is the right call, but that
;;; had never actually been exercised end-to-end against a real Git
;;; repository whose own pathname contains characters more exotic
;;; than plain ASCII alphanumerics. These tests do exactly that: a
;;; real `git init --bare` repository (via WITH-TEMPORARY-GIT-
;;; REPOSITORY's NAME-PREFIX argument), inside a directory whose name
;;; contains a space or non-ASCII characters, exercised through the
;;; full GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE round trip and through
;;; GIT-SHOW-REF-SHA/GIT-UPDATE-REF (via RESOLVE-BRANCH/UPDATE-BRANCH
;;; and a real CALL-WITH-REPOSITORY/WITH-TRANSACTION commit).
;;;
;;; UNC paths (\\server\share\...) are deliberately NOT covered here:
;;; exercising one for real would require an actual, reachable SMB
;;; network share, which is not available in an automated test
;;; environment. UIOP:NATIVE-NAMESTRING is still relied on for UNC
;;; paths exactly as it is for any other pathname, so this is a
;;; conscious scope reduction, not an oversight.

(def-suite pathname-portability-suite
  :in githack-suite
  :description "Tests exercising real Git repository pathnames containing spaces and non-ASCII characters.")

(in-suite pathname-portability-suite)

(defparameter +portability-author+ "Test Author <test@githack.local>")

(test git-hash-object-and-git-cat-file-round-trip-in-a-path-with-spaces
  "GIT-HASH-OBJECT and GIT-CAT-FILE round-trip real octets correctly
when the repository's own --git-dir pathname contains spaces."
  (with-temporary-git-repository (repository "githack e2e with spaces ")
    (let* ((octets (sb-ext:string-to-octets "hello, githack" :external-format :utf-8))
           (sha (git-hash-object repository "blob" octets)))
      (is (= 40 (length sha)))
      (is (equalp octets (git-cat-file repository sha)))
      (is (string= "blob" (git-type repository sha))))))

(test git-hash-object-and-git-cat-file-round-trip-in-a-path-with-non-ascii-characters
  "GIT-HASH-OBJECT and GIT-CAT-FILE round-trip real octets correctly
when the repository's own --git-dir pathname contains non-ASCII
characters."
  (with-temporary-git-repository (repository "githack-e2e-\u00fcn\u00efc\u00f8d\u00e9-")
    (let* ((octets (sb-ext:string-to-octets "hello, githack" :external-format :utf-8))
           (sha (git-hash-object repository "blob" octets)))
      (is (= 40 (length sha)))
      (is (equalp octets (git-cat-file repository sha)))
      (is (string= "blob" (git-type repository sha))))))

(test resolve-branch-and-update-branch-round-trip-in-a-path-with-spaces
  "GIT-SHOW-REF-SHA and GIT-UPDATE-REF (via RESOLVE-BRANCH and
UPDATE-BRANCH) round-trip a real branch ref correctly when the
repository's own --git-dir pathname contains spaces."
  (with-temporary-git-repository (repository "githack e2e with spaces ")
    ;; Build a genuinely valid commit -- an empty GIT-TREE persisted
    ;; for real, then SERIALIZE-COMMIT'd and hashed with GIT-HASH-
    ;; OBJECT's type "commit" -- since Git validates commit/tree
    ;; object content (unlike blobs), an arbitrary byte string is not
    ;; accepted as a commit object.
    (let* ((tree-sha (git-hash-object repository "tree"
                                       (serialize-tree (make-instance 'git-tree
                                                                       :repository repository
                                                                       :entries '()))))
           (tree-proxy (make-instance 'git-tree :repository repository :sha tree-sha))
           (commit (make-instance 'git-commit
                                   :repository repository
                                   :tree tree-proxy
                                   :parents '()
                                   :author +portability-author+
                                   :committer +portability-author+
                                   :timestamp 1700000000
                                   :message "portability"))
           (commit-sha (git-hash-object repository "commit"
                                         (sb-ext:string-to-octets (serialize-commit commit)
                                                                   :external-format :utf-8))))
      (is (null (get-target (resolve-branch repository "main" :if-does-not-exist nil))))
      (let* ((stand-in-commit (make-instance 'git-commit :repository repository :sha commit-sha))
             (branch (make-instance 'git-branch :repository repository :name "main" :target stand-in-commit)))
        (update-branch branch)
        (let ((resolved (resolve-branch repository "main")))
          (is (string= "main" (get-name resolved)))
          (is (string= commit-sha (sha (get-target resolved)))))))))

(test end-to-end-transaction-commits-successfully-in-a-path-with-non-ascii-characters
  "A full, real CALL-WITH-REPOSITORY/WITH-TRANSACTION read-write
commit -- exercising GIT-HASH-OBJECT, GIT-CAT-FILE, GIT-SHOW-REF-SHA,
and GIT-UPDATE-REF together -- succeeds when the repository's own
--git-dir pathname contains non-ASCII characters."
  (with-temporary-git-repository (repository-path "githack-e2e-\u00fcn\u00efc\u00f8d\u00e9-")
    (call-with-repository
     repository-path
     :branch "main" :author +portability-author+ :message "portability" :mode :read-write
     :receiver
     (lambda (repository)
       (with-transaction (value) (repository :read-write)
         (is (null value))
         :hello)
       (with-transaction (value) (repository :read-write)
         (is (eq :hello value))
         value)))))
