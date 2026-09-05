;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite atomic-wrapper-suite
  :in githack-suite
  :description "Tests for WRAP-ATOMIC-COMMIT-ROOT and RESOLVE-COMMIT-ROOT.")

(in-suite atomic-wrapper-suite)

(test wrap-atomic-commit-root-signals-error-for-unpersisted-object
  "WRAP-ATOMIC-COMMIT-ROOT cannot wrap a GIT-OBJECT that has no SHA
yet, since there is no SHA to point the wrapper's \"value\" entry at."
  (let ((unsaved-blob (make-instance 'git-blob :repository :dummy-repo)))
    (signals error (wrap-atomic-commit-root :dummy-repo unsaved-blob))))

(test wrap-atomic-commit-root-builds-standard-three-entry-tree
  "WRAP-ATOMIC-COMMIT-ROOT produces a persisted GIT-TREE with exactly
the three standard entries -- \".meta\", \"README.md\", and
\"value\" -- in sorted order, \"value\" pointing at the wrapped
GIT-OBJECT itself, and writes the fixed README.md content and the
(:TAG :ATOMIC-WRAPPER) plist as raw UTF-8 bytes."
  (let* ((calls '())
         (blob (make-instance 'git-blob :repository :dummy-repo :payload 42
                                         :sha "3333333333333333333333333333333333333333")))
    (with-recording-git-hash-object (calls)
      (let ((tree (wrap-atomic-commit-root :dummy-repo blob)))
        (is (typep tree 'git-tree))
        (is (stringp (sha tree)))
        (is (get-loaded? tree))
        (let ((entries (get-entries tree)))
          (is (equal (list ".meta" "README.md" "value") (mapcar #'car entries)))
          (is (eq blob (cdr (assoc "value" entries :test #'string=)))))))
    (is (find (sb-ext:string-to-octets +atomic-wrapper-readme+ :external-format :utf-8)
              calls :key #'third :test #'equalp))))

(test resolve-commit-root-passes-through-an-ordinary-tree
  "RESOLVE-COMMIT-ROOT returns a commit's own tree unchanged when
that tree has no \".meta\" entry at all (an ordinary directory tree,
not any kind of wrapper)."
  (let* ((file-sha "4444444444444444444444444444444444444444")
         (file (make-instance 'git-blob :repository :dummy-repo :sha file-sha))
         (tree (make-instance 'git-tree :repository :dummy-repo
                                         :entries (list (cons "file.txt" file))
                                         :sha "5555555555555555555555555555555555555555"
                                         :loaded? t))
         (commit (make-instance 'git-commit :repository :dummy-repo :tree tree
                                             :sha "6666666666666666666666666666666666666666"
                                             :loaded? t)))
    (is (eq tree (resolve-commit-root commit)))))

(test resolve-commit-root-unwraps-an-atomic-wrapper-tree
  "RESOLVE-COMMIT-ROOT transparently returns the GIT-OBJECT held in
an ATOMIC-WRAPPER-TREE's \"value\" entry, round-tripping through
WRAP-ATOMIC-COMMIT-ROOT and a real (fake) Git tree/commit."
  (let* ((blob-sha "7777777777777777777777777777777777777777")
         (blob (make-instance 'git-blob :repository :dummy-repo :payload :hello :sha blob-sha)))
    (with-fake-git-object-store ()
      (let* ((wrapper (wrap-atomic-commit-root :dummy-repo blob))
             (wrapper-entries (get-entries wrapper))
             (commit (make-instance 'git-commit :repository :dummy-repo
                                                 :tree (make-instance 'git-tree :repository :dummy-repo
                                                                                 :sha (sha wrapper))
                                                 :sha "8888888888888888888888888888888888888888"
                                                 :loaded? t)))
        (with-fake-git-type ((list (cons blob-sha "blob")
                                    (cons (sha (cdr (assoc ".meta" wrapper-entries :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "README.md" wrapper-entries :test #'string=))) "blob")))
          (let ((resolved (resolve-commit-root commit)))
            (is (typep resolved 'git-blob))
            (is (string= blob-sha (sha resolved)))))))))

(test resolve-commit-root-loads-a-genuinely-unloaded-commit-first
  "RESOLVE-COMMIT-ROOT works on a genuinely unloaded GIT-COMMIT proxy
-- as INFLATE-GIT-PROXY would produce for a GIT-BRANCH's TARGET --
fetching and parsing its raw commit text via GIT-CAT-FILE and
DESERIALIZE-COMMIT before inspecting its tree, exactly as it would
for an already fully in-memory commit."
  (let* ((blob-sha "9999999999999999999999999999999999999999")
         (blob (make-instance 'git-blob :repository :dummy-repo :payload 7 :sha blob-sha)))
    (with-fake-git-object-store ()
      (let* ((tree (make-instance 'git-tree :repository :dummy-repo
                                             :entries (list (cons "f.txt" blob))))
             (tree-sha (progn
                         (setf (sha tree) (git-hash-object :dummy-repo "tree" (serialize-tree tree)))
                         (sha tree)))
             (commit (make-instance 'git-commit :repository :dummy-repo
                                                 :tree tree :parents '()
                                                 :author "A <a@b.c>" :committer "A <a@b.c>"
                                                 :timestamp 1000 :message "initial")))
        (setf (sha commit)
              (git-hash-object :dummy-repo "commit"
                                (sb-ext:string-to-octets (serialize-commit commit) :external-format :utf-8)))
        (let ((unloaded-commit (make-instance 'git-commit :repository :dummy-repo :sha (sha commit))))
          (is (null (get-loaded? unloaded-commit)))
          (with-fake-git-type ((list (cons tree-sha "tree") (cons blob-sha "blob")))
            (let ((resolved (resolve-commit-root unloaded-commit)))
              (is (eq t (get-loaded? unloaded-commit)))
              (is (typep resolved 'git-tree))
              (is (string= tree-sha (sha resolved))))))))))
