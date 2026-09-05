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
                                             :sha "6666666666666666666666666666666666666666")))
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
                                                 :sha "8888888888888888888888888888888888888888")))
        (with-fake-git-type ((list (cons blob-sha "blob")
                                    (cons (sha (cdr (assoc ".meta" wrapper-entries :test #'string=))) "blob")
                                    (cons (sha (cdr (assoc "README.md" wrapper-entries :test #'string=))) "blob")))
          (let ((resolved (resolve-commit-root commit)))
            (is (typep resolved 'git-blob))
            (is (string= blob-sha (sha resolved)))))))))
