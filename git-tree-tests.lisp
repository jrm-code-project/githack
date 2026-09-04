;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite git-tree-suite
  :in githack-suite
  :description "Tests for the GIT-TREE proxy, INFER-GIT-MODE, SERIALIZE-TREE, and DESERIALIZE-TREE.")

(in-suite git-tree-suite)

(defparameter +blob-sha+ "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a persisted GIT-BLOB in tests.")

(defparameter +tree-sha+ "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  "A syntactically valid, arbitrary 40-character hex SHA used to stand in for a persisted GIT-TREE in tests.")

(test infer-git-mode-distinguishes-blob-and-tree
  "INFER-GIT-MODE returns the standard file mode for a GIT-BLOB and
the directory mode for a GIT-TREE."
  (is (string= "100644" (infer-git-mode (make-instance 'git-blob :sha +blob-sha+ :repository :dummy-repo))))
  (is (string= "40000" (infer-git-mode (make-instance 'git-tree :sha +tree-sha+ :repository :dummy-repo)))))

(test serialize-tree-signals-error-for-unpersisted-entry
  "SERIALIZE-TREE cannot encode an entry whose GIT-OBJECT has no SHA
yet, since there is no binary digest to pack into the entry."
  (let* ((unsaved-blob (make-instance 'git-blob :repository :dummy-repo))
         (tree (make-instance 'git-tree :repository :dummy-repo
                                          :entries (list (cons "x.txt" unsaved-blob)))))
    (signals error (serialize-tree tree))))

(test serialize-tree-sorts-and-formats-entries
  "SERIALIZE-TREE sorts entries by filename (respecting Git's
directory-suffix sorting rule) and formats each as
<mode> <space> <filename> <NUL> <20-byte binary SHA>."
  (let* ((blob (make-instance 'git-blob :sha +blob-sha+ :repository :dummy-repo))
         (tree-obj (make-instance 'git-tree :sha +tree-sha+ :repository :dummy-repo))
         (tree (make-instance 'git-tree :repository :dummy-repo
                                          :entries (list (cons "z" blob)
                                                          (cons "a" tree-obj))))
         (octets (serialize-tree tree)))
    ;; "a" (as a tree, compared as "a/") sorts before "z".
    (is (string= "40000 a" (sb-ext:octets-to-string octets :end 7 :external-format :utf-8)))
    (is (= 0 (aref octets 7)))
    (is (= 20 (count 170 octets)))          ; 170 = #xAA, from +tree-sha+'s bytes
    (let ((z-offset (position 122 octets))) ; 122 = char-code of #\z
      (is (string= "100644 z"
                    (sb-ext:octets-to-string octets
                                              :start (- z-offset 7)
                                              :end (1+ z-offset)
                                              :external-format :utf-8))))))

(test deserialize-tree-round-trips-with-serialize-tree
  "Serializing a GIT-TREE's entries and then deserializing the
resulting bytes reconstructs an equivalent alist of filenames to
correctly-typed, correctly-SHA'd proxy objects."
  (let* ((blob (make-instance 'git-blob :sha +blob-sha+ :repository :dummy-repo))
         (subtree (make-instance 'git-tree :sha +tree-sha+ :repository :dummy-repo))
         (tree (make-instance 'git-tree :repository :dummy-repo
                                          :entries (list (cons "file.txt" blob)
                                                          (cons "subdir" subtree))))
         (octets (serialize-tree tree)))
    (with-fake-git-type ((list (cons +blob-sha+ "blob") (cons +tree-sha+ "tree")))
      (let ((result (deserialize-tree :dummy-repo octets)))
        (is (= 2 (length result)))
        (destructuring-bind (first second) result
          (is (string= "file.txt" (car first)))
          (is (typep (cdr first) 'git-blob))
          (is (string= +blob-sha+ (get-sha (cdr first))))
          (is (string= "subdir" (car second)))
          (is (typep (cdr second) 'git-tree))
          (is (string= +tree-sha+ (get-sha (cdr second)))))))))

(test deserialize-tree-orders-file-before-similarly-named-directory
  "Git's directory-suffix sorting rule means a file named \"ab.txt\"
sorts before a directory named \"ab\", because comparing their
implicit terminators puts '.' (0x2E) before the directory's implicit
'/' (0x2F) once the shared \"ab\" prefix is exhausted; this test
uses a case where the naive (un-suffixed) comparison would give the
opposite order to double-check the suffix rule is applied. Verified
against real Git's own tree sort via `git ls-tree`."
  (let* ((dir (make-instance 'git-tree :sha +tree-sha+ :repository :dummy-repo))
         (file (make-instance 'git-blob :sha +blob-sha+ :repository :dummy-repo))
         (tree (make-instance 'git-tree :repository :dummy-repo
                                          :entries (list (cons "ab.txt" file)
                                                          (cons "ab" dir))))
         (octets (serialize-tree tree)))
    (with-fake-git-type ((list (cons +blob-sha+ "blob") (cons +tree-sha+ "tree")))
      (let ((result (deserialize-tree :dummy-repo octets)))
        (is (string= "ab.txt" (car (first result))))
        (is (string= "ab" (car (second result))))))))
