;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; Tests for GIT-IO.LISP's low-level GIT-HASH-OBJECT/GIT-TYPE/
;;; GIT-CAT-FILE primitives. Unlike most of this suite (which fakes
;;; these three functions via WITH-FAKE-GIT-*/WITH-RECORDING-GIT-*),
;;; these tests genuinely shell out to a real, temporary, bare Git
;;; repository (via WITH-TEMPORARY-GIT-REPOSITORY) to exercise the
;;; actual subprocess plumbing: non-zero exit-status handling,
;;; binary/non-UTF-8-safe content round-tripping, and temp-file
;;; cleanup on failure.

(def-suite git-io-suite
  :in githack-suite
  :description "Tests for GIT-HASH-OBJECT, GIT-TYPE, and GIT-CAT-FILE against a real Git repository.")

(in-suite git-io-suite)

(defun %count-githack-temp-files ()
  "Return the number of files in the system's default temporary
directory whose name begins with \"githack-object-\" or
\"githack-catfile-\" -- GIT-IO.LISP's own temp-file naming
convention (see %UNIQUE-TEMPORARY-PATHNAME) -- so a test can confirm
none are ever left behind, even when the subprocess using one
fails."
  (length
   (remove-if-not
    (lambda (pathname)
      (let ((name (pathname-name pathname)))
        (and (stringp name)
             (or (and (>= (length name) 15) (string= name "githack-object-" :end1 15))
                 (and (>= (length name) 17) (string= name "githack-catfile-" :end1 16))))))
    (directory (merge-pathnames "*.tmp" (uiop:default-temporary-directory))))))

(test git-hash-object-and-git-cat-file-round-trip-a-blob
  "GIT-HASH-OBJECT writes real octets into a real repository's object
database, and GIT-CAT-FILE reads that exact same byte-for-byte
content back out again given the SHA GIT-HASH-OBJECT returned."
  (with-temporary-git-repository (repository)
    (let* ((octets (sb-ext:string-to-octets "hello, githack" :external-format :utf-8))
           (sha (git-hash-object repository "blob" octets)))
      (is (= 40 (length sha)))
      (is (every (lambda (char) (digit-char-p char 16)) sha))
      (is (equalp octets (git-cat-file repository sha))))))

(test git-type-reports-blob-tree-and-commit
  "GIT-TYPE reports the correct object type string for each of Git's
three object kinds, all written into the same real repository."
  (with-temporary-git-repository (repository)
    (let* ((blob-sha (git-hash-object repository "blob"
                                       (sb-ext:string-to-octets "atom" :external-format :utf-8)))
           (tree-octets (serialize-tree
                         (make-instance 'git-tree
                                        :repository repository
                                        :entries (list (cons "a" (make-instance 'git-blob
                                                                                 :repository repository
                                                                                 :sha blob-sha))))))
           (tree-sha (git-hash-object repository "tree" tree-octets))
           (commit-text (format nil "tree ~A~%author A <a@b.c> 1700000000 +0000~%committer A <a@b.c> 1700000000 +0000~%~%msg" tree-sha))
           (commit-sha (git-hash-object repository "commit"
                                         (sb-ext:string-to-octets commit-text :external-format :utf-8))))
      (is (string= "blob" (git-type repository blob-sha)))
      (is (string= "tree" (git-type repository tree-sha)))
      (is (string= "commit" (git-type repository commit-sha))))))

(test git-cat-file-round-trips-binary-non-utf8-content
  "GIT-CAT-FILE reads back arbitrary binary content -- including byte
sequences that are not valid UTF-8 -- byte-for-byte unchanged, since
it captures subprocess output through a temporary file rather than a
Lisp string."
  (with-temporary-git-repository (repository)
    (let* ((octets (make-array 256 :element-type '(unsigned-byte 8)
                                    :initial-contents (loop for i below 256 collect i)))
           (sha (git-hash-object repository "blob" octets)))
      (is (equalp octets (git-cat-file repository sha))))))

(test git-type-signals-an-error-for-a-nonexistent-sha
  "GIT-TYPE propagates a real subprocess failure (Git's own non-zero
exit status for `cat-file -t` on a SHA that does not exist in the
repository) as a Lisp error, rather than silently returning garbage."
  (with-temporary-git-repository (repository)
    (signals error
      (git-type repository "dddddddddddddddddddddddddddddddddddddddd"))))

(test git-cat-file-signals-an-error-for-a-nonexistent-sha
  "GIT-CAT-FILE propagates a real subprocess failure the same way
GIT-TYPE does, for a SHA absent from the repository."
  (with-temporary-git-repository (repository)
    (signals error
      (git-cat-file repository "dddddddddddddddddddddddddddddddddddddddd"))))

(test git-hash-object-signals-an-error-for-an-invalid-type
  "GIT-HASH-OBJECT propagates a real subprocess failure when asked to
write an object of a TYPE Git itself does not recognize."
  (with-temporary-git-repository (repository)
    (signals error
      (git-hash-object repository "not-a-real-type"
                        (sb-ext:string-to-octets "x" :external-format :utf-8)))))

(test git-hash-object-cleans-up-its-temp-file-even-when-git-itself-fails
  "GIT-HASH-OBJECT's temporary file (used to hand OCTETS to `git
hash-object` on disk) is deleted via UNWIND-PROTECT even when the
subprocess itself fails and signals an error, leaving no stray
\"githack-object-*.tmp\" file behind."
  (with-temporary-git-repository (repository)
    (let ((before (%count-githack-temp-files)))
      (signals error
        (git-hash-object repository "not-a-real-type"
                          (sb-ext:string-to-octets "x" :external-format :utf-8)))
      (is (= before (%count-githack-temp-files))))))

(test git-cat-file-cleans-up-its-temp-file-even-when-git-itself-fails
  "GIT-CAT-FILE's temporary output file is deleted via UNWIND-PROTECT
even when the underlying subprocess fails (here, because GIT-TYPE
itself signals first, before any `cat-file <type> <sha>` temp file
is even created), leaving no stray \"githack-catfile-*.tmp\" file
behind."
  (with-temporary-git-repository (repository)
    (let ((before (%count-githack-temp-files)))
      (signals error
        (git-cat-file repository "dddddddddddddddddddddddddddddddddddddddd"))
      (is (= before (%count-githack-temp-files))))))
