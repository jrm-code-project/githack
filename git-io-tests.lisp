;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; Tests for GIT-IO.LISP's low-level GIT-HASH-OBJECT/GIT-TYPE/
;;; GIT-CAT-FILE primitives. Unlike most of this suite (which fakes
;;; these three functions via WITH-FAKE-GIT-*/WITH-RECORDING-GIT-*),
;;; these tests genuinely shell out to a real, temporary, bare Git
;;; repository (via WITH-TEMPORARY-GIT-REPOSITORY) to exercise the
;;; actual subprocess plumbing: non-zero exit-status handling,
;;; binary/non-UTF-8-safe content round-tripping, temp-file cleanup
;;; on failure, persistent-session reuse across repeated calls (see
;;; *GIT-IO-SESSIONS*), CLOSE-GIT-IO-SESSIONS's per-repository
;;; teardown, and the one-shot fallback path when a session has died.

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

(test git-hash-object-reuses-a-single-persistent-session-across-calls
  "Two GIT-HASH-OBJECT calls of the same TYPE against the same
repository reuse a single cached `git hash-object --stdin-paths`
session (see *GIT-IO-SESSIONS*) instead of spawning a fresh
subprocess every time."
  (with-temporary-git-repository (repository)
    (git-hash-object repository "blob" (sb-ext:string-to-octets "one" :external-format :utf-8))
    (let ((process (gethash (%git-io-session-key :hash-object repository "blob") *git-io-sessions*)))
      (is-true process)
      (is-true (uiop:process-alive-p process))
      (git-hash-object repository "blob" (sb-ext:string-to-octets "two" :external-format :utf-8))
      (is (eq process (gethash (%git-io-session-key :hash-object repository "blob") *git-io-sessions*))))))

(test git-type-and-git-cat-file-reuse-their-own-persistent-sessions
  "GIT-TYPE and GIT-CAT-FILE each reuse their own cached session
\(`cat-file --batch-check` and `cat-file --batch`, respectively)
across repeated calls against the same repository."
  (with-temporary-git-repository (repository)
    (let ((sha (git-hash-object repository "blob" (sb-ext:string-to-octets "atom" :external-format :utf-8))))
      (git-type repository sha)
      (let ((batch-check-process (gethash (%git-io-session-key :batch-check repository) *git-io-sessions*)))
        (is-true batch-check-process)
        (git-type repository sha)
        (is (eq batch-check-process (gethash (%git-io-session-key :batch-check repository) *git-io-sessions*))))
      (git-cat-file repository sha)
      (let ((batch-process (gethash (%git-io-session-key :batch repository) *git-io-sessions*)))
        (is-true batch-process)
        (git-cat-file repository sha)
        (is (eq batch-process (gethash (%git-io-session-key :batch repository) *git-io-sessions*)))))))

(test close-git-io-sessions-terminates-and-forgets-a-repositorys-sessions
  "CLOSE-GIT-IO-SESSIONS terminates every cached session for a given
repository and removes it from *GIT-IO-SESSIONS*, without disturbing
sessions cached for a different repository."
  (with-temporary-git-repository (repository-1)
    (with-temporary-git-repository (repository-2)
      (git-hash-object repository-1 "blob" (sb-ext:string-to-octets "a" :external-format :utf-8))
      (git-hash-object repository-2 "blob" (sb-ext:string-to-octets "b" :external-format :utf-8))
      (let ((process-1 (gethash (%git-io-session-key :hash-object repository-1 "blob") *git-io-sessions*))
            (process-2 (gethash (%git-io-session-key :hash-object repository-2 "blob") *git-io-sessions*)))
        (is-true process-1)
        (is-true process-2)
        (close-git-io-sessions repository-1)
        (is (null (gethash (%git-io-session-key :hash-object repository-1 "blob") *git-io-sessions*)))
        (is-false (uiop:process-alive-p process-1))
        (is (eq process-2 (gethash (%git-io-session-key :hash-object repository-2 "blob") *git-io-sessions*)))
        (is-true (uiop:process-alive-p process-2))
        (close-git-io-sessions repository-2)))))

(test git-hash-object-falls-back-to-one-shot-when-its-session-dies
  "If GIT-HASH-OBJECT's persistent session for a TYPE has already
died (e.g. Git rejected that TYPE and exited without echoing a SHA),
the next call for that same TYPE transparently falls back to a
one-shot subprocess and still succeeds, rather than propagating the
dead session's failure."
  (with-temporary-git-repository (repository)
    (signals error
      (git-hash-object repository "not-a-real-type"
                        (sb-ext:string-to-octets "x" :external-format :utf-8)))
    (let* ((octets (sb-ext:string-to-octets "recovered" :external-format :utf-8))
           (sha (git-hash-object repository "blob" octets)))
      (is (= 40 (length sha)))
      (is (equalp octets (git-cat-file repository sha))))))

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

(defmacro %with-fresh-git-availability-cache (() &body body)
  "Within BODY, rebind *GIT-AVAILABLE-P* to NIL, so %ENSURE-GIT-
AVAILABLE's memoization does not leak between tests (or reflect
whatever earlier test in this suite already ran a real `git
--version` check); restores the prior value afterward."
  `(let ((*git-available-p* nil)) ,@body))

(test ensure-git-available-succeeds-and-memoizes-against-a-real-git
  "%ENSURE-GIT-AVAILABLE returns T when a real `git` executable is on
PATH (as it must be, for every other test in this suite to work at
all), and memoizes that result in *GIT-AVAILABLE-P* so a second call
does not need to shell out again."
  (%with-fresh-git-availability-cache ()
    (is (eq t (%ensure-git-available)))
    (is (eq t *git-available-p*))
    (is (eq t (%ensure-git-available)))))

(test ensure-git-available-signals-git-not-found-error-when-git-is-unreachable
  "%ENSURE-GIT-AVAILABLE signals GIT-NOT-FOUND-ERROR, not some raw
UIOP condition, when running `git --version` itself fails (e.g. no
such executable on PATH), and does not memoize that failure."
  (%with-fresh-git-availability-cache ()
    (let ((was-bound (fboundp 'uiop:run-program))
          (original (fdefinition 'uiop:run-program)))
      (setf (fdefinition 'uiop:run-program)
            (lambda (&rest args) (declare (ignore args)) (error "no such executable")))
      (unwind-protect
           (signals git-not-found-error (%ensure-git-available))
        (if was-bound (setf (fdefinition 'uiop:run-program) original) (fmakunbound 'uiop:run-program)))
      (is (null *git-available-p*)))))

(test ensure-git-available-signals-git-not-found-error-for-a-nonzero-exit-status
  "%ENSURE-GIT-AVAILABLE signals GIT-NOT-FOUND-ERROR when `git
--version` itself runs but exits with a non-zero status, not just
when the subprocess fails to start at all."
  (%with-fresh-git-availability-cache ()
    (let ((was-bound (fboundp 'uiop:run-program))
          (original (fdefinition 'uiop:run-program)))
      (setf (fdefinition 'uiop:run-program)
            (lambda (&rest args)
              (declare (ignore args))
              (values "" "not git" 1)))
      (unwind-protect
           (signals git-not-found-error (%ensure-git-available))
        (if was-bound (setf (fdefinition 'uiop:run-program) original) (fmakunbound 'uiop:run-program)))
      (is (null *git-available-p*)))))

(test call-with-repository-signals-git-not-found-error-when-git-is-unreachable
  "CALL-WITH-REPOSITORY propagates %ENSURE-GIT-AVAILABLE's
GIT-NOT-FOUND-ERROR up front, before ever constructing a
GIT-REPOSITORY or invoking RECEIVER."
  (%with-fresh-git-availability-cache ()
    (let ((was-bound (fboundp 'uiop:run-program))
          (original (fdefinition 'uiop:run-program))
          (receiver-called nil))
      (setf (fdefinition 'uiop:run-program)
            (lambda (&rest args) (declare (ignore args)) (error "no such executable")))
      (unwind-protect
           (signals git-not-found-error
             (call-with-repository #p"/fake/repo/"
                                    :receiver (lambda (repository) (declare (ignore repository)) (setf receiver-called t))))
        (if was-bound (setf (fdefinition 'uiop:run-program) original) (fmakunbound 'uiop:run-program)))
      (is-false receiver-called))))

