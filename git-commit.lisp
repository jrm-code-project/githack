;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Serialization of a GIT-COMMIT proxy's slots to/from the exact
;;; plain-text format Git itself uses for a commit object.
;;;
;;; A Git commit's raw payload is plain UTF-8 text: a "tree" header
;;; line, zero or more "parent" header lines, an "author" line, a
;;; "committer" line, a single blank line, and finally the raw
;;; commit message. Both signature lines end with a Unix epoch
;;; timestamp and a timezone offset, e.g.:
;;;
;;;   tree 3c4be5f559c124db205bc1409132500c3b4869d3
;;;   author The Boss <boss@githack.local> 1700000000 +0000
;;;   committer The Boss <boss@githack.local> 1700000000 +0000
;;;
;;;   first commit

(defparameter +default-commit-timezone-offset+ "+0000"
  "The timezone offset SERIALIZE-COMMIT stamps onto \"author\" and
\"committer\" lines. GIT-COMMIT has no per-signature timezone slot
of its own -- only a single shared TIMESTAMP -- so every commit this
layer produces is recorded as UTC.")

(defun %commit-signature-line (header signature timestamp)
  "Return one Git commit header line: HEADER (\"author\" or
\"committer\"), a space, SIGNATURE (e.g. \"The Boss
<boss@githack.local>\"), TIMESTAMP printed as a decimal Unix epoch
integer, and the default timezone offset."
  (format nil "~A ~A ~D ~A" header signature timestamp +default-commit-timezone-offset+))

(defun %commit-object-sha (git-object description)
  "Return GIT-OBJECT's 40-character hexadecimal SHA, signaling an
error mentioning DESCRIPTION if GIT-OBJECT has not yet been
persisted (and so has no SHA)."
  (or (sha git-object)
      (error "Cannot serialize commit: its ~A has no SHA (not yet persisted)." description)))

(defun serialize-commit (commit)
  "Compile COMMIT into the exact plain-text format Git uses for a
commit object: a \"tree\" line, zero or more \"parent\" lines, an
\"author\" line, a \"committer\" line, a blank line, and finally the
raw MESSAGE, in that order."
  (let* ((tree-sha (%commit-object-sha (get-tree commit) "TREE"))
         (parent-shas
           (mapcar (lambda (parent) (%commit-object-sha parent "PARENT"))
                   (get-parents commit)))
         (timestamp (get-timestamp commit))
         (header-lines
           (append (list (format nil "tree ~A" tree-sha))
                   (mapcar (lambda (sha) (format nil "parent ~A" sha)) parent-shas)
                   (list (%commit-signature-line "author" (get-author commit) timestamp)
                         (%commit-signature-line "committer" (get-committer commit) timestamp)
                         ""))))
    (format nil "~{~A~%~}~A" header-lines (get-message commit))))

(defun %split-commit-header-and-message (text)
  "Split TEXT -- the raw text of a Git commit object -- into two
values: a list of its header lines (\"tree\", \"parent\", \"author\",
\"committer\", and any others Git may add, such as \"gpgsig\") up
to but excluding the first blank line, and MESSAGE, the remainder of
TEXT following that blank line's own trailing newline."
  (let ((length (length text)))
    (labels ((next-line (start)
               (let ((newline (position #\Newline text :start start)))
                 (values (subseq text start newline)
                         (if newline (1+ newline) length)))))
      (loop with start = 0
            with lines = '()
            do (multiple-value-bind (line next) (next-line start)
                 (when (string= line "")
                   (return (values (nreverse lines) (subseq text next))))
                 (push line lines)
                 (setf start next))))))

(defun %parse-commit-header-line (prefix line)
  "If LINE begins with PREFIX followed by a single space, return the
remainder of LINE following that space; otherwise return NIL."
  (let ((prefix-length (1+ (length prefix))))
    (when (and (>= (length line) prefix-length)
               (string= prefix line :end2 (length prefix))
               (char= #\Space (char line (length prefix))))
      (subseq line prefix-length))))

(defun %parse-commit-signature-line (remainder)
  "Parse REMAINDER -- the text following an \"author\"/\"committer\"
header's name, as returned by %PARSE-COMMIT-HEADER-LINE -- into two
values: the signature string (e.g. \"The Boss
<boss@githack.local>\") and its integer Unix epoch timestamp,
discarding the trailing timezone offset."
  (let* ((timezone-space (position #\Space remainder :from-end t))
         (timestamp-space (position #\Space remainder :from-end t :end timezone-space)))
    (unless (and timezone-space timestamp-space)
      (error "Malformed Git commit signature line ~S." remainder))
    (values (subseq remainder 0 timestamp-space)
            (parse-integer remainder :start (1+ timestamp-space) :end timezone-space))))

(defun deserialize-commit (commit text)
  "Parse TEXT -- the raw text of a Git commit object -- and populate
COMMIT's TREE, PARENTS, AUTHOR, COMMITTER, TIMESTAMP, and MESSAGE
slots from it, using INFLATE-GIT-PROXY (bound to COMMIT's own
REPOSITORY) to lazily construct the TREE and PARENTS proxies. Marks
COMMIT loaded and returns it."
  (multiple-value-bind (header-lines message) (%split-commit-header-and-message text)
    (let ((repository (get-repository commit))
          (tree-sha nil)
          (parent-shas '())
          (author-remainder nil)
          (committer-remainder nil))
      (dolist (line header-lines)
        (let (remainder)
          (cond
            ((setf remainder (%parse-commit-header-line "tree" line))
             (setf tree-sha remainder))
            ((setf remainder (%parse-commit-header-line "parent" line))
             (push remainder parent-shas))
            ((setf remainder (%parse-commit-header-line "author" line))
             (setf author-remainder remainder))
            ((setf remainder (%parse-commit-header-line "committer" line))
             (setf committer-remainder remainder)))))
      (unless tree-sha
        (error "Malformed Git commit object: missing \"tree\" header."))
      (unless author-remainder
        (error "Malformed Git commit object: missing \"author\" header."))
      (unless committer-remainder
        (error "Malformed Git commit object: missing \"committer\" header."))
      (setf parent-shas (nreverse parent-shas))
      (multiple-value-bind (author timestamp) (%parse-commit-signature-line author-remainder)
        (multiple-value-bind (committer committer-timestamp) (%parse-commit-signature-line committer-remainder)
          (declare (ignore committer-timestamp))
          (setf (get-tree commit) (inflate-git-proxy repository tree-sha))
          (setf (get-parents commit)
                (mapcar (lambda (sha) (inflate-git-proxy repository sha)) parent-shas))
          (setf (get-author commit) author)
          (setf (get-committer commit) committer)
          (setf (get-timestamp commit) timestamp)
          (setf (get-message commit) message)
          (setf (get-loaded? commit) t)
          commit)))))
