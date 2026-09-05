;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Low-level, generic Git object-database I/O primitives shared by
;;; every higher-level proxy that needs to *write* a new object into
;;; Git's object store (GIT-TRANSACTION, PERSISTENT-CONS, ...). Kept
;;; in its own file, loaded early, so that any proxy layer may depend
;;; on it without introducing a load-order cycle.

(defun %unique-temporary-pathname (prefix)
  "Return a pathname, unlikely to collide with any other file, named
PREFIX followed by random hexadecimal digits and a \".tmp\" type,
within the system's default temporary directory."
  (merge-pathnames
   (make-pathname :name (format nil "~A~(~36,10,'0R~)" prefix (random (expt 36 10) (make-random-state t)))
                   :type "tmp")
   (uiop:default-temporary-directory)))

(defun git-hash-object (repository type octets)
  "Shell out to `git hash-object -w -t <TYPE>` against REPOSITORY (a
pathname naming a Git directory), writing OCTETS -- a (VECTOR
(UNSIGNED-BYTE 8)) -- into Git's object database as a new object of
TYPE (\"blob\", \"tree\", or \"commit\"), and return the resulting
40-character hexadecimal SHA."
  (let ((path (%unique-temporary-pathname "githack-object-")))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
             (write-sequence octets stream))
           (string-trim '(#\Space #\Newline #\Return)
                         (uiop:run-program (list "git"
                                                  (format nil "--git-dir=~A" (uiop:native-namestring repository))
                                                  "hash-object" "-w" "-t" type
                                                  (uiop:native-namestring path))
                                            :output :string)))
      (ignore-errors (delete-file path)))))
