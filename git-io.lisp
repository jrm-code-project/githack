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

(defun git-cat-file (repository sha)
  "Shell out to `git cat-file -p <SHA>` against REPOSITORY (a
pathname naming a Git directory) and return that Git object's raw,
already-decompressed content as a (VECTOR (UNSIGNED-BYTE 8)). Unlike
GIT-HASH-OBJECT's counterpart write path, the object's content is
captured through a temporary file (rather than a Lisp string) so
that arbitrary binary content -- such as a tree's packed binary SHA
entries -- round-trips exactly, with no character-encoding or
line-ending translation."
  (let ((path (%unique-temporary-pathname "githack-catfile-")))
    (unwind-protect
         (progn
           (uiop:run-program (list "git"
                                    (format nil "--git-dir=~A" (uiop:native-namestring repository))
                                    "cat-file" "-p" sha)
                              :output path
                              :if-output-exists :supersede)
           (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
             (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
               (read-sequence bytes stream)
               bytes)))
      (ignore-errors (delete-file path)))))

(defun %serialize-plist (plist)
  "Encode PLIST (a property list of keyword keys and simple values --
integers, strings, keywords, T, or NIL) as a UTF-8 octet vector,
suitable for storing as the raw, human-readable content of a Git
blob (e.g. a \".meta\" file). *PACKAGE* is bound to the COMMON-LISP
package while printing (rather than KEYWORD, as SERIALIZE-ATOM binds
it) so that a T or NIL value prints as, and later reads back as, the
familiar CL:T/CL:NIL rather than a same-named but distinct keyword."
  (let ((*print-readably* t)
        (*print-circle* nil)
        (*print-pretty* nil)
        (*print-case* :downcase)
        (*package* (find-package "COMMON-LISP")))
    (sb-ext:string-to-octets (prin1-to-string plist) :external-format :utf-8)))

(defun %deserialize-plist (octets)
  "Inverse of %SERIALIZE-PLIST: parse OCTETS -- the raw content of a
Git blob holding an encoded property list -- and return that plist."
  (let ((*read-eval* nil)
        (*package* (find-package "COMMON-LISP")))
    (read-from-string (sb-ext:octets-to-string octets :external-format :utf-8))))
