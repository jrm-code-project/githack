;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Serialization of a GIT-TREE proxy's ENTRIES alist to/from the
;;; exact binary format Git itself uses for a tree object.
;;;
;;; A Git tree's raw payload is a concatenation of entries, each of
;;; the form:
;;;
;;;   <mode-string> <space> <filename> <NUL> <20-raw-binary-byte SHA>
;;;
;;; with no separator between entries and no trailing terminator.
;;; The 20-byte SHA is the raw binary digest, not the familiar
;;; 40-character hex string, and entries must appear in sorted
;;; order for the tree's own SHA to hash consistently with any other
;;; Git implementation.

(defun %hex-digit->integer (char)
  "Return the integer 0-15 that hex digit CHAR denotes, signaling an
error if CHAR is not a valid hex digit."
  (or (digit-char-p char 16)
      (error 'malformed-git-object-error
             :format-control "~S is not a valid hexadecimal digit."
             :format-arguments (list char))))

(defun %sha-hex->bytes (hex)
  "Convert the 40-character hexadecimal SHA-1 string HEX into the
raw 20-byte binary octet vector Git itself uses inside tree entries."
  (unless (= (length hex) 40)
    (error 'malformed-git-object-error
           :format-control "Invalid SHA string ~S: must be exactly 40 characters."
           :format-arguments (list hex)))
  (let ((bytes (make-array 20 :element-type '(unsigned-byte 8))))
    (dotimes (i 20 bytes)
      (setf (aref bytes i)
            (+ (* 16 (%hex-digit->integer (char hex (* 2 i))))
               (%hex-digit->integer (char hex (1+ (* 2 i)))))))))

(defun %sha-bytes->hex (bytes start)
  "Convert the 20 raw binary SHA-1 octets found in BYTES starting at
START into the familiar lowercase 40-character hexadecimal string."
  (let ((hex (make-string 40)))
    (dotimes (i 20 hex)
      (let ((byte (aref bytes (+ start i))))
        (setf (char hex (* 2 i)) (char-downcase (digit-char (ldb (byte 4 4) byte) 16)))
        (setf (char hex (1+ (* 2 i))) (char-downcase (digit-char (ldb (byte 4 0) byte) 16)))))))

(defgeneric infer-git-mode (git-object)
  (:documentation
   "Return the Git permission-mode string appropriate for the
concrete type of GIT-OBJECT: \"40000\" for a GIT-TREE (directory),
or \"100644\" for a GIT-BLOB (regular file)."))

(defmethod infer-git-mode ((git-object git-tree))
  "40000")

(defmethod infer-git-mode ((git-object git-blob))
  "100644")

(defun %tree-sort-key (name object)
  "Return the string Git itself sorts tree entries by: NAME as-is
for a GIT-BLOB, or NAME with a trailing \"/\" appended for a
GIT-TREE, since Git compares directory entries as though their
names were suffixed with a path separator."
  (if (typep object 'git-tree)
      (concatenate 'string name "/")
      name))

(defun %sort-tree-entries (entries)
  "Return a new list of ENTRIES (each a (FILENAME . GIT-OBJECT)
cons) sorted into the exact order Git requires for tree hashing."
  (sort (copy-list entries)
        #'string<
        :key (lambda (entry) (%tree-sort-key (car entry) (cdr entry)))))

(defun %tree-entry->octets (name object)
  "Return the raw byte-vector encoding of one tree entry: NAME's
associated OBJECT's inferred mode, a space, NAME itself, a NUL byte,
and OBJECT's SHA packed into 20 raw binary bytes."
  (let ((sha (sha object)))
    (unless sha
      (error 'unpersisted-object-error
             :format-control "Cannot serialize tree entry ~S: its GIT-OBJECT has no SHA (not yet persisted)."
             :format-arguments (list name)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (sb-ext:string-to-octets (infer-git-mode object) :external-format :utf-8)
                 #(32)
                 (sb-ext:string-to-octets name :external-format :utf-8)
                 #(0)
                 (%sha-hex->bytes sha))))

(defun serialize-tree (tree)
  "Compile TREE's ENTRIES alist into the exact binary format Git
uses for a tree object: entries sorted by filename, each formatted
as <mode> <space> <filename> <NUL> <20-byte binary SHA>."
  (apply #'concatenate
         '(simple-array (unsigned-byte 8) (*))
         (mapcar (lambda (entry) (%tree-entry->octets (car entry) (cdr entry)))
                 (%sort-tree-entries (get-entries tree)))))

(defun %read-tree-entry (octets start)
  "Parse one tree entry out of OCTETS beginning at START. Returns
three values: the entry's filename, its 40-character hexadecimal
SHA, and the index in OCTETS immediately following the entry."
  (let* ((space (position 32 octets :start start))
         (nul (position 0 octets :start space))
         (sha-end (+ nul 1 20)))
    (unless (and space nul (<= sha-end (length octets)))
      (error 'malformed-git-object-error
             :format-control "Malformed Git tree entry at offset ~D."
             :format-arguments (list start)))
    (values (sb-ext:octets-to-string octets :start (1+ space) :end nul :external-format :utf-8)
            (%sha-bytes->hex octets (1+ nul))
            sha-end)))

(defun deserialize-tree (repository octets)
  "Parse the raw byte-vector OCTETS of a Git tree object into an
alist of (FILENAME . GIT-OBJECT) pairs, using INFLATE-GIT-PROXY to
lazily construct the correct GIT-BLOB or GIT-TREE proxy (bound to
REPOSITORY) for each entry's SHA."
  (loop with length = (length octets)
        with start = 0
        while (< start length)
        collect (multiple-value-bind (name sha next) (%read-tree-entry octets start)
                  (setf start next)
                  (cons name (inflate-git-proxy repository sha)))))
