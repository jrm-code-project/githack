;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; Kept in its own component (see githack.asd) so that files which
;;; use WITH-FAKE-GIT-TYPE are compiled after it is already loaded,
;;; rather than compiling and using it within the same file.

(defmacro with-fake-git-show-ref-sha ((alist) &body body)
  "Within BODY, GIT-SHOW-REF-SHA returns the SHA associated with a
\(REPOSITORY . NAME) key in ALIST (an alist of ((repository . name)
. sha) conses, compared with EQUAL), or NIL if absent -- exactly
mirroring GIT-SHOW-REF-SHA's real behavior for a nonexistent branch.
The real definition (or lack of one) of GIT-SHOW-REF-SHA is restored
afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL"))
        (table (gensym "TABLE")))
    `(let* ((,table ,alist)
            (,was-bound (fboundp 'git-show-ref-sha))
            (,original (and ,was-bound (fdefinition 'git-show-ref-sha))))
       (setf (fdefinition 'git-show-ref-sha)
             (lambda (repository name)
               (cdr (assoc (cons repository name) ,table :test #'equal))))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-show-ref-sha) ,original)
             (fmakunbound 'git-show-ref-sha))))))

(defmacro with-recording-git-update-ref ((calls-var) &body body)
  "Within BODY, GIT-UPDATE-REF does not shell out to Git; instead
each call pushes a (REPOSITORY NAME SHA) list onto the setf-able
place CALLS-VAR and returns SHA, exactly mimicking GIT-UPDATE-REF's
real return value. The real definition (or lack of one) of
GIT-UPDATE-REF is restored afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL")))
    `(let* ((,was-bound (fboundp 'git-update-ref))
            (,original (and ,was-bound (fdefinition 'git-update-ref))))
       (setf (fdefinition 'git-update-ref)
             (lambda (repository name sha)
               (push (list repository name sha) ,calls-var)
               sha))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-update-ref) ,original)
             (fmakunbound 'git-update-ref))))))

(defun %fake-sha-for (type octets)
  "Deterministically derive a syntactically valid 40-character
hexadecimal string from TYPE and OCTETS, suitable for use as a fake
Git SHA in tests: identical TYPE/OCTETS pairs always produce the
same fake SHA, and different pairs are (with overwhelming
likelihood) different, without ever touching the filesystem or an
external process."
  (let ((hash (mod (logxor (sxhash type) (sxhash (coerce octets 'list)) (length octets))
                    (expt 16 40))))
    (format nil "~(~40,'0X~)" hash)))

(defmacro with-fake-git-hash-object (() &body body)
  "Within BODY, GIT-HASH-OBJECT does not shell out to Git or touch
the filesystem; instead it returns a fake SHA deterministically
derived from its TYPE and OCTETS arguments by %FAKE-SHA-FOR. The
real definition (or lack of one) of GIT-HASH-OBJECT is restored
afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL")))
    `(let* ((,was-bound (fboundp 'git-hash-object))
            (,original (and ,was-bound (fdefinition 'git-hash-object))))
       (setf (fdefinition 'git-hash-object)
             (lambda (repository type octets)
               (declare (ignore repository))
               (%fake-sha-for type octets)))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-hash-object) ,original)
             (fmakunbound 'git-hash-object))))))

(defmacro with-recording-git-hash-object ((calls-var) &body body)
  "Within BODY, GIT-HASH-OBJECT does not shell out to Git or touch
the filesystem; instead each call pushes a (REPOSITORY TYPE OCTETS)
list onto the setf-able place CALLS-VAR and returns a fake SHA
deterministically derived from TYPE and OCTETS by %FAKE-SHA-FOR,
exactly as WITH-FAKE-GIT-HASH-OBJECT's fake SHAs behave. The real
definition (or lack of one) of GIT-HASH-OBJECT is restored
afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL")))
    `(let* ((,was-bound (fboundp 'git-hash-object))
            (,original (and ,was-bound (fdefinition 'git-hash-object))))
       (setf (fdefinition 'git-hash-object)
             (lambda (repository type octets)
               (push (list repository type octets) ,calls-var)
               (%fake-sha-for type octets)))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-hash-object) ,original)
             (fmakunbound 'git-hash-object))))))

(defmacro with-fake-git-cat-file ((sha->octets-alist) &body body)
  "Within BODY, GIT-CAT-FILE returns the octet-vector associated
with a SHA in SHA->OCTETS-ALIST (an alist of (sha . octets) conses,
compared with STRING=), signaling an error for any SHA not present.
The real definition (or lack of one) of GIT-CAT-FILE is restored
afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL"))
        (alist (gensym "ALIST")))
    `(let* ((,alist ,sha->octets-alist)
            (,was-bound (fboundp 'git-cat-file))
            (,original (and ,was-bound (fdefinition 'git-cat-file))))
       (setf (fdefinition 'git-cat-file)
             (lambda (repository sha)
               (declare (ignore repository))
               (or (cdr (assoc sha ,alist :test #'string=))
                   (error "No fake GIT-CAT-FILE mapping for SHA ~S." sha))))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-cat-file) ,original)
             (fmakunbound 'git-cat-file))))))

(defmacro with-fake-git-object-store (() &body body)
  "Within BODY, GIT-HASH-OBJECT and GIT-CAT-FILE cooperate as a
single in-memory fake Git object database, neither shelling out to
Git nor touching the filesystem: GIT-HASH-OBJECT computes a fake SHA
for its TYPE/OCTETS arguments via %FAKE-SHA-FOR and remembers OCTETS
under that SHA; GIT-CAT-FILE returns the OCTETS previously remembered
for a SHA, signaling an error for any SHA never hashed in this way.
The real definitions (or lack thereof) of both functions are
restored afterward."
  (let ((hash-was-bound (gensym "HASH-WAS-BOUND"))
        (hash-original (gensym "HASH-ORIGINAL"))
        (cat-was-bound (gensym "CAT-WAS-BOUND"))
        (cat-original (gensym "CAT-ORIGINAL"))
        (table (gensym "TABLE")))
    `(let* ((,table (make-hash-table :test 'equal))
            (,hash-was-bound (fboundp 'git-hash-object))
            (,hash-original (and ,hash-was-bound (fdefinition 'git-hash-object)))
            (,cat-was-bound (fboundp 'git-cat-file))
            (,cat-original (and ,cat-was-bound (fdefinition 'git-cat-file))))
       (setf (fdefinition 'git-hash-object)
             (lambda (repository type octets)
               (declare (ignore repository))
               (let ((sha (%fake-sha-for type octets)))
                 (setf (gethash sha ,table) octets)
                 sha)))
       (setf (fdefinition 'git-cat-file)
             (lambda (repository sha)
               (declare (ignore repository))
               (multiple-value-bind (octets present?) (gethash sha ,table)
                 (unless present?
                   (error "No fake GIT-CAT-FILE content for SHA ~S." sha))
                 octets)))
       (unwind-protect (progn ,@body)
         (if ,hash-was-bound
             (setf (fdefinition 'git-hash-object) ,hash-original)
             (fmakunbound 'git-hash-object))
         (if ,cat-was-bound
             (setf (fdefinition 'git-cat-file) ,cat-original)
             (fmakunbound 'git-cat-file))))))

(defmacro with-fake-git-type ((type-alist) &body body)
  "Within BODY, GIT-TYPE returns the string associated with a SHA in
TYPE-ALIST (an alist of (sha . type) conses, compared with STRING=),
signaling an error for any SHA not present. The real definition (or
lack of one) of GIT-TYPE is restored afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL"))
        (alist (gensym "ALIST")))
    `(let* ((,alist ,type-alist)
            (,was-bound (fboundp 'git-type))
            (,original (and ,was-bound (fdefinition 'git-type))))
       (setf (fdefinition 'git-type)
             (lambda (repository sha)
               (declare (ignore repository))
               (or (cdr (assoc sha ,alist :test #'string=))
                   (error "No fake GIT-TYPE mapping for SHA ~S." sha))))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-type) ,original)
             (fmakunbound 'git-type))))))
