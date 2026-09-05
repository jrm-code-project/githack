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
each call pushes a (REPOSITORY NAME SHA) list -- or, if called with
an EXPECTED-SHA other than :UNCONDITIONAL, a (REPOSITORY NAME SHA
EXPECTED-SHA) list -- onto the setf-able place CALLS-VAR and returns
SHA, exactly mimicking GIT-UPDATE-REF's real return value. Never
simulates a compare-and-swap failure; see
WITH-CAS-FAILING-GIT-UPDATE-REF for that. The real definition (or
lack of one) of GIT-UPDATE-REF is restored afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL")))
    `(let* ((,was-bound (fboundp 'git-update-ref))
            (,original (and ,was-bound (fdefinition 'git-update-ref))))
       (setf (fdefinition 'git-update-ref)
             (lambda (repository name sha &key (expected-sha :unconditional))
               (push (if (eq expected-sha :unconditional)
                         (list repository name sha)
                         (list repository name sha expected-sha))
                     ,calls-var)
               sha))
       (unwind-protect (progn ,@body)
         (if ,was-bound
             (setf (fdefinition 'git-update-ref) ,original)
             (fmakunbound 'git-update-ref))))))

(defmacro with-cas-failing-git-update-ref ((&key (fail-count 1)) &body body)
  "Within BODY, GIT-UPDATE-REF does not shell out to Git; instead,
whenever it is called with an EXPECTED-SHA other than :UNCONDITIONAL
(i.e. a genuine compare-and-swap attempt), the first FAIL-COUNT such
calls signal CONCURRENT-MODIFICATION-ERROR (simulating some other
writer having already advanced the ref), and every call thereafter
(and any unconditional call, at any time) succeeds and returns SHA,
exactly mimicking GIT-UPDATE-REF's real return value. Suitable for
testing CALL-WITH-GIT-TRANSACTION's :CONFLICT-RESOLUTION :RETRY
mode's re-attempt loop. The real definition (or lack of one) of
GIT-UPDATE-REF is restored afterward."
  (let ((was-bound (gensym "WAS-BOUND"))
        (original (gensym "ORIGINAL"))
        (remaining (gensym "REMAINING")))
    `(let* ((,was-bound (fboundp 'git-update-ref))
            (,original (and ,was-bound (fdefinition 'git-update-ref)))
            (,remaining ,fail-count))
       (setf (fdefinition 'git-update-ref)
             (lambda (repository name sha &key (expected-sha :unconditional))
               (if (and (not (eq expected-sha :unconditional)) (plusp ,remaining))
                   (progn
                     (decf ,remaining)
                     (error 'concurrent-modification-error
                            :repository repository :name name
                            :expected-sha expected-sha :new-sha sha
                            :detail "simulated concurrent writer"))
                   sha)))
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
external process. Folds every byte of OCTETS individually (an
FNV-1a-style hash) rather than hashing OCTETS as a whole via SXHASH,
since SBCL's SXHASH on a list is only required to examine a bounded
prefix and so can (and does, e.g. for octet vectors as short and as
similar as the serialized envelopes of small, adjacent integers)
collide for distinct OCTETS that merely share a long enough common
prefix."
  (let ((hash (logand (sxhash type) #xFFFFFFFFFFFFFFFF)))
    (loop for byte across octets
          do (setf hash (logand (* (logxor hash byte) 1099511628211) #xFFFFFFFFFFFFFFFF)))
    (setf hash (logxor hash (length octets)))
    (format nil "~(~40,'0X~)" (mod hash (expt 16 40)))))

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

(defun %e2e-unique-repository-pathname ()
  "Return a pathname, extremely unlikely to collide with any other
directory, naming a fresh temporary directory (not yet created)
within the system's default temporary directory, suitable to `git
init --bare` a real end-to-end test repository into. Mirrors
GIT-IO.LISP's own %UNIQUE-TEMPORARY-PATHNAME, but names a directory
(a trailing \"/\") rather than a file."
  (merge-pathnames
   (format nil "githack-e2e-~(~36,10,'0R~)/" (random (expt 36 10) (make-random-state t)))
   (uiop:default-temporary-directory)))

(defmacro with-temporary-git-repository ((repository-var) &body body)
  "Create a brand-new, empty, real bare Git repository -- via `git
init --bare`, genuinely shelling out to the Git executable, not any
WITH-FAKE-GIT-*/WITH-RECORDING-GIT-* fixture above -- inside a fresh
temporary directory. Bind REPOSITORY-VAR, for the extent of BODY, to
that repository's pathname (its --git-dir, exactly what
GIT-HASH-OBJECT/GIT-CAT-FILE/GIT-TYPE/GIT-SHOW-REF-SHA/GIT-UPDATE-REF
all expect as their own REPOSITORY argument). Recursively deletes
the entire temporary directory afterward, regardless of how BODY
exits (normally, via a non-local exit, or by signaling an error)."
  (let ((path (gensym "PATH")))
    `(let ((,path (%e2e-unique-repository-pathname)))
       (ensure-directories-exist ,path)
       (uiop:run-program (list "git" "init" "--bare" (uiop:native-namestring ,path))
                          :output nil :error-output nil)
       (unwind-protect
            (let ((,repository-var ,path))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,path :validate t :if-does-not-exist :ignore))))))

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
