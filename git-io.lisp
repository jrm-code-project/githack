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

;;; --- `git` executable sanity check ---
;;;
;;; Every call site below (and in git-branch.lisp) shells out to the
;;; bare executable name "git", with no configurable path. A
;;; misconfigured PATH, a missing Git install, or an unusual shell
;;; environment would otherwise only surface as whatever raw
;;; UIOP:SUBPROCESS-ERROR or OS-level ENOENT the *first* such call
;;; happens to hit, deep in some unrelated stack. %ENSURE-GIT-
;;; AVAILABLE lets CALL-WITH-REPOSITORY check this once, up front,
;;; and fail fast with a clear diagnostic instead.

(defvar *git-available-p* nil
  "True once %ENSURE-GIT-AVAILABLE has confirmed a working `git`
executable is reachable on PATH in this Lisp image. Memoized so the
check only ever shells out once per image (assuming success), not
once per CALL-WITH-REPOSITORY call.")

(defun %ensure-git-available ()
  "Run `git --version` once, memoized in *GIT-AVAILABLE-P*, to
confirm a working `git` executable is reachable on PATH. Signals
GIT-NOT-FOUND-ERROR, with a clear diagnostic message, if `git
--version` cannot be run at all (e.g. no such executable) or exits
with a non-zero status. Returns T on success."
  (or *git-available-p*
      (handler-case
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (list "git" "--version")
                                 :output :string :error-output :string
                                 :ignore-error-status t)
            (if (zerop exit-code)
                (setf *git-available-p* t)
                (error 'git-not-found-error
                       :format-control "`git --version` exited with status ~D; is `git` installed and on PATH?~@[~%~A~]~@[~%~A~]"
                       :format-arguments (list exit-code
                                                (and (plusp (length output)) output)
                                                (and (plusp (length error-output)) error-output)))))
        (git-not-found-error (condition) (error condition))
        (error (condition)
          (error 'git-not-found-error
                 :format-control "Could not run the `git` executable; is it installed and on PATH? (~A)"
                 :format-arguments (list condition))))))

;;; --- Persistent Git subprocess sessions ---
;;;
;;; Every read (GIT-TYPE, GIT-CAT-FILE) and write (GIT-HASH-OBJECT)
;;; below is, in the common case, served by a long-lived `git`
;;; subprocess (`cat-file --batch-check`, `cat-file --batch`, and
;;; `hash-object -w -t <type> --stdin-paths`, respectively) instead
;;; of a brand-new process per call: one line in, one line (or one
;;; line plus a content body) out, so many calls share the same
;;; process's start-up cost. Sessions are created lazily, cached in
;;; *GIT-IO-SESSIONS*, and keyed by both the operation kind and the
;;; repository (and, for writes, the object TYPE, since `git
;;; hash-object` only accepts a single -t per invocation). If a
;;; session ever fails to start, or dies/misbehaves mid-conversation
;;; (broken pipe, unexpected EOF, ...), the offending entry is
;;; discarded and the call falls back to today's one-shot
;;; UIOP:RUN-PROGRAM implementation instead -- so a persistent
;;; session is purely a performance optimization, never a
;;; correctness requirement.

(defvar *git-io-sessions* (make-hash-table :test 'equal)
  "Cache of live, long-lived `git` subprocesses used by GIT-TYPE,
GIT-CAT-FILE, and GIT-HASH-OBJECT to avoid a fresh process spawn per
call. Keyed by a list of the form (KIND REPOSITORY-NATIVE-NAMESTRING
&optional TYPE), mapping to a UIOP process-info object. See
%ENSURE-GIT-IO-SESSION and CLOSE-GIT-IO-SESSIONS.")

(defun %git-io-session-key (kind repository &optional type)
  "Return the key under which a persistent Git subprocess for KIND
(:BATCH-CHECK, :BATCH, or :HASH-OBJECT) against REPOSITORY (and, for
:HASH-OBJECT, the object TYPE) is stored in *GIT-IO-SESSIONS*."
  (list kind (uiop:native-namestring repository) type))

(defun %ensure-git-io-session (key start-thunk)
  "Return the cached, still-alive UIOP process-info stored under KEY
in *GIT-IO-SESSIONS*, or call START-THUNK (a function of no
arguments returning a fresh process-info) to create, cache, and
return a new one if none is cached yet or the cached one has died."
  (let ((existing (gethash key *git-io-sessions*)))
    (if (and existing (uiop:process-alive-p existing))
        existing
        (setf (gethash key *git-io-sessions*) (funcall start-thunk)))))

(defun %discard-git-io-session (key)
  "Forcibly terminate and forget the session cached under KEY in
*GIT-IO-SESSIONS*, if any, so the next call for KEY starts a fresh
process instead of reusing a broken one."
  (let ((process (gethash key *git-io-sessions*)))
    (when process
      (remhash key *git-io-sessions*)
      (ignore-errors (uiop:terminate-process process :urgent t))
      (ignore-errors (uiop:close-streams process)))))

(defun close-git-io-sessions (&optional repository)
  "Terminate and forget every persistent Git subprocess session
cached in *GIT-IO-SESSIONS* for REPOSITORY, or every cached session
at all if REPOSITORY is NIL. Callers that create many short-lived
repositories (e.g. the test suite's WITH-TEMPORARY-GIT-REPOSITORY)
must call this before deleting a repository's directory, since a
live session's pipes silently outlive the directory they were
opened against otherwise, leaking OS processes."
  (let ((native (and repository (uiop:native-namestring repository))))
    (loop for key being the hash-keys of *git-io-sessions*
          when (or (null native) (equal native (second key)))
            do (%discard-git-io-session key))))

(defun %git-io-read-line-of-octets (stream)
  "Read octets from STREAM (an (UNSIGNED-BYTE 8) stream) up to and
excluding the next newline byte, decode them as UTF-8, and return
the resulting string, or NIL if STREAM is already at end of file
with no bytes read (signaling that the subprocess on the other end
has exited)."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil :eof)
          do (cond
               ((eq byte :eof) (return (and (plusp (fill-pointer bytes))
                                             (sb-ext:octets-to-string bytes :external-format :utf-8))))
               ((= byte 10) (return (sb-ext:octets-to-string bytes :external-format :utf-8)))
               (t (vector-push-extend byte bytes))))))

(defun %git-io-write-line-of-octets (stream string)
  "Write STRING to STREAM (an (UNSIGNED-BYTE 8) stream) as UTF-8
octets followed by a single newline byte, and flush STREAM so the
subprocess reading it sees the line immediately."
  (write-sequence (sb-ext:string-to-octets string :external-format :utf-8) stream)
  (write-byte 10 stream)
  (force-output stream))

(defun %git-io-read-exact-octets (stream n)
  "Read and return exactly N octets from STREAM as a fresh (VECTOR
(UNSIGNED-BYTE 8)). Signals an error (via READ-SEQUENCE's own
short-read detection) if STREAM reaches end of file first."
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (let ((read (read-sequence bytes stream)))
      (unless (= read n)
        (error "Expected ~D octets from a `git` subprocess session but only received ~D." n read)))
    bytes))

(defun %start-git-batch-check-session (repository)
  "Launch a persistent `git cat-file --batch-check` subprocess
against REPOSITORY, returning its UIOP process-info. Given a SHA
line on its input, it writes back a single \"<sha> <type> <size>\"
line (or \"<sha> missing\") on its output -- exactly what GIT-TYPE
needs, without ever transferring the object's content body."
  (uiop:launch-program (list "git"
                              (format nil "--git-dir=~A" (uiop:native-namestring repository))
                              "cat-file" "--batch-check")
                        :input :stream :output :stream
                        :element-type '(unsigned-byte 8)))

(defun %start-git-batch-session (repository)
  "Launch a persistent `git cat-file --batch` subprocess against
REPOSITORY, returning its UIOP process-info. Given a SHA line on its
input, it writes back a \"<sha> <type> <size>\" header line followed
by exactly <size> octets of raw content and a trailing newline (or
\"<sha> missing\" with no body) -- exactly what GIT-CAT-FILE needs,
in a single round-trip with no separate GIT-TYPE call first."
  (uiop:launch-program (list "git"
                              (format nil "--git-dir=~A" (uiop:native-namestring repository))
                              "cat-file" "--batch")
                        :input :stream :output :stream
                        :element-type '(unsigned-byte 8)))

(defun %start-git-hash-object-session (repository type)
  "Launch a persistent `git hash-object -w -t TYPE --stdin-paths`
subprocess against REPOSITORY, returning its UIOP process-info. It
reads one file path per input line and writes back that file's
newly-hashed-and-written SHA per output line. Bound to a single TYPE
for its whole lifetime, since Git does not support mixing object
types within one `hash-object` invocation."
  (uiop:launch-program (list "git"
                              (format nil "--git-dir=~A" (uiop:native-namestring repository))
                              "hash-object" "-w" "-t" type "--stdin-paths")
                        :input :stream :output :stream
                        :element-type '(unsigned-byte 8)))

(defun %parse-batch-header (header sha)
  "Parse HEADER -- a \"<sha> <type> <size>\" line read back from a
`git cat-file --batch`/`--batch-check` session -- and return (VALUES
TYPE SIZE). Signals MALFORMED-GIT-OBJECT-ERROR if HEADER instead
reports that SHA is missing from the repository, or a plain error
(triggering the caller's one-shot fallback) if HEADER is NIL (the
session already exited) or otherwise unparseable."
  (unless header
    (error "Git batch session produced no output for SHA ~A (process may have exited)." sha))
  (when (search " missing" header)
    (error 'malformed-git-object-error
           :format-control "Git object ~S does not exist."
           :format-arguments (list sha)))
  (let* ((space1 (position #\Space header))
         (space2 (and space1 (position #\Space header :start (1+ space1)))))
    (unless space2
      (error "Unparseable `git cat-file` batch header ~S for SHA ~A." header sha))
    (values (subseq header (1+ space1) space2)
            (parse-integer header :start (1+ space2)))))

(defun %git-type-via-batch-check (repository sha)
  "Look up SHA's object type using REPOSITORY's persistent `git
cat-file --batch-check` session, starting one if necessary. See
GIT-TYPE."
  (let* ((key (%git-io-session-key :batch-check repository))
         (process (%ensure-git-io-session key (lambda () (%start-git-batch-check-session repository)))))
    (%git-io-write-line-of-octets (uiop:process-info-input process) sha)
    (%parse-batch-header (%git-io-read-line-of-octets (uiop:process-info-output process)) sha)))

(defun %git-type-one-shot (repository sha)
  "Shell out to a fresh `git cat-file -t <SHA>` process against
REPOSITORY and return that Git object's type as a string. The
one-shot fallback GIT-TYPE uses if its persistent session is
unavailable or misbehaves."
  (string-trim '(#\Space #\Newline #\Return)
               (uiop:run-program (list "git"
                                        (format nil "--git-dir=~A" (uiop:native-namestring repository))
                                        "cat-file" "-t" sha)
                                  :output :string)))

(defun git-type (repository sha)
  "Return SHA's Git object type as a string: \"blob\", \"tree\", or
\"commit\", against REPOSITORY (a pathname naming a Git directory).
Normally served by a single round-trip to REPOSITORY's persistent
`git cat-file --batch-check` session; transparently falls back to a
one-shot `git cat-file -t` subprocess if that session cannot be
started or misbehaves. See INFLATE-GIT-PROXY, which dispatches on
this to choose the concrete proxy subclass for SHA."
  (handler-case
      (%git-type-via-batch-check repository sha)
    (githack-error (condition) (error condition))
    (error ()
      (%discard-git-io-session (%git-io-session-key :batch-check repository))
      (%git-type-one-shot repository sha))))

(defun %git-cat-file-via-batch (repository sha)
  "Fetch SHA's raw object content using REPOSITORY's persistent `git
cat-file --batch` session, starting one if necessary. See
GIT-CAT-FILE."
  (let* ((key (%git-io-session-key :batch repository))
         (process (%ensure-git-io-session key (lambda () (%start-git-batch-session repository))))
         (output (uiop:process-info-output process)))
    (%git-io-write-line-of-octets (uiop:process-info-input process) sha)
    (multiple-value-bind (type size) (%parse-batch-header (%git-io-read-line-of-octets output) sha)
      (declare (ignore type))
      (prog1 (%git-io-read-exact-octets output size)
        ;; consume the single trailing newline `--batch` appends after the content
        (read-byte output nil nil)))))

(defun %git-cat-file-one-shot (repository sha)
  "Shell out to a fresh `git cat-file <type> <SHA>` process against
REPOSITORY (first determining SHA's own type via GIT-TYPE) and
return that Git object's raw, already-decompressed content as a
\(VECTOR (UNSIGNED-BYTE 8)). An explicit TYPE, rather than `-p`
\(pretty-print), is essential here: `git cat-file -p` reformats a
TREE object into a human-readable directory listing, silently
corrupting the strict packed binary format DESERIALIZE-TREE (and
every DESERIALIZE-PERSISTENT-* built upon it) requires, whereas `git
cat-file tree <sha>` returns that same packed binary content
byte-for-byte unchanged (exactly as `-p`/<type> both already do,
identically, for a BLOB or COMMIT). The object's content is captured
through a temporary file (rather than a Lisp string) so that
arbitrary binary content -- such as a tree's packed binary SHA
entries -- round-trips exactly, with no character-encoding or
line-ending translation. The one-shot fallback GIT-CAT-FILE uses if
its persistent session is unavailable or misbehaves."
  (let ((path (%unique-temporary-pathname "githack-catfile-"))
        (type (git-type repository sha)))
    (unwind-protect
         (progn
           (uiop:run-program (list "git"
                                    (format nil "--git-dir=~A" (uiop:native-namestring repository))
                                    "cat-file" type sha)
                              :output path
                              :if-output-exists :supersede)
           (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
             (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
               (read-sequence bytes stream)
               bytes)))
      (ignore-errors (delete-file path)))))

(defun git-cat-file (repository sha)
  "Return SHA's raw, already-decompressed Git object content as a
\(VECTOR (UNSIGNED-BYTE 8)), against REPOSITORY (a pathname naming a
Git directory). Normally served by a single round-trip to
REPOSITORY's persistent `git cat-file --batch` session (one process
handles both type lookup and content fetch, unlike the one-shot
fallback below, which needs a separate GIT-TYPE call first);
transparently falls back to a one-shot `git cat-file <type>`
subprocess if that session cannot be started or misbehaves."
  (handler-case
      (%git-cat-file-via-batch repository sha)
    (githack-error (condition) (error condition))
    (error ()
      (%discard-git-io-session (%git-io-session-key :batch repository))
      (%git-cat-file-one-shot repository sha))))

(defun %git-hash-object-via-session (repository type path)
  "Hash and write the file at PATH into REPOSITORY's object database
as a new object of TYPE, using REPOSITORY/TYPE's persistent `git
hash-object -w -t TYPE --stdin-paths` session (starting one if
necessary), and return the resulting 40-character hexadecimal SHA.
See GIT-HASH-OBJECT."
  (let* ((key (%git-io-session-key :hash-object repository type))
         (process (%ensure-git-io-session key (lambda () (%start-git-hash-object-session repository type)))))
    (%git-io-write-line-of-octets (uiop:process-info-input process) (uiop:native-namestring path))
    (let ((sha (%git-io-read-line-of-octets (uiop:process-info-output process))))
      (unless sha
        (error "Git hash-object session for type ~A produced no output (process may have exited)." type))
      (string-trim '(#\Space #\Return) sha))))

(defun %git-hash-object-one-shot (repository type path)
  "Shell out to a fresh `git hash-object -w -t <TYPE> <PATH>` process
against REPOSITORY and return the resulting 40-character hexadecimal
SHA. The one-shot fallback GIT-HASH-OBJECT uses if its persistent
session is unavailable or misbehaves."
  (string-trim '(#\Space #\Newline #\Return)
               (uiop:run-program (list "git"
                                        (format nil "--git-dir=~A" (uiop:native-namestring repository))
                                        "hash-object" "-w" "-t" type
                                        (uiop:native-namestring path))
                                  :output :string)))

(defun git-hash-object (repository type octets)
  "Write OCTETS -- a (VECTOR (UNSIGNED-BYTE 8)) -- into REPOSITORY's
\(a pathname naming a Git directory) object database as a new object
of TYPE (\"blob\", \"tree\", or \"commit\"), and return the resulting
40-character hexadecimal SHA. OCTETS is always staged through a
temporary file first (as Git's `hash-object` requires a real file
path, not stdin content, for `--stdin-paths`); normally served by a
round-trip to a persistent, TYPE-specific `git hash-object
--stdin-paths` session, transparently falling back to a one-shot
`git hash-object` subprocess if that session cannot be started or
misbehaves."
  (let ((path (%unique-temporary-pathname "githack-object-")))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
             (write-sequence octets stream))
           (handler-case
               (%git-hash-object-via-session repository type path)
             (githack-error (condition) (error condition))
             (error ()
               (%discard-git-io-session (%git-io-session-key :hash-object repository type))
               (%git-hash-object-one-shot repository type path))))
      (ignore-errors (delete-file path)))))

(defun %git-merge-tree (repository commit-a commit-b)
  "Shell out to `git merge-tree --write-tree COMMIT-A COMMIT-B`
against REPOSITORY (a pathname naming a Git directory): a real,
working-tree-free three-way content merge of the two commits via
Git's own native merge machinery, which locates their common
ancestor automatically from the commit graph (no explicit merge-base
tree need be supplied) and requires no checkout/working tree/index
of any kind -- safe to run directly against a bare repository.

On a clean merge (Git's own exit code 0), returns the resulting
merged tree's 40-character hexadecimal SHA-1 as its first value
(Git prints it as the first line of output), and NIL as its second.

On any conflict (a non-zero exit code), returns NIL as its first
value and Git's own textual conflict report (combined stdout and
stderr) as its second, for a caller to fold into a MERGE-CONFLICT-
ERROR's own DETAIL.

Used by GIT-TRANSACTION.LISP's :REBASE CONFLICT-RESOLUTION strategy
to replay a transaction's own candidate commit onto a concurrently-
advanced branch HEAD instead of discarding it outright."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "git"
                               (format nil "--git-dir=~A" (uiop:native-namestring repository))
                               "merge-tree" "--write-tree" commit-a commit-b)
                         :output :string :error-output :string :ignore-error-status t)
    (if (zerop exit-code)
        (values (string-trim '(#\Space #\Newline #\Return)
                              (subseq output 0 (or (position #\Newline output) (length output))))
                nil)
        (values nil (concatenate 'string output error-output)))))

(defun %close-all-git-io-sessions-at-exit ()
  "SB-EXT:*EXIT-HOOKS* entry: terminate every persistent Git
subprocess session still cached in *GIT-IO-SESSIONS*. A defensive
backstop for callers (e.g. WITH-TEMPORARY-GIT-REPOSITORY) that
forgot to call CLOSE-GIT-IO-SESSIONS themselves, so no `git`
subprocess outlives the Lisp process that spawned it."
  (close-git-io-sessions))

;;; Registered by symbol, not a fresh closure, so reloading this file
;;; (e.g. during interactive development) does not accumulate
;;; duplicate entries -- PUSHNEW's default #'EQL test recognizes the
;;; same symbol across reloads.
(pushnew '%close-all-git-io-sessions-at-exit sb-ext:*exit-hooks*)

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
