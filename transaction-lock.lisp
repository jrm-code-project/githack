;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; WITH-REPOSITORY-TRANSACTION-LOCK implements the pessimistic side
;;; of CALL-WITH-GIT-TRANSACTION's :CONFLICT-RESOLUTION :LOCK mode:
;;; repository-wide mutual exclusion for :READ-WRITE transactions, so
;;; that at most one such transaction is ever reading-then-committing
;;; against a given repository at a time.
;;;
;;; Rather than any OS-specific advisory-locking API (e.g. flock(2),
;;; which does not exist on Windows), this uses a plain lock *file*
;;; (`<git-dir>/transaction.lock`, mirroring Git's own convention for
;;; e.g. `index.lock`): CL:OPEN's own :IF-EXISTS NIL together with
;;; :IF-DOES-NOT-EXIST :CREATE atomically creates-or-fails exactly
;;; like a POSIX open(2) with O_CREAT|O_EXCL, which is a standard,
;;; portable lock-file idiom available on every platform ANSI Common
;;; Lisp itself supports.

(defparameter +transaction-lock-poll-interval+ 0.05
  "Seconds WITH-REPOSITORY-TRANSACTION-LOCK sleeps between successive
attempts to acquire a busy repository transaction lock.")

(defparameter +transaction-lock-timeout+ 30
  "Seconds WITH-REPOSITORY-TRANSACTION-LOCK will keep polling to
acquire a busy repository transaction lock before giving up and
signaling TRANSACTION-LOCK-TIMEOUT-ERROR.")

(define-condition transaction-lock-timeout-error (error)
  ((pathname :initarg :pathname :reader get-pathname))
  (:report
   (lambda (condition stream)
     (format stream "Timed out after ~A seconds waiting to acquire the repository transaction lock ~A."
             +transaction-lock-timeout+
             (get-pathname condition))))
  (:documentation
   "Signaled by WITH-REPOSITORY-TRANSACTION-LOCK if some other
process or thread still holds a repository's transaction lock file
after +TRANSACTION-LOCK-TIMEOUT+ seconds of polling for it."))

(defun %transaction-lock-pathname (git-dir-pathname)
  "Return the pathname of GIT-DIR-PATHNAME's own transaction lock
file (\"transaction.lock\", directly under the Git directory,
mirroring Git's own convention for e.g. `index.lock`)."
  (merge-pathnames "transaction.lock" git-dir-pathname))

(defun %acquire-repository-transaction-lock (git-dir-pathname)
  "Block, busy-polling every +TRANSACTION-LOCK-POLL-INTERVAL+ seconds,
until this process can atomically create GIT-DIR-PATHNAME's own
transaction lock file (via CL:OPEN's :IF-EXISTS NIL, which only
succeeds if the file did not already exist), then return that open
output stream, still held open as the lock itself. Signals
TRANSACTION-LOCK-TIMEOUT-ERROR if the lock is still held by someone
else after +TRANSACTION-LOCK-TIMEOUT+ seconds."
  (let ((lock-pathname (%transaction-lock-pathname git-dir-pathname))
        (deadline (+ (get-internal-real-time)
                     (round (* +transaction-lock-timeout+ internal-time-units-per-second)))))
    (loop
      (let ((stream (open lock-pathname
                           :direction :output
                           :if-exists nil
                           :if-does-not-exist :create)))
        (when stream
          (return stream)))
      (when (> (get-internal-real-time) deadline)
        (error 'transaction-lock-timeout-error :pathname lock-pathname))
      (sleep +transaction-lock-poll-interval+))))

(defun %release-repository-transaction-lock (stream git-dir-pathname)
  "Release a lock previously acquired by
%ACQUIRE-REPOSITORY-TRANSACTION-LOCK: close STREAM and delete
GIT-DIR-PATHNAME's own transaction lock file."
  (close stream)
  (ignore-errors (delete-file (%transaction-lock-pathname git-dir-pathname))))

(defmacro with-repository-transaction-lock ((git-dir-pathname) &body body)
  "Evaluate GIT-DIR-PATHNAME once, then hold that Git directory's own
exclusive transaction lock (see %ACQUIRE-REPOSITORY-TRANSACTION-LOCK)
for the entire dynamic extent of BODY, guaranteeing no other GitHack
transaction -- in this process or any other -- opened with
:CONFLICT-RESOLUTION :LOCK against the same repository runs
concurrently with BODY. The lock is released (and its lock file
removed) when BODY exits, whether normally or abnormally."
  (let ((path-var (gensym "GIT-DIR"))
        (stream-var (gensym "LOCK-STREAM")))
    `(let* ((,path-var ,git-dir-pathname)
            (,stream-var (%acquire-repository-transaction-lock ,path-var)))
       (unwind-protect (progn ,@body)
         (%release-repository-transaction-lock ,stream-var ,path-var)))))
