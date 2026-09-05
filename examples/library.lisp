;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; GITHACK-EXAMPLE-LIBRARY: a small, self-hosting example application
;;; demonstrating GitHack by using the GitHack repository's OWN working
;;; copy as its persistent database. All state lives on the orphan
;;; "database-example" branch, so it can never tangle with GitHack's own
;;; source-code history on "main" (or whatever branch you happen to have
;;; checked out) -- the two branches share only their object database,
;;; not any commit ancestry.
;;;
;;; To run:
;;;   (asdf:load-system :githack/example)
;;;   (githack-example-library::populate-satirical-library)
;;; Then open your terminal and run: git log database-example

(defpackage "GITHACK-EXAMPLE-LIBRARY"
  (:use "COMMON-LISP")
  (:import-from "GITHACK"
                "DEFINE-PERSISTENT-STRUCT"
                "PERSISTENT-HASH-TABLE"
                "PHASH-MAKE"
                "PHASH-GET"
                "PHASH-PUT"
                "PHASH-REMOVE"
                "DESERIALIZE-PERSISTENT-OBJECT"
                "WITH-REPOSITORY"
                "WITH-TRANSACTION")
  (:export "GET-GITHACK-REPO-PATH"
           "ENSURE-LIBRARY-INITIALIZED"
           "ADD-BOOK"
           "CHECKOUT-BOOK"
           "POPULATE-SATIRICAL-LIBRARY"))

(in-package "GITHACK-EXAMPLE-LIBRARY")

(defparameter +library-branch+ "database-example"
  "The orphan branch every GITHACK-EXAMPLE-LIBRARY transaction reads
from and writes to. Deliberately never \"main\": the very first
transaction against this branch name creates a parentless (orphan)
root commit -- see CALL-WITH-GIT-TRANSACTION's ORPHAN-COMMIT
GUARANTEE comment in githack's own git-transaction.lisp -- so this
example's data can never share commit ancestry with GitHack's own
source history.")

(defparameter +library-signature+ "GitHack Example Library <library@githack.local>"
  "The AUTHOR/COMMITTER signature every GITHACK-EXAMPLE-LIBRARY
transaction commits under.")

(define-persistent-struct book
  (isbn "")
  (title "")
  (author "")
  (checked-out-p nil))

(define-persistent-struct library
  (catalog nil))

(defun get-githack-repo-path ()
  "Return the pathname of GitHack's own `.git` directory -- the
--git-dir every WITH-REPOSITORY/WITH-TRANSACTION call in this file
targets -- derived from ASDF's own record of where the :GITHACK
system's source lives. GitHack's working copy is an ordinary
(non-bare) repository, so its Git directory is \".git\" underneath
its source directory, not the source directory itself."
  (merge-pathnames ".git/" (asdf:system-source-directory :githack)))

(defun %resolve-persistent-object (value)
  "Return a live, fully-typed CLOS instance for VALUE -- a raw Lisp
value as handed to a CALL-WITH-TRANSACTION receiver (see
%TRANSACTION-READ-VALUE in transaction.lisp) or fetched via PHASH-GET
from a catalog that was itself read this way. VALUE is NIL if no
such object exists yet (an uninitialized \"database-example\"
branch, or a bucket search that legitimately came up empty
beforehand), in which case NIL is returned unchanged; otherwise VALUE
is always a hollow GIT-OBJECT proxy that has never yet been resolved
into its real class (BOOK, LIBRARY, ...), so DESERIALIZE-PERSISTENT-
OBJECT is unconditionally applied to it. Never call this on an object
this same transaction attempt already constructed itself in memory
-- such an object is already properly typed and has no SHA yet for
DESERIALIZE-PERSISTENT-OBJECT to fetch."
  (and value (deserialize-persistent-object value)))

(defun ensure-library-initialized ()
  "Idempotently ensure a LIBRARY (with an empty CATALOG) exists as
the root object of the \"database-example\" branch's head commit,
creating one -- via a genuine orphan root commit, since this branch
will not yet exist the very first time this is called -- if it does
not already. Uses :RETRY conflict resolution, so this is safe to call
concurrently from multiple threads/processes; does nothing but
return the existing LIBRARY if one is already there."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +library-branch+
                                  :author +library-signature+
                                  :message "Initialize the satirical library."
                                  :conflict-resolution :retry)
        (or (%resolve-persistent-object value)
            (make-instance 'library
                           :repository repository-path
                           :catalog (phash-make :repository repository-path :test 'equal)))))))

(defun add-book (isbn title author-name)
  "Add a new, not-yet-checked-out BOOK (ISBN/TITLE/AUTHOR-NAME) to
the LIBRARY's CATALOG on the \"database-example\" branch, as a
single real Git transaction (:RETRY conflict resolution). Signals an
error (via GITHACK's own transaction machinery) if the library has
never been initialized (see ENSURE-LIBRARY-INITIALIZED) -- there is
then no CATALOG to add ISBN to."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +library-branch+
                                  :author +library-signature+
                                  :message (format nil "Add book ~S (ISBN ~A) by ~A." title isbn author-name)
                                  :conflict-resolution :retry)
        (let* ((library (or (%resolve-persistent-object value)
                             (error "Cannot add ISBN ~S: the library has not been initialized yet. Call ENSURE-LIBRARY-INITIALIZED first." isbn)))
               (book (make-instance 'book
                                    :repository repository-path
                                    :isbn isbn
                                    :title title
                                    :author author-name
                                    :checked-out-p nil))
               (catalog (phash-put isbn book (library-catalog library))))
          (make-instance 'library :repository repository-path :catalog catalog))))))

(defun checkout-book (isbn)
  "Mark the BOOK named ISBN in the \"database-example\" branch's
LIBRARY catalog as checked out (CHECKED-OUT-P true), as a single real
Git transaction (:RETRY conflict resolution). Signals an error if the
library has never been initialized, or if ISBN is not present in its
catalog."
  (let ((repository-path (get-githack-repo-path)))
    (with-repository (repository) (repository-path :mode :read-write)
      (with-transaction (value) (repository :read-write
                                  :branch +library-branch+
                                  :author +library-signature+
                                  :message (format nil "Check out book ISBN ~A." isbn)
                                  :conflict-resolution :retry)
        (let ((library (or (%resolve-persistent-object value)
                            (error "Cannot check out ISBN ~S: the library has not been initialized yet. Call ENSURE-LIBRARY-INITIALIZED first." isbn))))
          (multiple-value-bind (raw-book found?) (phash-get isbn (library-catalog library))
            (unless found?
              (error "Cannot check out ISBN ~S: no such book in the library's catalog." isbn))
            (let* ((book (%resolve-persistent-object raw-book))
                   (checked-out-book (make-instance 'book
                                                    :repository repository-path
                                                    :isbn (book-isbn book)
                                                    :title (book-title book)
                                                    :author (book-author book)
                                                    :checked-out-p t))
                   (catalog (phash-put isbn checked-out-book (library-catalog library))))
              (make-instance 'library :repository repository-path :catalog catalog))))))))

;;; --- Satirical seed data ---------------------------------------------

(defparameter +satirical-books+
  '(("978-001" "Git Push --Force: A Memoir of Regret" "Linus Torvalds")
    ("978-002" "Parentheses and Pain: Navigating Lisp" "John McCarthy's Ghost")
    ("978-003" "Lost Updates and Found Sanity: An Optimistic Guide" "Concurrency Carl")
    ("978-004" "Fifty Shades of Beige: Surviving the Helpful Assistant" "Anonymous Corporate Drone")
    ("978-005" "To Kill a Mocking-Object: TDD for the Soulless" "Enterprise Architect Bob")
    ("978-006" "The Ouroboros Hack: Eating Your Own Repository" "The Boss"))
  "ISBN/TITLE/AUTHOR triples POPULATE-SATIRICAL-LIBRARY seeds the
\"database-example\" branch's LIBRARY catalog with.")

(defun populate-satirical-library ()
  "Ensure the \"database-example\" library exists (see ENSURE-
LIBRARY-INITIALIZED), then ADD-BOOK every entry of
+SATIRICAL-BOOKS+ to it, one real Git transaction (and so one real
Git commit) per book. Safe to call more than once: re-adding an ISBN
already present simply replaces that entry with an identical one."
  (ensure-library-initialized)
  (dolist (entry +satirical-books+)
    (destructuring-bind (isbn title author-name) entry
      (add-book isbn title author-name)))
  (values))
