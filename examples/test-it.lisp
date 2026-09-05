;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; A small smoke test for GITHACK-EXAMPLE-LIBRARY, exercising it
;;; against the real "database-example" branch. Load the system
;;; first, then LOAD this file:
;;;   (asdf:load-system :githack/example)
;;;   (load "examples/test-it.lisp")

;;; List every book currently in the library, printing its ISBN,
;;; title, author, and checked-out status; remember the first book's
;;; ISBN (in DEFVAR *FIRST-BOOK-ISBN*) so the next two forms can act
;;; on that same book.
(defvar *first-book-isbn*)
(let ((books (githack-example-library:list-books)))
  (dolist (book books)
    (format t "~A: ~A by ~A~@[ [CHECKED OUT]~]~%"
            (githack-example-library::book-isbn book)
            (githack-example-library::book-title book)
            (githack-example-library::book-author book)
            (githack-example-library::book-checked-out-p book)))
  (setf *first-book-isbn* (githack-example-library::book-isbn (first books))))

;;; Check out the first book from that list (marking it
;;; CHECKED-OUT-P true), as its own real Git transaction/commit.
(githack-example-library:checkout-book *first-book-isbn*)

;;; Check that same book back in (marking it CHECKED-OUT-P false
;;; again), as its own real Git transaction/commit.
(githack-example-library:check-in-book *first-book-isbn*)
