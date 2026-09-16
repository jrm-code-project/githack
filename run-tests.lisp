;;; -*- Mode: Lisp; coding: utf-8; -*-
;;; Loads Quicklisp, quickloads FiveAM, loads/quickloads GITHACK, and
;;; runs the full FiveAM test suite via ASDF:TEST-SYSTEM.

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :fiveam)
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
(asdf:test-system :githack)
