;;; -*- Mode: Lisp; coding: utf-8; -*-

(defpackage "GITHACK-TEST"
  (:use "COMMON-LISP" "FIVEAM")
  (:import-from "GITHACK"
                "GIT-OBJECT"
                "GET-SHA"
                "GET-REPOSITORY"
                "GET-LOADED?"
                "GIT-BLOB"
                "GET-PAYLOAD"
                "SERIALIZE-ATOM"
                "DESERIALIZE-ATOM"
                "GIT-TREE"
                "GET-ENTRIES"
                "INFER-GIT-MODE"
                "SERIALIZE-TREE"
                "DESERIALIZE-TREE"
                "GIT-COMMIT"
                "GET-TREE"
                "GET-PARENTS"
                "GET-AUTHOR"
                "GET-COMMITTER"
                "GET-TIMESTAMP"
                "GET-MESSAGE"
                "SERIALIZE-COMMIT"
                "DESERIALIZE-COMMIT"
                "GIT-TYPE"
                "INFLATE-GIT-PROXY"
                "GIT-BRANCH"
                "GET-NAME"
                "GET-TARGET"
                "GIT-SHOW-REF-SHA"
                "GIT-UPDATE-REF"
                "RESOLVE-BRANCH"
                "UPDATE-BRANCH"
                "GIT-REPOSITORY"
                "GET-PATHNAME"
                "GET-BRANCH"
                "GET-MODE"
                "CALL-WITH-REPOSITORY"
                "GIT-TRANSACTION"
                "GET-GIT-REPOSITORY"
                "GET-TARGET-BRANCH"
                "GET-STATUS"
                "GET-RESULT"
                "GIT-HASH-OBJECT"
                "CALL-WITH-GIT-TRANSACTION"
                "COMMIT-GIT-TRANSACTION"
                "ABORT-GIT-TRANSACTION")
  (:export "GITHACK-SUITE"
           "RUN-GITHACK-TESTS"))

(in-package "GITHACK-TEST")

(def-suite githack-suite
  :description "Top-level suite for all GitHack tests.")

(defun run-githack-tests ()
  "Run every GitHack test and explain the results to
*STANDARD-OUTPUT*. Returns true iff every test passed."
  (run! 'githack-suite))
