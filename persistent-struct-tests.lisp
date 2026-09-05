;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-struct-suite
  :in githack-suite
  :description "Tests for DEFINE-PERSISTENT-STRUCT and its generated PERSISTENT-STANDARD-CLASS/constructor/predicate.")

(in-suite persistent-struct-suite)

(define-persistent-struct (struct-widget :conc-name sw-)
  (id 0 :type integer)
  (name "Unknown")
  (cache nil :transient t)
  (serial-number 0 :read-only t))

(define-persistent-struct (struct-bare :conc-name nil)
  label)

(test define-persistent-struct-generates-a-persistent-standard-class
  "DEFINE-PERSISTENT-STRUCT's generated class inherits from
PERSISTENT-STRUCT (and so, transitively, PERSISTENT-OBJECT and
GIT-TREE) and uses the PERSISTENT-STANDARD-CLASS metaclass."
  (let ((class (find-class 'struct-widget)))
    (is (typep class 'persistent-standard-class))
    (is (subtypep class (find-class 'persistent-struct)))
    (is (subtypep class (find-class 'persistent-object)))
    (is (subtypep class (find-class 'git-tree)))))

(test define-persistent-struct-generates-conc-name-accessors
  "DEFINE-PERSISTENT-STRUCT generates an accessor per slot, named by
concatenating :CONC-NAME with the slot's own name."
  (let ((widget (make-instance 'struct-widget :repository :dummy-repo :id 7 :name "Bob")))
    (is (= 7 (sw-id widget)))
    (is (string= "Bob" (sw-name widget)))
    (setf (sw-name widget) "Alice")
    (is (string= "Alice" (sw-name widget)))))

(test define-persistent-struct-honors-explicit-nil-conc-name
  "A struct defined with an explicit :CONC-NAME NIL generates
accessors with no prefix at all -- exactly the slot's own name."
  (let ((bare (make-instance 'struct-bare :repository :dummy-repo :label :x)))
    (is (eq :x (label bare)))))

(test define-persistent-struct-read-only-slot-has-no-setf-accessor
  "A slot declared :READ-ONLY T generates only a :READER, never a
setf-able :ACCESSOR."
  (let ((widget (make-instance 'struct-widget :repository :dummy-repo :serial-number 42)))
    (is (= 42 (sw-serial-number widget)))
    (is (not (fboundp '(setf sw-serial-number))))))

(test make-struct-generates-constructor-with-slot-defaults
  "MAKE-<NAME>, DEFINE-PERSISTENT-STRUCT's generated constructor,
accepts a keyword argument per slot, defaulting to each slot's own
DEFAULT-INITFORM, and calls MAKE-INSTANCE."
  (let ((widget (make-struct-widget :name "Carol")))
    (is (typep widget 'struct-widget))
    (is (= 0 (sw-id widget)))
    (is (string= "Carol" (sw-name widget)))))

(test struct-predicate-uses-typep
  "<NAME>-P, DEFINE-PERSISTENT-STRUCT's generated predicate, is true
for an instance of the generated class and false for anything else."
  (is (struct-widget-p (make-struct-widget)))
  (is (not (struct-widget-p 42)))
  (is (not (struct-widget-p (make-struct-bare)))))

(test define-persistent-struct-transient-slot-is-excluded-from-serialization
  "A slot declared :TRANSIENT T is passed through to the generated
CLOS slot specifier, so SERIALIZE-PERSISTENT-OBJECT excludes it from
the resulting Git tree exactly as any other transient slot."
  (let ((widget (make-instance 'struct-widget :repository :dummy-repo
                                :id 1 :name "Bob" :cache :some-cache :serial-number 1)))
    (with-fake-git-hash-object ()
      (serialize-persistent-object widget))
    (is (equal (list ".meta" "id" "name" "serial-number")
               (mapcar #'car (get-entries widget))))))
