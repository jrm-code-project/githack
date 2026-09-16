;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite query-engine-suite
  :in githack-suite
  :description "Tests for the QUERY declarative query engine over GitHack persistent collections.")

(in-suite query-engine-suite)

(define-persistent-struct qe-person
  (name "")
  (age 0)
  (city ""))

(defun qe-make-people ()
  (list (make-instance 'qe-person :repository :dummy-repo :name "Alice" :age 30 :city "Boston")
        (make-instance 'qe-person :repository :dummy-repo :name "Bob" :age 25 :city "Austin")
        (make-instance 'qe-person :repository :dummy-repo :name "Carol" :age 35 :city "Boston")
        (make-instance 'qe-person :repository :dummy-repo :name "Dave" :age 25 :city "Austin")))

(test query-basic-from-select
  "A single :FROM/:SELECT clause pair projects every element of the
source list."
  (let ((people (qe-make-people)))
    (is (equal '("Alice" "Bob" "Carol" "Dave")
               (query (:from p people) (:select (qe-person-name p)))))))

(test query-default-select-is-the-sole-var
  "Omitting :SELECT with a single :FROM clause returns the bound
variable itself, unprojected."
  (let ((people (qe-make-people)))
    (is (every (lambda (p) (typep p 'qe-person)) (query (:from p people))))))

(test query-where-filters-rows
  "A :WHERE clause keeps only rows for which its predicate is true."
  (let ((people (qe-make-people)))
    (is (equal '("Bob" "Dave")
               (query (:from p people)
                      (:where (= (qe-person-age p) 25))
                      (:select (qe-person-name p)))))))

(test query-multiple-where-clauses-are-anded
  "Multiple :WHERE clauses are implicitly ANDed together."
  (let ((people (qe-make-people)))
    (is (equal '("Carol")
               (query (:from p people)
                      (:where (= (qe-person-age p) 35))
                      (:where (string= (qe-person-city p) "Boston"))
                      (:select (qe-person-name p)))))))

(test query-order-by-ascending
  "An :ORDER-BY clause stably sorts results by its key form."
  (let ((people (qe-make-people)))
    (is (equal '("Bob" "Dave" "Alice" "Carol")
               (query (:from p people)
                      (:order-by (qe-person-age p))
                      (:select (qe-person-name p)))))))

(test query-order-by-descending
  "An :ORDER-BY clause with :DESCENDING T reverses the sort order."
  (let ((people (qe-make-people)))
    (is (equal '("Carol" "Alice" "Bob" "Dave")
               (query (:from p people)
                      (:order-by (qe-person-age p) :descending t)
                      (:select (qe-person-name p)))))))

(test query-order-by-is-stable
  "Rows with equal :ORDER-BY keys retain their relative source order."
  (let ((people (qe-make-people)))
    (is (equal '("Bob" "Dave")
               (query (:from p people)
                      (:where (= (qe-person-age p) 25))
                      (:order-by (qe-person-age p))
                      (:select (qe-person-name p)))))))

(test query-distinct-removes-duplicate-projected-rows
  "A :DISTINCT clause removes duplicate rows after projection,
keeping the first occurrence of each."
  (let ((people (qe-make-people)))
    (is (equal '("Boston" "Austin")
               (query (:from p people)
                      (:select (qe-person-city p))
                      (:distinct))))))

(test query-limit-truncates-results
  "A :LIMIT clause keeps only the first N results, after any
:ORDER-BY has already run."
  (let ((people (qe-make-people)))
    (is (equal '("Bob" "Dave")
               (query (:from p people)
                      (:order-by (qe-person-age p))
                      (:select (qe-person-name p))
                      (:limit 2))))))

(test query-limit-larger-than-result-count-is-harmless
  "A :LIMIT clause larger than the number of surviving rows returns
every row, without error."
  (let ((people (qe-make-people)))
    (is (= 4 (length (query (:from p people) (:limit 100)))))))

(test query-multiple-from-clauses-join
  "Multiple :FROM clauses form a correlated nested loop (a join),
with later SOURCE forms able to reference earlier VARs."
  (let ((people (qe-make-people))
        (cities '("Austin" "Boston")))
    (is (equal '(("Alice" . "Boston") ("Carol" . "Boston"))
               (query (:from p people)
                      (:from c cities)
                      (:where (string= (qe-person-city p) c))
                      (:where (string= c "Boston"))
                      (:select (cons (qe-person-name p) c)))))))

(test query-default-select-with-multiple-froms-is-a-list-of-vars
  "Omitting :SELECT with more than one :FROM clause returns a list
of every bound variable, in :FROM order."
  (is (equal '((1 :a) (1 :b) (2 :a) (2 :b))
             (query (:from x '(1 2)) (:from y '(:a :b))))))

(test query-no-from-clause-signals-an-error
  "QUERY signals an ordinary Lisp error if no :FROM clause is given."
  (signals error (macroexpand-1 '(query (:select 1)))))

(test query-repeated-singular-clause-signals-an-error
  "QUERY signals an ordinary Lisp error if :ORDER-BY, :DISTINCT,
:LIMIT, or :SELECT appears more than once."
  (signals error (macroexpand-1 '(query (:from x '(1)) (:limit 1) (:limit 2)))))

(test query-source-list-on-a-persistent-vector
  "QUERY-SOURCE-LIST on a PERSISTENT-VECTOR returns its elements in
index order, so QUERY works directly over one as a :FROM source."
  (let ((vector (collect-persistent-vector :dummy-repo '(10 20 30))))
    (is (equal '(10 20 30) (query-source-list vector)))
    (is (equal '(20 30) (query (:from x vector) (:where (> x 10)))))))

(test query-source-list-on-a-persistent-hash-table
  "QUERY-SOURCE-LIST on a PERSISTENT-HASH-TABLE returns a
(KEY . VALUE) cons per association."
  (let ((table (phash-put :b 2 (phash-put :a 1 (phash-make :repository :dummy-repo :test 'eql)))))
    (is (equal '((1 . :a) (2 . :b))
               (sort (query (:from pair table) (:select (cons (cdr pair) (car pair))))
                     #'< :key #'car)))))

(test query-source-list-on-a-function-thunk
  "QUERY-SOURCE-LIST FUNCALLs a function source and recursively
resolves its result, so a :FROM source may be a lazily-computed
thunk."
  (is (equal '(1 2 3) (query-source-list (lambda () '(1 2 3))))))

(test query-source-list-on-a-single-persistent-object
  "QUERY-SOURCE-LIST on a single PERSISTENT-OBJECT treats it as a
one-element collection holding just itself."
  (let ((person (make-instance 'qe-person :repository :dummy-repo :name "Eve")))
    (is (equal (list person) (query-source-list person)))))

(test query-source-list-signals-for-unsupported-types
  "QUERY-SOURCE-LIST signals an ordinary Lisp error for a type with
no applicable method."
  (signals error (query-source-list 42)))

(test query-count-counts-matching-elements
  "QUERY-COUNT counts elements of a list satisfying a predicate."
  (is (= 2 (query-count #'evenp '(1 2 3 4 5)))))

(test query-sum-sums-a-key-function
  "QUERY-SUM sums a key function over every element of a list, and
is 0 for an empty list."
  (is (= 15 (query-sum #'identity '(1 2 3 4 5))))
  (is (= 0 (query-sum #'identity '()))))

(test query-max-and-query-min-find-extrema
  "QUERY-MAX/QUERY-MIN return the extremal element and its own key,
and (VALUES NIL NIL) for an empty list."
  (multiple-value-bind (element key) (query-max #'identity '(3 1 4 1 5 9 2 6))
    (is (= 9 element))
    (is (= 9 key)))
  (multiple-value-bind (element key) (query-min #'identity '(3 1 4 1 5 9 2 6))
    (is (= 1 element))
    (is (= 1 key)))
  (is (equal '(nil nil) (multiple-value-list (query-max #'identity '())))))

(test query-group-by-groups-elements-by-key
  "QUERY-GROUP-BY groups elements by a key function, preserving
each group's own relative element order and first-seen group
order."
  (is (equal '((:odd 1 3 5) (:even 2 4))
             (query-group-by (lambda (n) (if (oddp n) :odd :even)) '(1 2 3 4 5)))))
