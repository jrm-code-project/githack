;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; QUERY is a small, declarative, Lisp-embedded query language for
;;; reading GitHack persistent collections (PERSISTENT-VECTOR,
;;; PERSISTENT-HASH-TABLE, PERSISTENT-WTTREE, ordinary Lisp lists/
;;; vectors, and PERSISTENT-OBJECT/PERSISTENT-STRUCT instances found
;;; within them) without hand-writing PHASH-MAP/SCAN-*/WT-FOLD
;;; traversal code at every call site. A QUERY form reads like a
;;; small relational-algebra pipeline:
;;;
;;;   (:from VAR SOURCE)   bind VAR, in turn, to each element of
;;;                        SOURCE (via QUERY-SOURCE-LIST -- see
;;;                        below); one or more of these, evaluated
;;;                        left to right as a correlated nested loop,
;;;                        so a later SOURCE expression may freely
;;;                        reference an earlier VAR (e.g. to join two
;;;                        collections on a shared key)
;;;   (:where PREDICATE)   keep only rows for which PREDICATE (an
;;;                        ordinary Lisp expression seeing every VAR
;;;                        bound so far) is true; zero or more,
;;;                        implicitly ANDed together
;;;   (:order-by KEY-FORM  stably sort surviving rows by KEY-FORM
;;;    &key DESCENDING      (seeing every VAR); at most one
;;;    TEST)
;;;   (:distinct)          drop duplicate rows (compared via EQUAL,
;;;                        after :SELECT's own projection); at most
;;;                        one
;;;   (:limit N-FORM)      keep only the first N-FORM rows; at most
;;;                        one
;;;   (:select EXPR)       project each surviving row to EXPR (seeing
;;;                        every VAR); at most one -- defaults to the
;;;                        sole VAR itself if there is exactly one
;;;                        :FROM clause, or a list of every VAR, in
;;;                        :FROM order, otherwise
;;;
;;; QUERY always returns a fresh Lisp list. See QUERY-SOURCE-LIST for
;;; how a :FROM clause's SOURCE expression is turned into a series of
;;; elements to iterate over.

(defgeneric query-source-list (source)
  (:documentation
   "Return a fresh Lisp list of SOURCE's own elements, in an
implementation-defined but stable order, for QUERY's :FROM clauses
to iterate over. Every GitHack persistent collection type has its
own method: PERSISTENT-VECTOR (index order, via SCAN-PERSISTENT-
VECTOR), PERSISTENT-HASH-TABLE (a (KEY . VALUE) cons per association,
in unspecified order, via PHASH-MAP), and PERSISTENT-WTTREE (a
(KEY . VALUE) cons per association, in ascending key order, via
WT->ALIST). An ordinary Lisp list is returned unchanged; an ordinary
Lisp vector is coerced to a list; a single PERSISTENT-OBJECT is
treated as a one-element collection holding just itself; a function
is FUNCALLed with no arguments and its result recursively passed back
through QUERY-SOURCE-LIST (so a :FROM clause's SOURCE may be a thunk
that computes/loads its collection lazily); NIL is the empty
collection. Define further methods, specialized on your own
collection classes, to make QUERY work directly over them too."))

(defmethod query-source-list ((source null))
  nil)

(defmethod query-source-list ((source cons))
  source)

(defmethod query-source-list ((source vector))
  (coerce source 'list))

(defmethod query-source-list ((source persistent-vector))
  (collect (scan-persistent-vector source)))

(defmethod query-source-list ((source persistent-hash-table))
  (let ((pairs '()))
    (phash-map (lambda (key value) (push (cons key value) pairs)) source)
    (nreverse pairs)))

(defmethod query-source-list ((source persistent-wttree))
  (wt->alist source))

(defmethod query-source-list ((source function))
  (query-source-list (funcall source)))

(defmethod query-source-list ((source persistent-object))
  (list source))

(defmethod query-source-list ((source t))
  (error 'invalid-argument-error
         :format-control "QUERY-SOURCE-LIST has no method for ~S (of type ~S); define one to use it as a QUERY :FROM source."
         :format-arguments (list source (type-of source))))

(defun query-default-less-than (a b)
  "Return true if A sorts strictly before B under QUERY's own
default :ORDER-BY comparison, used whenever an :ORDER-BY clause does
not supply its own :TEST: numbers compare via <, strings (or, more
generally, arrays of CHARACTER) compare via STRING<, symbols compare
by their own SYMBOL-NAME via STRING<, and any other pair of values
compares by SXHASH -- an arbitrary but total and stable order,
sufficient to make sorting deterministic even over incomparable
application data."
  (cond
    ((and (realp a) (realp b)) (< a b))
    ((and (stringp a) (stringp b)) (string< a b))
    ((and (symbolp a) (symbolp b)) (string< (symbol-name a) (symbol-name b)))
    (t (< (sxhash a) (sxhash b)))))

(defun query-parse-clauses (clauses)
  "Return seven values parsed out of CLAUSES (a QUERY macro's own
&REST clause list, each clause a list headed by one of :FROM,
:WHERE, :ORDER-BY, :DISTINCT, :LIMIT, or :SELECT): FROMS (a list of
(VAR SOURCE-FORM) lists, in the order given), WHERES (a list of
predicate forms, in the order given), ORDER-BY-KEY (a form, or NIL if
no :ORDER-BY clause was given), ORDER-BY-DESCENDING (a form, default
NIL), ORDER-BY-TEST (a form, default NIL, meaning
QUERY-DEFAULT-LESS-THAN), DISTINCT? (true if a :DISTINCT clause was
given), LIMIT (a form, or NIL if no :LIMIT clause was given), and
SELECT (a form, or the keyword :DEFAULT if no :SELECT clause was
given, meaning QUERY should compute its own default -- see QUERY).
Signals an ordinary Lisp error if CLAUSES holds no :FROM clause at
all, or more than one each of :ORDER-BY, :DISTINCT, :LIMIT, or
:SELECT."
  (let ((froms '())
        (wheres '())
        (order-by-key nil)
        (order-by-descending nil)
        (order-by-test nil)
        (order-by-seen? nil)
        (distinct? nil)
        (limit nil)
        (limit-seen? nil)
        (select :default)
        (select-seen? nil))
    (dolist (clause clauses)
      (unless (and (consp clause) (keywordp (car clause)))
        (error "Malformed QUERY clause ~S: expected a list headed by a keyword (:FROM, :WHERE, :ORDER-BY, :DISTINCT, :LIMIT, or :SELECT)." clause))
      (ecase (car clause)
        (:from
         (destructuring-bind (var source-form) (cdr clause)
           (push (list var source-form) froms)))
        (:where
         (destructuring-bind (predicate-form) (cdr clause)
           (push predicate-form wheres)))
        (:order-by
         (when order-by-seen? (error "QUERY accepts at most one :ORDER-BY clause."))
         (setf order-by-seen? t)
         (destructuring-bind (key-form &key descending test) (cdr clause)
           (setf order-by-key key-form)
           (setf order-by-descending descending)
           (setf order-by-test test)))
        (:distinct
         (when distinct? (error "QUERY accepts at most one :DISTINCT clause."))
         (setf distinct? t))
        (:limit
         (when limit-seen? (error "QUERY accepts at most one :LIMIT clause."))
         (setf limit-seen? t)
         (destructuring-bind (limit-form) (cdr clause)
           (setf limit limit-form)))
        (:select
         (when select-seen? (error "QUERY accepts at most one :SELECT clause."))
         (setf select-seen? t)
         (destructuring-bind (select-form) (cdr clause)
           (setf select select-form)))))
    (setf froms (nreverse froms))
    (unless froms (error "QUERY requires at least one :FROM clause."))
    (values froms (nreverse wheres) order-by-key order-by-descending order-by-test distinct? limit select)))

(defun query-build-loop (froms where-form body-form)
  "Return the form QUERY's expansion nests its own BODY-FORM inside:
one DOLIST per (VAR SOURCE-FORM) pair in FROMS (in order, so a later
SOURCE-FORM may reference an earlier VAR), around a WHEN guarded by
WHERE-FORM (a single form -- QUERY itself ANDs every :WHERE clause
together first) wrapping BODY-FORM. Each SOURCE-FORM is passed
through QUERY-SOURCE-LIST so any supported collection (or a thunk
producing one) may appear directly, unconverted, as a :FROM source."
  (if (null froms)
      `(when ,where-form ,body-form)
      (destructuring-bind (var source-form) (first froms)
        `(dolist (,var (query-source-list ,source-form))
           ,(query-build-loop (rest froms) where-form body-form)))))

(defmacro query (&rest clauses)
  "Evaluate a declarative query over one or more GitHack persistent
(or ordinary Lisp) collections and return a fresh Lisp list of
results. CLAUSES is a sequence of (:FROM VAR SOURCE), (:WHERE
PREDICATE), (:ORDER-BY KEY-FORM &KEY DESCENDING TEST), (:DISTINCT),
(:LIMIT N), and (:SELECT EXPR) clauses -- see this file's own top-of-
file commentary for the full grammar and evaluation order. At least
one :FROM clause is required; every other clause kind is optional,
and every kind but :FROM and :WHERE may appear at most once.

Example -- every checked-out book's title, newest first:

  (query (:from book *catalog*)
         (:where (book-checked-out-p book))
         (:order-by (book-title book))
         (:select (book-title book)))

Example -- a join of two collections, correlated by ISBN:

  (query (:from book *catalog*)
         (:from loan *loans*)
         (:where (string= (book-isbn book) (loan-isbn loan)))
         (:select (list (book-title book) (loan-borrower loan))))"
  (multiple-value-bind (froms wheres order-by-key order-by-descending order-by-test distinct? limit select)
      (query-parse-clauses clauses)
    (let* ((vars (mapcar #'first froms))
           (where-form (if wheres `(and ,@wheres) t))
           (select-form (if (eq select :default)
                             (if (rest vars) `(list ,@vars) (first vars))
                             select))
           (results (gensym "RESULTS"))
           (row (gensym "ROW")))
      `(let ((,results '()))
         ,(query-build-loop froms where-form
                              `(push ,(if order-by-key
                                          `(cons ,order-by-key ,select-form)
                                          select-form)
                                     ,results))
         (setf ,results (nreverse ,results))
         ,@(when order-by-key
             `((setf ,results
                     (stable-sort ,results
                                  ,(if order-by-descending
                                       `(lambda (a b) (funcall ,(or order-by-test '#'query-default-less-than) b a))
                                       `(lambda (a b) (funcall ,(or order-by-test '#'query-default-less-than) a b)))
                                  :key #'car))
               (setf ,results (mapcar #'cdr ,results))))
         ,@(when distinct?
             `((setf ,results (remove-duplicates ,results :test #'equal :from-end t))))
         ,@(when limit
             `((let ((,row ,limit))
                 (setf ,results (if (< ,row (length ,results)) (subseq ,results 0 ,row) ,results)))))
         ,results))))

(defun query-count (predicate list)
  "Return the number of elements of LIST (typically a QUERY result,
or any other Lisp list) for which PREDICATE is true. A thin,
self-documenting wrapper over COUNT-IF, provided so a query's own
final aggregation step reads declaratively alongside QUERY itself."
  (count-if predicate list))

(defun query-sum (key-function list)
  "Return the sum of (FUNCALL KEY-FUNCTION element) over every
element of LIST (typically a QUERY result). Returns 0 if LIST is
empty."
  (reduce #'+ list :key key-function :initial-value 0))

(defun query-max (key-function list)
  "Return two values: the element of LIST (typically a QUERY result)
for which (FUNCALL KEY-FUNCTION element) is greatest, and that
greatest key itself; or (VALUES NIL NIL) if LIST is empty. Ties are
broken in favor of the earliest such element in LIST."
  (if (null list)
      (values nil nil)
      (let* ((best (first list))
             (best-key (funcall key-function best)))
        (dolist (element (rest list))
          (let ((key (funcall key-function element)))
            (when (> key best-key)
              (setf best element)
              (setf best-key key))))
        (values best best-key))))

(defun query-min (key-function list)
  "Return two values: the element of LIST (typically a QUERY result)
for which (FUNCALL KEY-FUNCTION element) is least, and that least key
itself; or (VALUES NIL NIL) if LIST is empty. Ties are broken in
favor of the earliest such element in LIST."
  (if (null list)
      (values nil nil)
      (let* ((best (first list))
             (best-key (funcall key-function best)))
        (dolist (element (rest list))
          (let ((key (funcall key-function element)))
            (when (< key best-key)
              (setf best element)
              (setf best-key key))))
        (values best best-key))))

(defun query-group-by (key-function list)
  "Return a fresh (KEY . ELEMENTS) alist grouping every element of
LIST (typically a QUERY result) by (FUNCALL KEY-FUNCTION element)
(compared via EQUAL), each group's own ELEMENTS in the same relative
order they appeared in LIST, and groups themselves in first-seen
order."
  (let ((groups '()))
    (dolist (element list)
      (let* ((key (funcall key-function element))
             (existing (assoc key groups :test #'equal)))
        (if existing
            (push element (cdr existing))
            (push (list key element) groups))))
    (setf groups (nreverse groups))
    (dolist (group groups groups)
      (setf (cdr group) (nreverse (cdr group))))))
