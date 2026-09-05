;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-cons-suite
  :in githack-suite
  :description "Tests for the PERSISTENT-CONS proxy, SERIALIZE-PERSISTENT-CONS, and DESERIALIZE-PERSISTENT-CONS.")

(in-suite persistent-cons-suite)

(test persistent-cons-is-a-git-tree
  "A PERSISTENT-CONS is a GIT-TREE (so it reuses ENTRIES,
INFER-GIT-MODE, and SERIALIZE-TREE), holding its own PERSISTENT-CAR
and PERSISTENT-CDR."
  (let* ((car-blob (make-instance 'git-blob :repository :dummy-repo :payload 42))
         (cons (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car car-blob
                                                :persistent-cdr nil)))
    (is (typep cons 'git-tree))
    (is (eq car-blob (persistent-car cons)))
    (is (null (persistent-cdr cons)))
    (is (string= "40000" (infer-git-mode cons)))))

(test serialize-persistent-cons-singleton-list
  "Serializing a cons whose PERSISTENT-CDR is NIL (a singleton
proper list) computes LENGTH 1 and PROPER T, synthesizes a GIT-BLOB
encoding the atom NIL as its persisted CDR, and produces the
standard four tree entries in sorted order."
  (let* ((calls '())
         (car-blob (make-instance 'git-blob :repository :dummy-repo :payload 42))
         (cons (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car car-blob
                                                :persistent-cdr nil)))
    (with-recording-git-hash-object (calls)
      (let ((sha (serialize-persistent-cons cons)))
        (is (stringp sha))
        (is (string= sha (sha cons)))))
    (is (= 1 (persistent-cons-length cons)))
    (is (eq t (persistent-cons-proper cons)))
    (is (get-loaded? cons))
    (let ((entries (get-entries cons)))
      (is (equal (list ".meta" "README.md" "car" "cdr") (mapcar #'car entries)))
      (is (eq car-blob (cdr (assoc "car" entries :test #'string=))))
      (let ((cdr-object (cdr (assoc "cdr" entries :test #'string=))))
        (is (eq cdr-object (persistent-cdr cons)))
        (is (typep cdr-object 'git-blob))
        (is (null (get-payload cdr-object)))
        (is (string= (%fake-sha-for "blob" (serialize-atom nil)) (sha cdr-object)))))))

(test serialize-persistent-cons-dotted-pair
  "Serializing a cons whose PERSISTENT-CDR is an ordinary,
already-persisted GIT-OBJECT (not another PERSISTENT-CONS and not
NIL) computes LENGTH 1 and PROPER NIL."
  (let* ((car-blob (make-instance 'git-blob :repository :dummy-repo :payload :a))
         (cdr-blob (make-instance 'git-blob :repository :dummy-repo :payload :b
                                             :sha "cccccccccccccccccccccccccccccccccccccccc"))
         (cons (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car car-blob
                                                :persistent-cdr cdr-blob)))
    (with-fake-git-hash-object ()
      (serialize-persistent-cons cons))
    (is (= 1 (persistent-cons-length cons)))
    (is (null (persistent-cons-proper cons)))
    (is (eq cdr-blob (persistent-cdr cons)))))

(test serialize-persistent-cons-nested-cons
  "Serializing a cons whose PERSISTENT-CDR is itself an unpersisted
PERSISTENT-CONS recursively persists that tail first, computes
LENGTH as one more than the tail's own LENGTH, and inherits the
tail's PROPER flag."
  (let* ((tail-car (make-instance 'git-blob :repository :dummy-repo :payload 2))
         (tail (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car tail-car
                                                :persistent-cdr nil))
         (head-car (make-instance 'git-blob :repository :dummy-repo :payload 1))
         (head (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car head-car
                                                :persistent-cdr tail)))
    (with-fake-git-hash-object ()
      (serialize-persistent-cons head))
    (is (stringp (sha tail)))
    (is (= 1 (persistent-cons-length tail)))
    (is (eq t (persistent-cons-proper tail)))
    (is (= 2 (persistent-cons-length head)))
    (is (eq t (persistent-cons-proper head)))))

(test serialize-persistent-cons-requires-a-car
  "SERIALIZE-PERSISTENT-CONS signals an error when PERSISTENT-CAR
has not been set."
  (let ((cons (make-instance 'persistent-cons :repository :dummy-repo)))
    (with-fake-git-hash-object ()
      (signals error (serialize-persistent-cons cons)))))

(test serialize-persistent-cons-is-idempotent
  "SERIALIZE-PERSISTENT-CONS does nothing (beyond returning the
existing SHA) for a cons that has already been persisted."
  (let* ((sha "dddddddddddddddddddddddddddddddddddddddd")
         (cons (make-instance 'persistent-cons :repository :dummy-repo :sha sha)))
    (is (string= sha (serialize-persistent-cons cons)))
    (is (null (get-entries cons)))))

(test serialize-persistent-cons-writes-standard-readme-and-meta
  "SERIALIZE-PERSISTENT-CONS writes the exact, fixed README.md
markdown content as raw (non-atom-envelope) UTF-8 bytes. (Its
\".meta\" blob's content is verified indirectly, by the round-trip
test below via the exported DESERIALIZE-PERSISTENT-CONS.)"
  (let* ((calls '())
         (car-blob (make-instance 'git-blob :repository :dummy-repo :payload 1))
         (cons (make-instance 'persistent-cons :repository :dummy-repo
                                                :persistent-car car-blob
                                                :persistent-cdr nil)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-cons cons))
    (is (find (sb-ext:string-to-octets +persistent-cons-readme+ :external-format :utf-8)
              calls :key #'third :test #'equalp))))

(test deserialize-persistent-cons-round-trips-with-serialize
  "Deserializing the tree bytes and .meta bytes produced by
SERIALIZE-PERSISTENT-CONS reconstructs an equivalent PERSISTENT-CONS
with the same LENGTH/PROPER and hollow CAR/CDR proxies for the same
SHAs."
  (let* ((car-blob-sha "1111111111111111111111111111111111111111")
         (car-blob (make-instance 'git-blob :repository :dummy-repo :payload 7 :sha car-blob-sha))
         (original (make-instance 'persistent-cons :repository :dummy-repo
                                                    :persistent-car car-blob
                                                    :persistent-cdr nil))
         (calls '()))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-cons original))
    (let* ((tree-octets (serialize-tree original))
           (meta-entry (cdr (assoc ".meta" (get-entries original) :test #'string=)))
           (readme-entry (cdr (assoc "README.md" (get-entries original) :test #'string=)))
           (cdr-entry (cdr (assoc "cdr" (get-entries original) :test #'string=)))
           (meta-octets (third (find (sha meta-entry) calls
                                      :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                      :test #'string=)))
           (hollow (make-instance 'persistent-cons :repository :dummy-repo :sha (sha original))))
      (is (not (null meta-octets)))
      (with-fake-git-type ((list (cons car-blob-sha "blob")
                                  (cons (sha cdr-entry) "blob")
                                  (cons (sha meta-entry) "blob")
                                  (cons (sha readme-entry) "blob")))
        (deserialize-persistent-cons hollow tree-octets meta-octets))
      (is (= (persistent-cons-length original) (persistent-cons-length hollow)))
      (is (eq (persistent-cons-proper original) (persistent-cons-proper hollow)))
      (is (get-loaded? hollow))
      (is (string= car-blob-sha (sha (persistent-car hollow))))
      (is (string= (sha cdr-entry) (sha (persistent-cdr hollow)))))))

(test deserialize-persistent-cons-signals-error-for-missing-entries
  "DESERIALIZE-PERSISTENT-CONS signals an error if the underlying
tree is missing any of the four required entries."
  (let* ((blob-sha "2222222222222222222222222222222222222222")
         (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
         (incomplete-tree (make-instance 'git-tree :repository :dummy-repo
                                                    :entries (list (cons "car" blob))))
         (tree-octets (serialize-tree incomplete-tree))
         (hollow (make-instance 'persistent-cons :repository :dummy-repo)))
    (with-fake-git-type ((list (cons blob-sha "blob")))
      (signals error (deserialize-persistent-cons hollow tree-octets #())))))

(test scan-persistent-list-of-nil-is-empty
  "SCAN-PERSISTENT-LIST of NIL (the empty list) produces an empty
series."
  (is (null (series:collect (scan-persistent-list nil)))))

(test scan-persistent-list-collects-decoded-car-elements
  "SCAN-PERSISTENT-LIST of an in-memory, already-loaded chain of
PERSISTENT-CONS cells produces a series of their PERSISTENT-CAR
values, each decoded via %PERSISTENT-CONS-DECODE (a GIT-BLOB's own
PAYLOAD, here plain integers)."
  (let* ((c3 (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                             :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 3 :loaded? t)
                             :persistent-cdr nil))
         (c2 (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                             :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 2 :loaded? t)
                             :persistent-cdr c3))
         (c1 (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                             :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 1 :loaded? t)
                             :persistent-cdr c2)))
    (is (equal '(1 2 3) (series:collect (scan-persistent-list c1))))))

(test scan-persistent-list-stops-before-a-dotted-pairs-final-atom
  "SCAN-PERSISTENT-LIST of a dotted pair (whose final PERSISTENT-CDR
is a non-NIL GIT-BLOB, not a further PERSISTENT-CONS) stops before
that terminating atom, yielding only the real cons cells' decoded
CAR values."
  (let* ((final-atom (make-instance 'git-blob :repository :dummy-repo :payload :tail :loaded? t))
         (c2 (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                             :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 2 :loaded? t)
                             :persistent-cdr final-atom))
         (c1 (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                             :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 1 :loaded? t)
                             :persistent-cdr c2)))
    (is (equal '(1 2) (series:collect (scan-persistent-list c1))))))

(test scan-persistent-list-round-trips-through-a-fake-git-repository
  "SCAN-PERSISTENT-LIST correctly walks a chain of PERSISTENT-CONS
cells that must be lazily fetched and inflated from Git: after
serializing an in-memory list and obtaining only a hollow proxy for
its head SHA (via INFLATE-GIT-PROXY, exactly as a fresh repository
read would), scanning the hollow proxy yields the same decoded CAR
values as the original in-memory list."
  (with-fake-git-repository ()
    (let* ((c2 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 2)
                               :persistent-cdr nil))
           (c1 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 1)
                               :persistent-cdr c2))
           (head-sha (serialize-persistent-cons c1))
           (hollow (inflate-git-proxy :dummy-repo head-sha)))
      (is (equal '(1 2) (series:collect (scan-persistent-list hollow)))))))

(test collect-persistent-list-of-nil-is-nil
  "COLLECT-PERSISTENT-LIST of an empty list (NIL) is NIL."
  (is (null (collect-persistent-list :dummy-repo nil))))

(test collect-persistent-list-builds-a-chain-of-blob-wrapped-atoms
  "COLLECT-PERSISTENT-LIST wraps every raw, non-GIT-OBJECT element
of ITEMS in a fresh, already GET-LOADED? GIT-BLOB, chains them via
PERSISTENT-CDR in ITEMS' own order, marks every new cons cell
GET-LOADED?, and leaves every new cons cell's own SHA unset (not yet
persisted)."
  (let ((head (collect-persistent-list :dummy-repo (list 1 2 3))))
    (is (typep head 'persistent-cons))
    (is (null (sha head)))
    (is (get-loaded? head))
    (is (= 1 (get-payload (persistent-car head))))
    (let ((tail (persistent-cdr head)))
      (is (= 2 (get-payload (persistent-car tail))))
      (let ((tail2 (persistent-cdr tail)))
        (is (= 3 (get-payload (persistent-car tail2))))
        (is (null (persistent-cdr tail2)))))))

(test collect-persistent-list-passes-through-compound-elements-unchanged
  "COLLECT-PERSISTENT-LIST stores an already-compound GIT-OBJECT
element (a nested PERSISTENT-CONS, here) directly as that cons
cell's own PERSISTENT-CAR, unwrapped, rather than re-wrapping it in
a further GIT-BLOB."
  (let* ((nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                 :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :inner :loaded? t)
                                 :persistent-cdr nil))
         (head (collect-persistent-list :dummy-repo (list nested))))
    (is (eq nested (persistent-car head)))))

(test collect-persistent-list-is-the-inverse-of-scan-persistent-list
  "Round-tripping a plain list of atoms through COLLECT-PERSISTENT-
LIST then SCAN-PERSISTENT-LIST (materialized via SERIES:COLLECT)
reproduces the original list's elements, in order."
  (let ((items (list :a :b :c)))
    (is (equal items (series:collect (scan-persistent-list
                                       (collect-persistent-list :dummy-repo items)))))))

(test collect-persistent-list-result-serializes-with-serialize-persistent-cons
  "The in-memory chain built by COLLECT-PERSISTENT-LIST can be
persisted directly via SERIALIZE-PERSISTENT-CONS, exactly like any
other hand-built PERSISTENT-CONS chain."
  (let ((head (collect-persistent-list :dummy-repo (list 1 2 3))))
    (with-fake-git-hash-object ()
      (let ((sha (serialize-persistent-cons head)))
        (is (stringp sha))
        (is (string= sha (sha head)))))
    (is (= 3 (persistent-cons-length head)))
    (is (eq t (persistent-cons-proper head)))))

(defun %make-persistent-alist-pair (key value)
  "Helper for the SCAN-PERSISTENT-ALIST tests below: build a single,
in-memory, already GET-LOADED? PERSISTENT-CONS pair whose
PERSISTENT-CAR is KEY and PERSISTENT-CDR is VALUE, wrapping each in
a fresh GIT-BLOB unless it is already a GIT-OBJECT -- the same
PERSISTENT-CONS-of-PERSISTENT-CONSes shape PERSISTENT-HASH-TABLE.LISP
uses for its own bucket chains."
  (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                   :persistent-car (if (typep key 'git-object) key
                                                        (make-instance 'git-blob :repository :dummy-repo :payload key :loaded? t))
                                   :persistent-cdr (if (typep value 'git-object) value
                                                        (make-instance 'git-blob :repository :dummy-repo :payload value :loaded? t))))

(defun %make-persistent-alist-spine (pairs)
  "Helper for the SCAN-PERSISTENT-ALIST tests below: build an
in-memory, already GET-LOADED? PERSISTENT-CONS spine chaining the
given PAIRS (each already a PERSISTENT-CONS, e.g. as returned by
%MAKE-PERSISTENT-ALIST-PAIR) in order, terminated by NIL."
  (reduce (lambda (pair tail)
            (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                             :persistent-car pair
                                             :persistent-cdr tail))
          pairs
          :from-end t
          :initial-value nil))

(test scan-persistent-alist-of-nil-is-empty
  "SCAN-PERSISTENT-ALIST of NIL (the empty alist) produces two empty
series, for keys and values respectively."
  (multiple-value-bind (keys values) (scan-persistent-alist nil)
    (is (null (series:collect keys)))
    (is (null (series:collect values)))))

(test scan-persistent-alist-collects-decoded-key-value-pairs
  "SCAN-PERSISTENT-ALIST of an in-memory, already-loaded spine of
PERSISTENT-CONS pairs produces two series -- keys and, respectively,
values -- each decoded via %PERSISTENT-CONS-DECODE (a GIT-BLOB's own
PAYLOAD, here plain keywords/integers), in the spine's own order."
  (let ((spine (%make-persistent-alist-spine
                (list (%make-persistent-alist-pair :a 1)
                      (%make-persistent-alist-pair :b 2)
                      (%make-persistent-alist-pair :c 3)))))
    (multiple-value-bind (keys values) (scan-persistent-alist spine)
      (is (equal '(:a :b :c) (series:collect keys)))
      (is (equal '(1 2 3) (series:collect values))))))

(test scan-persistent-alist-passes-through-compound-values-unchanged
  "SCAN-PERSISTENT-ALIST leaves an already-compound GIT-OBJECT value
(a nested PERSISTENT-CONS, here) unwrapped, exactly as
%PERSISTENT-CONS-DECODE always does for any non-GIT-BLOB element."
  (let* ((nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                 :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :inner :loaded? t)
                                 :persistent-cdr nil))
         (spine (%make-persistent-alist-spine (list (%make-persistent-alist-pair :a nested)))))
    (multiple-value-bind (keys values) (scan-persistent-alist spine)
      (is (equal '(:a) (series:collect keys)))
      (is (eq nested (first (series:collect values)))))))

(test scan-persistent-alist-round-trips-through-a-fake-git-repository
  "SCAN-PERSISTENT-ALIST correctly walks a spine of PERSISTENT-CONS
pairs that must be lazily fetched and inflated from Git: after
serializing an in-memory alist and obtaining only a hollow proxy for
its head SHA (via INFLATE-GIT-PROXY, exactly as a fresh repository
read would), scanning the hollow proxy yields the same decoded
keys/values as the original in-memory alist."
  (with-fake-git-repository ()
    (let* ((pair2 (make-instance 'persistent-cons :repository :dummy-repo
                                  :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :b)
                                  :persistent-cdr (make-instance 'git-blob :repository :dummy-repo :payload 2)))
           (pair1 (make-instance 'persistent-cons :repository :dummy-repo
                                  :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :a)
                                  :persistent-cdr (make-instance 'git-blob :repository :dummy-repo :payload 1)))
           (spine2 (make-instance 'persistent-cons :repository :dummy-repo
                                   :persistent-car pair2 :persistent-cdr nil))
           (spine1 (make-instance 'persistent-cons :repository :dummy-repo
                                   :persistent-car pair1 :persistent-cdr spine2))
           (head-sha (serialize-persistent-cons spine1))
           (hollow (inflate-git-proxy :dummy-repo head-sha)))
      (multiple-value-bind (keys values) (scan-persistent-alist hollow)
        (is (equal '(:a :b) (series:collect keys)))
        (is (equal '(1 2) (series:collect values)))))))

(defun %make-persistent-plist-spine (plist)
  "Helper for the SCAN-PERSISTENT-PLIST tests below: build an
in-memory, already GET-LOADED? PERSISTENT-CONS spine holding the
successive elements of PLIST (an ordinary Lisp plist -- a flat list
alternating indicator, value, indicator, value, ...) in order,
wrapping each raw, non-GIT-OBJECT element in a fresh GIT-BLOB."
  (reduce (lambda (value tail)
            (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                             :persistent-car (if (typep value 'git-object) value
                                                                  (make-instance 'git-blob :repository :dummy-repo :payload value :loaded? t))
                                             :persistent-cdr tail))
          plist
          :from-end t
          :initial-value nil))

(test scan-persistent-plist-of-nil-is-empty
  "SCAN-PERSISTENT-PLIST of NIL (the empty plist) produces two empty
series, for indicators and values respectively."
  (multiple-value-bind (indicators values) (scan-persistent-plist nil)
    (is (null (series:collect indicators)))
    (is (null (series:collect values)))))

(test scan-persistent-plist-collects-decoded-indicator-value-pairs
  "SCAN-PERSISTENT-PLIST of an in-memory, already-loaded flat spine
of alternating indicator/value elements produces two series --
indicators and, respectively, values -- each decoded via
%PERSISTENT-CONS-DECODE (a GIT-BLOB's own PAYLOAD, here plain
keywords/integers), in the spine's own order."
  (let ((spine (%make-persistent-plist-spine (list :a 1 :b 2 :c 3))))
    (multiple-value-bind (indicators values) (scan-persistent-plist spine)
      (is (equal '(:a :b :c) (series:collect indicators)))
      (is (equal '(1 2 3) (series:collect values))))))

(test scan-persistent-plist-passes-through-compound-values-unchanged
  "SCAN-PERSISTENT-PLIST leaves an already-compound GIT-OBJECT value
(a nested PERSISTENT-CONS, here) unwrapped, exactly as
%PERSISTENT-CONS-DECODE always does for any non-GIT-BLOB element."
  (let* ((nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                 :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :inner :loaded? t)
                                 :persistent-cdr nil))
         (spine (%make-persistent-plist-spine (list :a nested))))
    (multiple-value-bind (indicators values) (scan-persistent-plist spine)
      (is (equal '(:a) (series:collect indicators)))
      (is (eq nested (first (series:collect values)))))))

(test scan-persistent-plist-round-trips-through-a-fake-git-repository
  "SCAN-PERSISTENT-PLIST correctly walks a flat spine of alternating
indicator/value elements that must be lazily fetched and inflated
from Git: after serializing an in-memory plist and obtaining only a
hollow proxy for its head SHA (via INFLATE-GIT-PROXY, exactly as a
fresh repository read would), scanning the hollow proxy yields the
same decoded indicators/values as the original in-memory plist."
  (with-fake-git-repository ()
    (let* ((c4 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 2)
                               :persistent-cdr nil))
           (c3 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :b)
                               :persistent-cdr c4))
           (c2 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload 1)
                               :persistent-cdr c3))
           (c1 (make-instance 'persistent-cons :repository :dummy-repo
                               :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :a)
                               :persistent-cdr c2))
           (head-sha (serialize-persistent-cons c1))
           (hollow (inflate-git-proxy :dummy-repo head-sha)))
      (multiple-value-bind (indicators values) (scan-persistent-plist hollow)
        (is (equal '(:a :b) (series:collect indicators)))
        (is (equal '(1 2) (series:collect values)))))))

(test collect-persistent-alist-of-nil-is-nil
  "COLLECT-PERSISTENT-ALIST of two empty lists (KEYS and VALUES both
NIL) is NIL."
  (is (null (collect-persistent-alist :dummy-repo nil nil))))

(test collect-persistent-alist-signals-error-for-mismatched-lengths
  "COLLECT-PERSISTENT-ALIST signals an error if KEYS and VALUES are
not the same length."
  (signals error (collect-persistent-alist :dummy-repo (list :a :b) (list 1))))

(test collect-persistent-alist-builds-a-spine-of-blob-wrapped-pairs
  "COLLECT-PERSISTENT-ALIST wraps every raw, non-GIT-OBJECT key/value
of KEYS/VALUES in a fresh, already GET-LOADED? GIT-BLOB, pairs them
into a further PERSISTENT-CONS per entry, chains those pairs via the
outer spine's own PERSISTENT-CDR in KEYS/VALUES' own order, marks
every new cons cell (outer spine and inner pairs) GET-LOADED?, and
leaves every new cons cell's own SHA unset (not yet persisted)."
  (let ((spine (collect-persistent-alist :dummy-repo (list :a :b :c) (list 1 2 3))))
    (is (typep spine 'persistent-cons))
    (is (null (sha spine)))
    (is (get-loaded? spine))
    (let ((pair1 (persistent-car spine)))
      (is (typep pair1 'persistent-cons))
      (is (get-loaded? pair1))
      (is (eq :a (get-payload (persistent-car pair1))))
      (is (= 1 (get-payload (persistent-cdr pair1)))))
    (let* ((tail2 (persistent-cdr spine))
           (pair2 (persistent-car tail2)))
      (is (eq :b (get-payload (persistent-car pair2))))
      (is (= 2 (get-payload (persistent-cdr pair2))))
      (let* ((tail3 (persistent-cdr tail2))
             (pair3 (persistent-car tail3)))
        (is (eq :c (get-payload (persistent-car pair3))))
        (is (= 3 (get-payload (persistent-cdr pair3))))
        (is (null (persistent-cdr tail3)))))))

(test collect-persistent-alist-passes-through-compound-elements-unchanged
  "COLLECT-PERSISTENT-ALIST stores an already-compound GIT-OBJECT
key or value (a nested PERSISTENT-CONS, here) directly as that
pair's own PERSISTENT-CAR/PERSISTENT-CDR, unwrapped, rather than
re-wrapping it in a further GIT-BLOB."
  (let* ((nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                 :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :inner :loaded? t)
                                 :persistent-cdr nil))
         (spine (collect-persistent-alist :dummy-repo (list :a) (list nested))))
    (is (eq nested (persistent-cdr (persistent-car spine))))))

(test collect-persistent-alist-is-the-inverse-of-scan-persistent-alist
  "Round-tripping two plain lists of keys/values through
COLLECT-PERSISTENT-ALIST then SCAN-PERSISTENT-ALIST (materialized via
SERIES:COLLECT) reproduces the original keys/values, in order."
  (let ((keys (list :a :b :c))
        (values (list 1 2 3)))
    (multiple-value-bind (scanned-keys scanned-values)
        (scan-persistent-alist (collect-persistent-alist :dummy-repo keys values))
      (is (equal keys (series:collect scanned-keys)))
      (is (equal values (series:collect scanned-values))))))

(test collect-persistent-alist-result-serializes-with-serialize-persistent-cons
  "The in-memory spine built by COLLECT-PERSISTENT-ALIST can be
persisted directly via SERIALIZE-PERSISTENT-CONS, exactly like any
other hand-built PERSISTENT-CONS chain."
  (let ((spine (collect-persistent-alist :dummy-repo (list :a :b) (list 1 2))))
    (with-fake-git-hash-object ()
      (let ((sha (serialize-persistent-cons spine)))
        (is (stringp sha))
        (is (string= sha (sha spine)))))
    (is (= 2 (persistent-cons-length spine)))
    (is (eq t (persistent-cons-proper spine)))))

(test collect-persistent-plist-of-nil-is-nil
  "COLLECT-PERSISTENT-PLIST of two empty lists (INDICATORS and VALUES
both NIL) is NIL."
  (is (null (collect-persistent-plist :dummy-repo nil nil))))

(test collect-persistent-plist-signals-error-for-mismatched-lengths
  "COLLECT-PERSISTENT-PLIST signals an error if INDICATORS and VALUES
are not the same length."
  (signals error (collect-persistent-plist :dummy-repo (list :a :b) (list 1))))

(test collect-persistent-plist-builds-a-flat-spine-of-blob-wrapped-elements
  "COLLECT-PERSISTENT-PLIST wraps every raw, non-GIT-OBJECT indicator/
value of INDICATORS/VALUES in a fresh, already GET-LOADED? GIT-BLOB,
chains them via PERSISTENT-CDR into one flat spine alternating
indicator, value, indicator, value, ... in INDICATORS/VALUES' own
order, marks every new cons cell GET-LOADED?, and leaves every new
cons cell's own SHA unset (not yet persisted)."
  (let ((spine (collect-persistent-plist :dummy-repo (list :a :b) (list 1 2))))
    (is (typep spine 'persistent-cons))
    (is (null (sha spine)))
    (is (get-loaded? spine))
    (is (eq :a (get-payload (persistent-car spine))))
    (let ((tail1 (persistent-cdr spine)))
      (is (= 1 (get-payload (persistent-car tail1))))
      (let ((tail2 (persistent-cdr tail1)))
        (is (eq :b (get-payload (persistent-car tail2))))
        (let ((tail3 (persistent-cdr tail2)))
          (is (= 2 (get-payload (persistent-car tail3))))
          (is (null (persistent-cdr tail3))))))))

(test collect-persistent-plist-passes-through-compound-elements-unchanged
  "COLLECT-PERSISTENT-PLIST stores an already-compound GIT-OBJECT
value (a nested PERSISTENT-CONS, here) directly as its own cons
cell's PERSISTENT-CAR, unwrapped, rather than re-wrapping it in a
further GIT-BLOB."
  (let* ((nested (make-instance 'persistent-cons :repository :dummy-repo :loaded? t
                                 :persistent-car (make-instance 'git-blob :repository :dummy-repo :payload :inner :loaded? t)
                                 :persistent-cdr nil))
         (spine (collect-persistent-plist :dummy-repo (list :a) (list nested))))
    (is (eq nested (persistent-car (persistent-cdr spine))))))

(test collect-persistent-plist-is-the-inverse-of-scan-persistent-plist
  "Round-tripping two plain lists of indicators/values through
COLLECT-PERSISTENT-PLIST then SCAN-PERSISTENT-PLIST (materialized via
SERIES:COLLECT) reproduces the original indicators/values, in
order."
  (let ((indicators (list :a :b :c))
        (values (list 1 2 3)))
    (multiple-value-bind (scanned-indicators scanned-values)
        (scan-persistent-plist (collect-persistent-plist :dummy-repo indicators values))
      (is (equal indicators (series:collect scanned-indicators)))
      (is (equal values (series:collect scanned-values))))))

(test collect-persistent-plist-result-serializes-with-serialize-persistent-cons
  "The in-memory spine built by COLLECT-PERSISTENT-PLIST can be
persisted directly via SERIALIZE-PERSISTENT-CONS, exactly like any
other hand-built PERSISTENT-CONS chain."
  (let ((spine (collect-persistent-plist :dummy-repo (list :a :b) (list 1 2))))
    (with-fake-git-hash-object ()
      (let ((sha (serialize-persistent-cons spine)))
        (is (stringp sha))
        (is (string= sha (sha spine)))))
    (is (= 4 (persistent-cons-length spine)))
    (is (eq t (persistent-cons-proper spine)))))
