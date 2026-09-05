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
