;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite persistent-wttree-suite
  :in githack-suite
  :description "Tests for the PERSISTENT-WTTREE proxy, SERIALIZE-PERSISTENT-WTTREE-NODE, DESERIALIZE-PERSISTENT-WTTREE-NODE, and the WT-* Adams-tree operations.")

(in-suite persistent-wttree-suite)

(test persistent-wttree-is-a-git-tree
  "A PERSISTENT-WTTREE is a GIT-TREE (so it reuses ENTRIES,
INFER-GIT-MODE, and SERIALIZE-TREE), holding its own KEY, VALUE,
LEFT, RIGHT, and WEIGHT."
  (let* ((key-blob (make-instance 'git-blob :repository :dummy-repo :payload :k :loaded? t))
         (value-blob (make-instance 'git-blob :repository :dummy-repo :payload :v :loaded? t))
         (node (make-instance 'persistent-wttree :repository :dummy-repo :loaded? t
                                                  :key key-blob :value value-blob
                                                  :left nil :right nil :weight 1)))
    (is (typep node 'git-tree))
    (is (string= "40000" (infer-git-mode node)))
    (is (eq :k (wt-node-key node)))
    (is (eq :v (wt-node-value node)))
    (is (null (wt-node-left node)))
    (is (null (wt-node-right node)))
    (is (= 1 (wt-weight node)))))

(test wt-weight-of-empty-tree-is-zero
  "WT-WEIGHT of NIL (the empty tree) is 0, and WT-EMPTY-P recognizes
NIL (only) as empty."
  (is (= 0 (wt-weight nil)))
  (is (wt-empty-p nil))
  (is (not (wt-empty-p (wt-singleton :dummy-repo :k :v)))))

(test wt-singleton-has-weight-one
  "WT-SINGLETON builds a single, already-loaded node of weight 1."
  (let ((node (wt-singleton :dummy-repo :k :v)))
    (is (= 1 (wt-weight node)))
    (is (eq :k (wt-node-key node)))
    (is (eq :v (wt-node-value node)))
    (is (get-loaded? node))))

(test wt-add-and-wt-lookup-basic
  "WT-ADD builds up a tree association by association, and WT-LOOKUP
finds every key that was added, returning a second value of NIL for
any key never added."
  (let ((tree nil))
    (dolist (key '(5 3 8 1 4 7 9 2 6 0))
      (setf tree (wt-add :dummy-repo #'< tree key (* key 10))))
    (is (= 10 (wt-weight tree)))
    (dotimes (key 10)
      (multiple-value-bind (value found?) (wt-lookup #'< tree key)
        (is (eq t found?))
        (is (= (* key 10) value))))
    (multiple-value-bind (value found?) (wt-lookup #'< tree 42)
      (is (null found?))
      (is (null value)))))

(test wt-add-replaces-existing-key
  "Re-adding an already-present key replaces its value without
increasing the tree's own WEIGHT."
  (let* ((tree (wt-add :dummy-repo #'< (wt-singleton :dummy-repo 1 :one) 1 :uno)))
    (is (= 1 (wt-weight tree)))
    (is (eq :uno (wt-node-value tree)))))

(test wt-add-does-not-mutate-original-tree
  "WT-ADD never mutates its NODE argument: the original tree remains
valid and unaffected after building a new tree from it."
  (let* ((original (wt-add :dummy-repo #'< nil 1 :one))
         (extended (wt-add :dummy-repo #'< original 2 :two)))
    (is (= 1 (wt-weight original)))
    (is (= 2 (wt-weight extended)))
    (multiple-value-bind (value found?) (wt-lookup #'< original 2)
      (is (null found?))
      (is (null value)))))

(test wt-delete-removes-key-and-preserves-others
  "WT-DELETE removes exactly the requested key, leaving every other
association intact, and is a no-op (in effect) for a key never
present."
  (let ((tree nil))
    (dolist (key '(5 3 8 1 4 7 9 2 6 0))
      (setf tree (wt-add :dummy-repo #'< tree key key)))
    (let ((without-4 (wt-delete :dummy-repo #'< tree 4)))
      (is (= 9 (wt-weight without-4)))
      (multiple-value-bind (value found?) (wt-lookup #'< without-4 4)
        (is (null found?))
        (is (null value)))
      (dolist (key '(5 3 8 1 7 9 2 6 0))
        (multiple-value-bind (value found?) (wt-lookup #'< without-4 key)
          (is (eq t found?))
          (is (= key value))))
      ;; Deleting an absent key leaves the tree's weight unchanged.
      (is (= 9 (wt-weight (wt-delete :dummy-repo #'< without-4 4)))))))

(test wt-min-and-wt-max
  "WT-MIN and WT-MAX return the smallest and largest key/value pairs
of a non-empty tree, and signal an error for the empty tree."
  (let ((tree nil))
    (dolist (key '(5 3 8 1 4 7 9 2 6 0))
      (setf tree (wt-add :dummy-repo #'< tree key (- key))))
    (multiple-value-bind (key value) (wt-min tree)
      (is (= 0 key))
      (is (= 0 value)))
    (multiple-value-bind (key value) (wt-max tree)
      (is (= 9 key))
      (is (= -9 value))))
  (signals error (wt-min nil))
  (signals error (wt-max nil)))

(test wt-fold-keys-values-and-alist-are-in-ascending-order
  "WT-FOLD (and so WT-KEYS/WT-VALUES/WT->ALIST, all built atop it)
visits every association exactly once, in strictly ascending key
order, regardless of insertion order."
  (let ((tree nil))
    (dolist (key '(5 3 8 1 4 7 9 2 6 0))
      (setf tree (wt-add :dummy-repo #'< tree key (* key key))))
    (is (equal '(0 1 2 3 4 5 6 7 8 9) (wt-keys tree)))
    (is (equal '(0 1 4 9 16 25 36 49 64 81) (wt-values tree)))
    (is (equal '((0 . 0) (1 . 1) (2 . 4) (3 . 9) (4 . 16)
                 (5 . 25) (6 . 36) (7 . 49) (8 . 64) (9 . 81))
                (wt->alist tree)))
    (is (equal '() (wt-keys nil)))))

(test serialize-persistent-wttree-node-is-idempotent
  "SERIALIZE-PERSISTENT-WTTREE-NODE does nothing (beyond returning
the existing SHA) for a node that has already been persisted."
  (let* ((sha "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
         (node (make-instance 'persistent-wttree :repository :dummy-repo :sha sha)))
    (is (string= sha (serialize-persistent-wttree-node node)))
    (is (null (get-entries node)))))

(test serialize-persistent-wttree-node-requires-a-key
  "SERIALIZE-PERSISTENT-WTTREE-NODE signals an error when KEY has
not been set."
  (let ((node (make-instance 'persistent-wttree :repository :dummy-repo :value 1)))
    (with-fake-git-hash-object ()
      (signals error (serialize-persistent-wttree-node node)))))

(test serialize-persistent-wttree-node-writes-standard-readme
  "SERIALIZE-PERSISTENT-WTTREE-NODE writes the exact, fixed
README.md markdown content as raw (non-atom-envelope) UTF-8 bytes,
and omits \"left\"/\"right\" entries entirely for an empty child."
  (let* ((calls '())
         (node (wt-singleton :dummy-repo :k :v)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-wttree-node node))
    (is (find (sb-ext:string-to-octets +persistent-wttree-readme+ :external-format :utf-8)
              calls :key #'third :test #'equalp))
    (is (equal (list ".meta" "README.md" "key" "value") (mapcar #'car (get-entries node))))))

(test serialize-persistent-wttree-node-caches-weight-in-meta
  "SERIALIZE-PERSISTENT-WTTREE-NODE writes a \".meta\" blob whose
content, once parsed back via %DESERIALIZE-PERSISTENT-WTTREE-META,
reports the node's own WEIGHT."
  (let* ((calls '())
         (tree nil))
    (dolist (key '(2 1 3))
      (setf tree (wt-add :dummy-repo #'< tree key key)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-wttree-node tree))
    (let* ((meta-entry (cdr (assoc ".meta" (get-entries tree) :test #'string=)))
           (meta-octets (third (find (sha meta-entry) calls
                                      :key (lambda (call) (%fake-sha-for (second call) (third call)))
                                      :test #'string=))))
      (is (= 3 (%deserialize-persistent-wttree-meta meta-octets))))))

(test wt-add-serialize-deserialize-and-lookup-round-trip
  "Building a tree via WT-ADD, persisting it via SERIALIZE-
PERSISTENT-WTTREE-NODE, then reloading it from a hollow proxy of the
same SHA reconstructs a tree that WT-LOOKUP/WT-KEYS/WT-VALUES report
identically to the original -- exercising the full write-then-read-
back path through DESERIALIZE-PERSISTENT-WTTREE-NODE and
INFLATE-GIT-PROXY's lazy CHANGE-CLASS retyping of nested children."
  (with-fake-git-repository ()
    (let ((tree nil))
      (dolist (key '(5 3 8 1 4 7 9 2 6 0))
        (setf tree (wt-add :dummy-repo #'< tree key (* key 10))))
      (serialize-persistent-wttree-node tree)
      (let ((hollow (make-instance 'persistent-wttree :repository :dummy-repo :sha (sha tree))))
        (is (= (wt-weight tree) (wt-weight hollow)))
        (dotimes (key 10)
          (multiple-value-bind (value found?) (wt-lookup #'< hollow key)
            (is (eq t found?))
            (is (= (* key 10) value))))
        (is (equal (wt-keys tree) (wt-keys hollow)))
        (is (equal (wt-values tree) (wt-values hollow)))))))

(test wt-weight-does-not-force-child-content
  "WT-WEIGHT of a hollow child proxy is available straight from that
child's own \".meta\" blob, in O(1), without ever fetching that
child's own KEY/VALUE or its further children's content: exercised
here by restricting the fake GIT-CAT-FILE mapping to only the root's
and its two immediate children's own tree/.meta blobs (deliberately
omitting every KEY/VALUE blob and every grandchild's content), and
confirming WT-WEIGHT of each immediate child still succeeds."
  (let* ((calls '())
         (tree nil))
    (dolist (key '(2 1 3 0 4))
      (setf tree (wt-add :dummy-repo #'< tree key key)))
    (with-recording-git-hash-object (calls)
      (serialize-persistent-wttree-node tree))
    (flet ((octets-for (sha)
             (third (find sha calls
                          :key (lambda (call) (%fake-sha-for (second call) (third call)))
                          :test #'string=))))
      (let* ((root-sha (sha tree))
             (left (%wt-raw-left tree))
             (right (%wt-raw-right tree))
             (allowed-shas (remove nil (list root-sha (and left (sha left)) (and right (sha right)))))
             (mapping (mapcar (lambda (node-sha)
                                 (cons node-sha (octets-for node-sha)))
                               allowed-shas)))
        (dolist (node (remove nil (list tree left right)))
          (let ((meta-sha (sha (cdr (assoc ".meta" (get-entries node) :test #'string=)))))
            (push (cons meta-sha (octets-for meta-sha)) mapping)))
        (let ((type-map (mapcar (lambda (call)
                                   (cons (%fake-sha-for (second call) (third call)) (second call)))
                                 calls)))
          (with-fake-git-type (type-map)
            (with-fake-git-cat-file (mapping)
              (is (integerp (wt-weight (make-instance 'persistent-wttree :repository :dummy-repo
                                                                          :sha root-sha))))
              (when left
                (is (integerp (wt-weight (make-instance 'persistent-wttree :repository :dummy-repo
                                                                            :sha (sha left))))))
              (when right
                (is (integerp (wt-weight (make-instance 'persistent-wttree :repository :dummy-repo
                                                                            :sha (sha right)))))))))))))

(test deserialize-persistent-wttree-node-signals-error-for-missing-entries
  "DESERIALIZE-PERSISTENT-WTTREE-NODE signals an error if the
underlying tree is missing any of the required \".meta\", \"README.md\",
\"key\", or \"value\" entries."
  (let* ((blob-sha "3333333333333333333333333333333333333333")
         (blob (make-instance 'git-blob :repository :dummy-repo :sha blob-sha))
         (incomplete-tree (make-instance 'git-tree :repository :dummy-repo
                                                    :entries (list (cons "key" blob))))
         (tree-octets (serialize-tree incomplete-tree))
         (hollow (make-instance 'persistent-wttree :repository :dummy-repo)))
    (with-fake-git-type ((list (cons blob-sha "blob")))
      (signals error (deserialize-persistent-wttree-node hollow tree-octets #())))))
