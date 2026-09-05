;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; PERSISTENT-WTTREE implements GitHack's 1:1 mapping between a
;;; single node of an immutable weight-balanced binary tree (Adams'
;;; tree, as used by MIT/GNU Scheme's WTTREE library) and a Git tree
;;; object: every persistent wttree node serializes to a Git tree
;;; containing --
;;;
;;;   .meta      a blob holding the serialized (:TAG :WTTREE
;;;              :WEIGHT n) property list, where n is this node's
;;;              own subtree size (itself plus every descendant)
;;;   README.md  a blob holding a fixed, human-readable description
;;;              of this layout
;;;   key        a proxy pointer to the object held as this node's key
;;;   value      a proxy pointer to the object held as this node's value
;;;   left       a proxy pointer to this node's left child -- entirely
;;;              ABSENT (no such entry at all) for an empty left subtree
;;;   right      a proxy pointer to this node's right child -- entirely
;;;              ABSENT for an empty right subtree
;;;
;;; so that structurally identical subtrees hash to the same SHA and
;;; are stored only once in Git's object database, and a hollow,
;;; just-deserialized node's own WEIGHT is known immediately, in
;;; O(1), straight from its ".meta" blob -- exactly as a hollow
;;; PERSISTENT-VECTOR's own LENGTH is -- without ever fetching that
;;; node's KEY, VALUE, or either child's own contents. Every Adams
;;; rebalancing operation (WT-ADD, WT-DELETE, and their shared %WT-
;;; JOIN/%WT-CONCAT internals) queries only WT-WEIGHT while deciding
;;; *whether* to rotate, so a whole rotation-free traversal path
;;; never forces a single extra Git fetch beyond each visited node's
;;; own ".meta" blob; a child's KEY/VALUE/further children are only
;;; ever fetched once an actual rotation (or a search/fold that must
;;; descend into it) genuinely requires reading that child's own
;;; content. An empty tree is simply the Lisp value NIL -- there is
;;; no persisted representation of "the empty tree" at all. Since a
;;; non-empty node's on-disk shape *is* an ordinary Git tree,
;;; PERSISTENT-WTTREE is implemented as a GIT-TREE subclass, reusing
;;; GIT-TREE's ENTRIES slot and SERIALIZE-TREE's binary encoding for
;;; its own underlying tree object.

(defparameter +persistent-wttree-readme+
  "# Persistent Weight-Balanced Tree Node

This Git tree represents a single node in an immutable, weight-balanced binary tree (Adams' tree).

  * **.meta**: Contains the serialized property list defining the `:tag` and the `:weight` (size of the subtree).
  * **key** / **value**: SHA pointers to the data held in this node.
  * **left** / **right**: SHA pointers to the child nodes of this tree.

This 1:1 node-to-tree mapping ensures that immutable updates only generate O(log N) new Git objects, allowing Git to naturally deduplicate and share unmodified subtrees.
"
  "The fixed README.md content SERIALIZE-PERSISTENT-WTTREE-NODE
writes, verbatim and unencoded, into every persistent wttree node.")

(defclass persistent-wttree (git-tree)
  ((key
    :initarg :key
    :initform nil
    :accessor %wt-raw-key
    :documentation
    "The GIT-OBJECT proxy held as this node's key -- a GIT-BLOB for
a scalar/atom, or any other GIT-OBJECT for a compound key -- or NIL
if not yet set (before serializing) or not yet loaded (after
deserializing). See WT-NODE-KEY, which decodes this into the real
Lisp key.")
   (value
    :initarg :value
    :initform nil
    :accessor %wt-raw-value
    :documentation
    "The GIT-OBJECT proxy held as this node's value, or NIL if not
yet set/loaded. See WT-NODE-VALUE, which decodes this into the real
Lisp value.")
   (left
    :initarg :left
    :initform nil
    :accessor %wt-raw-left
    :documentation
    "This node's left child: NIL for an empty left subtree, or a
GIT-OBJECT proxy (a PERSISTENT-WTTREE, or a plain GIT-TREE not yet
retyped by %ENSURE-PERSISTENT-WTTREE-NODE-LOADED) otherwise. See
WT-NODE-LEFT.")
   (right
    :initarg :right
    :initform nil
    :accessor %wt-raw-right
    :documentation
    "This node's right child: NIL for an empty right subtree, or a
GIT-OBJECT proxy otherwise. See WT-NODE-RIGHT.")
   (weight
    :initarg :weight
    :initform nil
    :accessor %wt-raw-weight
    :type (or null integer)
    :documentation
    "This node's own subtree size (itself plus every descendant),
or NIL if not yet computed/loaded. Known immediately from a hollow
(just-deserialized) node's \".meta\" blob, in O(1), without fetching
its KEY, VALUE, or either child's own contents. See WT-WEIGHT."))
  (:documentation
   "Proxy for a single node of an immutable weight-balanced binary
tree (Adams' tree), stored as a Git tree with \".meta\", \"README.md\",
\"key\", and \"value\" entries, plus a \"left\"/\"right\" entry for
each non-empty child (entirely absent for an empty one). The empty
tree itself is simply the Lisp value NIL, never a PERSISTENT-WTTREE
instance. See SERIALIZE-PERSISTENT-WTTREE-NODE and DESERIALIZE-
PERSISTENT-WTTREE-NODE for the on-disk representation, and WT-ADD/
WT-DELETE/WT-LOOKUP/WT-FOLD for the public Adams-tree operations."))

(defun %serialize-persistent-wttree-meta (weight)
  "Encode the small property list (:TAG :WTTREE :WEIGHT WEIGHT) as
a UTF-8 octet vector, via %SERIALIZE-PLIST: the exact raw content of
a persistent wttree node's \".meta\" blob."
  (%serialize-plist (list :tag :wttree :weight weight)))

(defun %deserialize-persistent-wttree-meta (octets)
  "Inverse of %SERIALIZE-PERSISTENT-WTTREE-META: parse OCTETS -- the
raw content of a persistent wttree node's \".meta\" blob -- via
%DESERIALIZE-PLIST, and return its :WEIGHT. Signals an error if
OCTETS is not a plist whose :TAG is :WTTREE."
  (let ((plist (%deserialize-plist octets)))
    (unless (eq (getf plist :tag) :wttree)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent wttree .meta blob: ~S."
             :format-arguments (list plist)))
    (getf plist :weight)))

(defun %wt-decode (git-object)
  "Return the real Lisp value GIT-OBJECT represents: its decoded
PAYLOAD, if GIT-OBJECT is a GIT-BLOB (fetching it from the
repository first via %ENSURE-BLOB-LOADED, if not yet loaded); or
GIT-OBJECT itself, unchanged, for any other (compound) GIT-OBJECT
proxy."
  (if (typep git-object 'git-blob)
      (get-payload (%ensure-blob-loaded git-object))
      git-object))

(defun %wt-wrap (repository value)
  "Return VALUE unchanged if it is already a GIT-OBJECT proxy;
otherwise, return a fresh, already-loaded GIT-BLOB, associated with
REPOSITORY, wrapping VALUE as a serializable atom."
  (if (typep value 'git-object)
      value
      (make-instance 'git-blob :repository repository :payload value :loaded? t)))

(defun %ensure-persistent-wttree-node-loaded (node)
  "Ensure NODE's KEY/VALUE/LEFT/RIGHT/WEIGHT slots are populated:
first, if NODE is merely a plain, not-yet-more-specifically-typed
GIT-TREE (as returned by WT-NODE-LEFT/WT-NODE-RIGHT for a child
freshly fetched from Git, since neither DESERIALIZE-TREE nor
INFLATE-GIT-PROXY ever distinguish a nested PERSISTENT-WTTREE from
an ordinary GIT-TREE), retype it in place into a PERSISTENT-WTTREE
via CHANGE-CLASS; then, if NODE is not yet loaded, fetch its raw
tree bytes and its own \".meta\" blob via GIT-CAT-FILE and populate
it via DESERIALIZE-PERSISTENT-WTTREE-NODE. This is the *only* I/O
this function ever performs: NODE's own KEY/VALUE/LEFT/RIGHT remain
hollow, unfetched proxies, so querying NODE's own WEIGHT (or
retyping/loading it in the first place) never cascades into a
fetch of its children's own contents. Returns NODE."
  (unless (typep node 'persistent-wttree)
    (change-class node 'persistent-wttree))
  (unless (get-loaded? node)
    (let* ((repository (get-repository node))
           (tree-octets (git-cat-file repository (sha node)))
           (entries (deserialize-tree repository tree-octets))
           (meta-entry (assoc ".meta" entries :test #'string=)))
      (unless meta-entry
        (error 'malformed-git-object-error
               :format-control "Malformed persistent wttree node: missing \".meta\" entry."))
      (deserialize-persistent-wttree-node
       node tree-octets (git-cat-file repository (sha (cdr meta-entry))))))
  node)

(defun wt-weight (node)
  "Return NODE's (a PERSISTENT-WTTREE, a not-yet-retyped GIT-TREE,
or NIL) cached subtree size: 0 if NODE is NIL (the empty tree),
otherwise its own WEIGHT, forcing at most a shallow load of NODE's
own tree and \".meta\" blobs (via %ENSURE-PERSISTENT-WTTREE-NODE-
LOADED) -- never a fetch of NODE's KEY, VALUE, or either child's own
contents. Every Adams rebalancing decision queries only this."
  (if (null node) 0 (%wt-raw-weight (%ensure-persistent-wttree-node-loaded node))))

(defun wt-empty-p (node)
  "Return true if NODE is the empty tree (NIL)."
  (null node))

(defun wt-node-key (node)
  "Return the real Lisp key held by NODE (a non-NIL PERSISTENT-
WTTREE or not-yet-retyped GIT-TREE), decoded via %WT-DECODE, forcing
NODE itself (but not either child) to be loaded first."
  (%wt-decode (%wt-raw-key (%ensure-persistent-wttree-node-loaded node))))

(defun wt-node-value (node)
  "Return the real Lisp value held by NODE, decoded via %WT-DECODE,
forcing NODE itself (but not either child) to be loaded first."
  (%wt-decode (%wt-raw-value (%ensure-persistent-wttree-node-loaded node))))

(defun wt-node-left (node)
  "Return NODE's left child: NIL for an empty left subtree, or a
GIT-OBJECT proxy (not yet forced loaded) otherwise. Forces NODE
itself to be loaded first."
  (%wt-raw-left (%ensure-persistent-wttree-node-loaded node)))

(defun wt-node-right (node)
  "Return NODE's right child: NIL for an empty right subtree, or a
GIT-OBJECT proxy (not yet forced loaded) otherwise. Forces NODE
itself to be loaded first."
  (%wt-raw-right (%ensure-persistent-wttree-node-loaded node)))

(defun %wt-log2-less-p (left right)
  "Return true if LEFT and RIGHT (both non-negative integer subtree
weights) differ enough, in Adams' balance criterion, that LEFT is
considered strictly smaller on a base-2 logarithmic scale."
  (and (< left right)
       (< (ash (logand left right) 1) right)))

(defun %wt-weight-too-small-p (small large)
  "Return true if SMALL is too small relative to LARGE for %WT-JOIN
to leave them combined without rebalancing, per Adams' criterion."
  (%wt-log2-less-p small (ash large -1)))

(defun %wt-single-rotation-p (inner outer)
  "Return true if a single (as opposed to double) rotation suffices
to rebalance a node whose heavy child's own INNER/OUTER grandchild
weights are as given."
  (not (%wt-log2-less-p outer inner)))

(defun %wt-make-node (repository key value left right)
  "Construct and return a fresh, already-loaded PERSISTENT-WTTREE
node (with a WEIGHT of one more than LEFT's and RIGHT's own WT-WEIGHT,
computed immediately) for KEY/VALUE (wrapped via %WT-WRAP if not
already GIT-OBJECT proxies) and children LEFT/RIGHT (each NIL or an
existing/fresh node). Performs no I/O of its own; see SERIALIZE-
PERSISTENT-WTTREE-NODE for that."
  (make-instance 'persistent-wttree
                 :repository repository
                 :loaded? t
                 :key (%wt-wrap repository key)
                 :value (%wt-wrap repository value)
                 :left left
                 :right right
                 :weight (+ 1 (wt-weight left) (wt-weight right))))

(defun %wt-join (repository key value left right)
  "Combine KEY/VALUE with children LEFT/RIGHT into a single,
correctly rebalanced PERSISTENT-WTTREE node, applying at most one
Adams single or double rotation (querying only WT-WEIGHT to decide
whether, and which, rotation is needed) if LEFT and RIGHT differ too
much in weight; otherwise simply %WT-MAKE-NODE's them together
directly. The classic 'join' step every Adams-tree operation (WT-ADD,
WT-DELETE, %WT-CONCAT) is built from."
  (let ((left-weight (wt-weight left))
        (right-weight (wt-weight right)))
    (cond
      ((%wt-weight-too-small-p left-weight right-weight)
       (let ((inner (wt-node-left right))
             (outer (wt-node-right right)))
         (if (%wt-single-rotation-p (wt-weight inner) (wt-weight outer))
             (%wt-make-node repository (wt-node-key right) (wt-node-value right)
                            (%wt-make-node repository key value left inner)
                            outer)
             (%wt-make-node repository (wt-node-key inner) (wt-node-value inner)
                            (%wt-make-node repository key value left (wt-node-left inner))
                            (%wt-make-node repository (wt-node-key right) (wt-node-value right)
                                           (wt-node-right inner) outer)))))
      ((%wt-weight-too-small-p right-weight left-weight)
       (let ((outer (wt-node-left left))
             (inner (wt-node-right left)))
         (if (%wt-single-rotation-p (wt-weight inner) (wt-weight outer))
             (%wt-make-node repository (wt-node-key left) (wt-node-value left)
                            outer
                            (%wt-make-node repository key value inner right))
             (%wt-make-node repository (wt-node-key inner) (wt-node-value inner)
                            (%wt-make-node repository (wt-node-key left) (wt-node-value left)
                                           outer (wt-node-left inner))
                            (%wt-make-node repository key value (wt-node-right inner) right)))))
      (t (%wt-make-node repository key value left right)))))

(defun wt-singleton (repository key value)
  "Return a fresh PERSISTENT-WTTREE node of weight 1 holding exactly
the single association KEY/VALUE."
  (%wt-make-node repository key value nil nil))

(defun wt-add (repository key-less-p node key value)
  "Return a new Adams tree, structurally sharing with NODE (a
PERSISTENT-WTTREE, a not-yet-retyped GIT-TREE, or NIL for the empty
tree) wherever KEY's own path is unaffected, associating KEY with
VALUE. KEY-LESS-P is a strict order predicate on keys. NODE itself
is left completely unmodified. REPOSITORY is used only to construct
any brand-new nodes/blobs (via %WT-WRAP); it need not equal any
existing node's own GET-REPOSITORY."
  (if (null node)
      (wt-singleton repository key value)
      (let ((node-key (wt-node-key node)))
        (cond
          ((funcall key-less-p key node-key)
           (%wt-join repository node-key (wt-node-value node)
                     (wt-add repository key-less-p (wt-node-left node) key value)
                     (wt-node-right node)))
          ((funcall key-less-p node-key key)
           (%wt-join repository node-key (wt-node-value node)
                     (wt-node-left node)
                     (wt-add repository key-less-p (wt-node-right node) key value)))
          (t (%wt-make-node repository node-key value (wt-node-left node) (wt-node-right node)))))))

(defun wt-lookup (key-less-p node key &optional default)
  "Return two values: the value associated with KEY in NODE (a
PERSISTENT-WTTREE, a not-yet-retyped GIT-TREE, or NIL), and T; or
DEFAULT and NIL if KEY is not present. KEY-LESS-P is the same strict
order predicate NODE was built with. Only ever descends into the
single root-to-KEY search path, forcing each node visited along it
(but no sibling subtree) to be loaded."
  (loop while node
        do (let ((node-key (wt-node-key node)))
             (cond
               ((funcall key-less-p key node-key) (setf node (wt-node-left node)))
               ((funcall key-less-p node-key key) (setf node (wt-node-right node)))
               (t (return-from wt-lookup (values (wt-node-value node) t))))))
  (values default nil))

(defun %wt-extreme (node direction)
  "Return the leftmost (DIRECTION :LEFT) or rightmost (DIRECTION
:RIGHT) node of the non-empty Adams tree rooted at NODE. Signals an
error if NODE is NIL (the empty tree)."
  (when (null node)
    (error 'invalid-argument-error
           :format-control "Cannot take the extreme node of an empty WT-tree."))
  (let ((current node))
    (loop for next = (if (eq direction :left) (wt-node-left current) (wt-node-right current))
          while next
          do (setf current next))
    current))

(defun wt-min (node)
  "Return two values, the smallest key in the non-empty Adams tree
rooted at NODE and its associated value. Signals an error if NODE is
NIL."
  (let ((extreme (%wt-extreme node :left)))
    (values (wt-node-key extreme) (wt-node-value extreme))))

(defun wt-max (node)
  "Return two values, the largest key in the non-empty Adams tree
rooted at NODE and its associated value. Signals an error if NODE is
NIL."
  (let ((extreme (%wt-extreme node :right)))
    (values (wt-node-key extreme) (wt-node-value extreme))))

(defun %wt-remove-extreme (repository node direction)
  "Return a new Adams tree equal to NODE (non-NIL) with its own
%WT-EXTREME (in DIRECTION) removed, rebalancing via %WT-JOIN as
necessary."
  (when (null node)
    (error 'invalid-argument-error
           :format-control "Cannot remove the extreme node of an empty WT-tree."))
  (let* ((left (wt-node-left node))
         (right (wt-node-right node))
         (next (if (eq direction :left) left right)))
    (if (null next)
        (if (eq direction :left) right left)
        (%wt-join repository (wt-node-key node) (wt-node-value node)
                  (if (eq direction :left) (%wt-remove-extreme repository left direction) left)
                  (if (eq direction :right) (%wt-remove-extreme repository right direction) right)))))

(defun %wt-concat (repository left right)
  "Return a new Adams tree holding exactly the union of LEFT's and
RIGHT's own associations, assuming every key in LEFT is strictly
less than every key in RIGHT (the shape WT-DELETE always concats in).
Rebalances via %WT-JOIN, exactly like every other node-combining
operation here."
  (cond
    ((null left) right)
    ((null right) left)
    (t (multiple-value-bind (key value) (wt-min right)
         (%wt-join repository key value left (%wt-remove-extreme repository right :left))))))

(defun wt-delete (repository key-less-p node key)
  "Return a new Adams tree, structurally sharing with NODE wherever
KEY's own path is unaffected, with any association for KEY removed
(or NODE's own structural-equivalent, unchanged in effect, if KEY was
never present). NODE itself is left completely unmodified."
  (if (null node)
      nil
      (let ((node-key (wt-node-key node)))
        (cond
          ((funcall key-less-p key node-key)
           (%wt-join repository node-key (wt-node-value node)
                     (wt-delete repository key-less-p (wt-node-left node) key)
                     (wt-node-right node)))
          ((funcall key-less-p node-key key)
           (%wt-join repository node-key (wt-node-value node)
                     (wt-node-left node)
                     (wt-delete repository key-less-p (wt-node-right node) key)))
          (t (%wt-concat repository (wt-node-left node) (wt-node-right node)))))))

(defun wt-fold (procedure initial node)
  "Fold PROCEDURE (a function of three arguments: an accumulator, a
key, and a value) over every association in the Adams tree rooted at
NODE, in ascending key order, starting from INITIAL. Returns the
final accumulator."
  (if (null node)
      initial
      (wt-fold procedure
               (funcall procedure (wt-fold procedure initial (wt-node-left node))
                        (wt-node-key node) (wt-node-value node))
               (wt-node-right node))))

(defun wt-keys (node)
  "Return a fresh list of every key in the Adams tree rooted at
NODE, in ascending order."
  (nreverse (wt-fold (lambda (keys key value) (declare (ignore value)) (cons key keys)) nil node)))

(defun wt-values (node)
  "Return a fresh list of every value in the Adams tree rooted at
NODE, in ascending key order."
  (nreverse (wt-fold (lambda (values key value) (declare (ignore key)) (cons value values)) nil node)))

(defun wt->alist (node)
  "Return a fresh (KEY . VALUE) alist of every association in the
Adams tree rooted at NODE, in ascending key order."
  (nreverse (wt-fold (lambda (alist key value) (acons key value alist)) nil node)))

(defgeneric %persist-wttree-component-by-type (git-object)
  (:documentation
   "Persist GIT-OBJECT (which is known not to have a SHA yet) to
Git's object database according to its concrete type, and return the
resulting SHA. Broken out of %WT-PERSIST-COMPONENT so this dispatch
is its own generic function, with one DEFMETHOD per concrete type in
place of an ETYPECASE clause."))

(defmethod %persist-wttree-component-by-type ((git-object persistent-wttree))
  (serialize-persistent-wttree-node git-object))

(defmethod %persist-wttree-component-by-type ((git-object persistent-cons))
  (serialize-persistent-cons git-object))

(defmethod %persist-wttree-component-by-type ((git-object persistent-vector))
  (serialize-persistent-vector git-object))

(defmethod %persist-wttree-component-by-type ((git-object git-tree))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "tree" (serialize-tree git-object))))

(defmethod %persist-wttree-component-by-type ((git-object git-blob))
  (setf (sha git-object)
        (git-hash-object (get-repository git-object) "blob"
                          (serialize-atom (get-payload git-object)))))

(defmethod %persist-cons-component-by-type ((git-object persistent-wttree))
  (serialize-persistent-wttree-node git-object))

(defmethod %persist-vector-component-by-type ((git-object persistent-wttree))
  (serialize-persistent-wttree-node git-object))

(defun %wt-persist-component (git-object)
  "Ensure GIT-OBJECT (a GIT-BLOB, a plain GIT-TREE, a PERSISTENT-CONS,
a PERSISTENT-VECTOR, or a nested PERSISTENT-WTTREE) has a SHA,
persisting it if it does not already. Returns GIT-OBJECT's SHA."
  (or (sha git-object)
      (%persist-wttree-component-by-type git-object)))

(defun %wt-persist-child (node)
  "Persist NODE (a possibly-NIL PERSISTENT-WTTREE child of some
other node currently being serialized), preserving structural
sharing: NIL is returned unchanged (an empty child stays absent); an
already-persisted NODE (SHA already set -- an untouched, shared
subtree reused unmodified from some earlier tree) is returned
unchanged, its own tree bytes never re-examined or re-hashed; and a
freshly constructed, not-yet-persisted NODE is recursively persisted
via SERIALIZE-PERSISTENT-WTTREE-NODE. Returns NODE (itself now
persisted, if it was not already)."
  (cond
    ((null node) nil)
    ((sha node) node)
    (t (serialize-persistent-wttree-node node) node)))

(defun serialize-persistent-wttree-node (node)
  "Compute NODE's own WEIGHT (one more than its LEFT's and RIGHT's
own WT-WEIGHT -- ordinarily already known, since %WT-MAKE-NODE
always computes it at construction time), create and persist its
standard \".meta\" and \"README.md\" blobs, recursively persist its
KEY, VALUE, LEFT, and RIGHT (via %WT-PERSIST-COMPONENT/%WT-PERSIST-
CHILD -- the latter perfectly preserving structural sharing for any
untouched child reused from an earlier tree), and finally write
NODE's own Git tree object, omitting its \"left\"/\"right\" entries
entirely for an empty child. Like SERIALIZE-PERSISTENT-CONS/-VECTOR,
this performs real I/O, not pure encoding. Signals an error if
NODE's KEY has not been set. Returns NODE's own SHA, doing nothing
further if NODE already has one."
  (or (sha node)
      (let* ((repository (get-repository node))
             (key-object (%wt-raw-key node))
             (value-object (%wt-raw-value node))
             (left (%wt-persist-child (%wt-raw-left node)))
             (right (%wt-persist-child (%wt-raw-right node)))
             (weight (or (%wt-raw-weight node) (+ 1 (wt-weight left) (wt-weight right))))
             (meta-blob (make-instance 'git-blob :repository repository
                                                  :sha (git-hash-object
                                                        repository "blob"
                                                        (%serialize-persistent-wttree-meta weight))))
             (readme-blob (make-instance 'git-blob :repository repository
                                                    :sha (git-hash-object
                                                          repository "blob"
                                                          (sb-ext:string-to-octets
                                                           +persistent-wttree-readme+
                                                           :external-format :utf-8)))))
        (unless key-object
          (error 'unpersisted-object-error
                 :format-control "Cannot serialize persistent wttree node: its KEY has not been set."))
        (%wt-persist-component key-object)
        (%wt-persist-component value-object)
        (setf (%wt-raw-left node) left)
        (setf (%wt-raw-right node) right)
        (setf (%wt-raw-weight node) weight)
        (setf (get-entries node)
              (list* (cons ".meta" meta-blob)
                     (cons "README.md" readme-blob)
                     (cons "key" key-object)
                     (cons "value" value-object)
                     (append (and left (list (cons "left" left)))
                             (and right (list (cons "right" right))))))
        (setf (sha node) (git-hash-object repository "tree" (serialize-tree node)))
        (setf (get-loaded? node) t)
        (sha node))))

(defun deserialize-persistent-wttree-node (node tree-octets meta-octets)
  "Parse TREE-OCTETS -- the raw byte-vector of NODE's own underlying
Git tree object -- together with META-OCTETS -- the raw byte-vector
of that tree's \".meta\" blob -- and populate NODE's ENTRIES, KEY,
VALUE, LEFT, RIGHT, and WEIGHT slots. KEY and VALUE are set to
hollow (unloaded) GIT-OBJECT proxies via INFLATE-GIT-PROXY (as
DESERIALIZE-TREE already arranges); LEFT/RIGHT are likewise set to
hollow proxies if present, or NIL if their entry is entirely absent
(an empty child) -- so no key/value/child content is ever fetched
eagerly by this function, only NODE's own cached WEIGHT, which this
function is required to populate immediately, straight from
META-OCTETS. Signals an error if TREE-OCTETS' entries do not include
\".meta\", \"README.md\", \"key\", and \"value\". Marks NODE loaded
and returns it."
  (let* ((repository (get-repository node))
         (entries (deserialize-tree repository tree-octets))
         (key-entry (assoc "key" entries :test #'string=))
         (value-entry (assoc "value" entries :test #'string=))
         (left-entry (assoc "left" entries :test #'string=))
         (right-entry (assoc "right" entries :test #'string=)))
    (unless (assoc ".meta" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent wttree node: missing \".meta\" entry."))
    (unless (assoc "README.md" entries :test #'string=)
      (error 'malformed-git-object-error
             :format-control "Malformed persistent wttree node: missing \"README.md\" entry."))
    (unless key-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent wttree node: missing \"key\" entry."))
    (unless value-entry
      (error 'malformed-git-object-error
             :format-control "Malformed persistent wttree node: missing \"value\" entry."))
    (let ((weight (%deserialize-persistent-wttree-meta meta-octets)))
      (setf (get-entries node) entries)
      (setf (%wt-raw-key node) (cdr key-entry))
      (setf (%wt-raw-value node) (cdr value-entry))
      (setf (%wt-raw-left node) (and left-entry (cdr left-entry)))
      (setf (%wt-raw-right node) (and right-entry (cdr right-entry)))
      (setf (%wt-raw-weight node) weight)
      (setf (get-loaded? node) t)
      node)))
