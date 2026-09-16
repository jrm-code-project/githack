;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; The in-memory k-bucket routing table: +ID-BITS+ buckets (one per
;;; possible XOR-distance bit-length), each holding up to +K+ CONTACTs,
;;; ordered least- to most-recently-seen. This structure is always
;;; purely transient/in-process (never itself serialized to Git -- see
;;; persistence.lisp for how its *contents* get checkpointed into a
;;; node's own local GitHack database) since it is a node's hot-path
;;; working set, updated on every single inbound datagram; committing a
;;; Git object per packet would be far too slow (see persistence.lisp's
;;; commentary on why persistence is instead periodic/checkpointed).
;;;
;;; Thread safety: a KADEMLIA-NODE's listener thread and any thread
;;; performing an iterative lookup (see node.lisp) can both be mutating
;;; the same ROUTING-TABLE concurrently, so every mutating operation
;;; here takes ROUTING-TABLE's own lock; per this project's established
;;; thread-safety convention (see git-object.lisp's %CAS-INSTALL-ONCE!/
;;; WITH-OBJECT-LOAD-LOCK work), the lock is held only while touching
;;; the bucket vector itself, never across a network call (see
;;; ROUTING-TABLE-INSERT!'s PING-FN handling below) -- holding it across
;;; a PING RPC would both stall unrelated routing-table readers/writers
;;; for the RPC's full timeout and risk deadlocking against a PONG
;;; handler that turns around and calls ROUTING-TABLE-INSERT! itself.

(in-package "GITHACK-KADEMLIA")

(defstruct (routing-table
            (:constructor %make-routing-table (self-id buckets lock))
            (:predicate routing-table-p))
  "SELF-ID is the owning node's own NODE-ID (never itself stored as a
contact -- see ROUTING-TABLE-INSERT!). BUCKETS is a SIMPLE-VECTOR of
length +ID-BITS+, each element a list of CONTACT instances (bucket I
holds contacts at XOR distance in [2^I, 2^(I+1)) from SELF-ID -- see
NODE-ID-BUCKET-INDEX), ordered from least- to most-recently-seen: the
first element is the next eviction candidate, the last the freshest.
LOCK serializes every mutating and reading access to BUCKETS."
  (self-id 0 :type (integer 0 *) :read-only t)
  (buckets nil :read-only t)
  (lock nil :read-only t))

(defun make-routing-table (self-id)
  "Return a fresh, empty ROUTING-TABLE rooted at SELF-ID (the owning
node's own NODE-ID)."
  (%make-routing-table self-id
                        (make-array +id-bits+ :initial-element nil)
                        (sb-thread:make-mutex :name "kademlia-routing-table")))

(defun routing-table-insert! (table contact &key ping-fn)
  "Insert or refresh CONTACT (a live CONTACT instance, freshly received
off the wire) into TABLE, per the classic Kademlia bucket-update rule:
if CONTACT's node-id is already present in its bucket, it is moved to
the most-recently-seen (tail) position; else if that bucket has room
(fewer than +K+ entries), CONTACT is simply appended; else, if PING-FN
is supplied, it is called (with the bucket's current least-recently-
seen CONTACT) to decide the bucket's fate outside of TABLE's lock --
if PING-FN returns true (that old contact answered), the old contact is
refreshed to most-recently-seen and CONTACT is discarded (a full,
all-still-live bucket never evicts); if PING-FN returns false (or is
not supplied at all), the old contact is evicted and CONTACT takes its
place. Silently does nothing (returns NIL) if CONTACT's node-id is
TABLE's own SELF-ID -- a node is never its own routing-table entry.
Returns CONTACT on any successful insert/refresh."
  (let ((self-id (routing-table-self-id table)))
    (when (= (contact-node-id contact) self-id)
      (return-from routing-table-insert! nil))
    (let ((index (node-id-bucket-index self-id (contact-node-id contact)))
          (lru nil)
          (bucket-full? nil))
      (sb-thread:with-mutex ((routing-table-lock table))
        (let* ((bucket (aref (routing-table-buckets table) index))
               (existing (find (contact-node-id contact) bucket :key #'contact-node-id)))
          (cond
            (existing
             (setf (aref (routing-table-buckets table) index)
                   (append (remove existing bucket) (list contact)))
             (return-from routing-table-insert! contact))
            ((< (length bucket) +k+)
             (setf (aref (routing-table-buckets table) index) (append bucket (list contact)))
             (return-from routing-table-insert! contact))
            (t (setf bucket-full? t lru (first bucket))))))
      ;; Bucket was full: decide LRU's fate without holding the lock.
      (when bucket-full?
        (let ((lru-alive? (and ping-fn (funcall ping-fn lru))))
          (sb-thread:with-mutex ((routing-table-lock table))
            (let ((bucket (remove lru (aref (routing-table-buckets table) index) :count 1)))
              (setf (aref (routing-table-buckets table) index)
                    (append bucket (list (if lru-alive? lru contact))))))))
      contact)))

(defun routing-table-remove! (table node-id)
  "Remove the contact identified by NODE-ID from TABLE, if present.
Returns no useful value."
  (let ((index (node-id-bucket-index (routing-table-self-id table) node-id)))
    (sb-thread:with-mutex ((routing-table-lock table))
      (setf (aref (routing-table-buckets table) index)
            (remove node-id (aref (routing-table-buckets table) index) :key #'contact-node-id))))
  (values))

(defun routing-table-all-contacts (table)
  "Return a fresh list of every CONTACT currently known to TABLE,
across all buckets, in an unspecified order."
  (sb-thread:with-mutex ((routing-table-lock table))
    (loop for bucket across (routing-table-buckets table) append (copy-list bucket))))

(defun routing-table-closest-contacts (table target-id &optional (count +k+) exclude-id)
  "Return a fresh list of at most COUNT CONTACTs known to TABLE,
sorted by increasing XOR distance from TARGET-ID (closest first), the
core primitive both FIND-NODE RPC handling and the iterative lookup
(see node.lisp) are built from. EXCLUDE-ID, if supplied, omits any
contact with that node-id from the result (used to keep a lookup from
ever returning the querying node's own id back to itself)."
  (let ((all (routing-table-all-contacts table)))
    (when exclude-id
      (setf all (remove exclude-id all :key #'contact-node-id :test #'=)))
    (let ((sorted (sort all #'< :key (lambda (c) (node-id-distance target-id (contact-node-id c))))))
      (subseq sorted 0 (min count (length sorted))))))
