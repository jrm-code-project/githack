;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; Bridging a ROUTING-TABLE to a node's own local GitHack database.
;;; This is exactly what the user's requirement "GitHack nodes should
;;; use their own local GitHack database for node management" means in
;;; concrete terms: every KADEMLIA-NODE persists the *contents* of its
;;; in-memory routing table as ordinary PERSISTENT-CONTACT instances,
;;; keyed by hex node-id, in a PERSISTENT-HASH-TABLE catalog committed
;;; to a dedicated orphan branch of its own local Git repository -- the
;;; very same PHASH-*/WITH-TRANSACTION machinery
;;; examples/library.lisp's satirical library example uses for its book
;;; catalog, applied here to peer contacts instead of books.
;;;
;;; Persistence is always a full-table snapshot (never an incremental
;;; per-contact commit): a node's routing table changes on every single
;;; inbound datagram, and a Git commit per UDP packet would make the
;;; DHT's hot path pay for a filesystem write + tree/commit object
;;; creation on every PING -- far too slow. Instead, node.lisp calls
;;; PERSIST-ROUTING-TABLE! only periodically (a background flush
;;; thread, see its FLUSH-INTERVAL) and once more on a graceful
;;; STOP-KADEMLIA-NODE, so contacts survive process restarts without
;;; taxing the UDP hot path at all.

(in-package "GITHACK-KADEMLIA")

(defparameter +kademlia-branch+ "githack-kademlia"
  "The orphan branch every KADEMLIA-NODE reads its persisted routing
table from and writes it back to, within its own local Git repository
-- deliberately never \"main\" (or whatever branch that repository's
working copy happens to have checked out), so a node's DHT bookkeeping
can never tangle with, or be tangled by, any other history that
repository holds. The very first PERSIST-ROUTING-TABLE! call against a
fresh repository creates a genuine orphan root commit on this branch,
exactly as WITH-TRANSACTION's ORPHAN-COMMIT GUARANTEE promises (see
git-transaction.lisp).")

(defparameter +kademlia-signature+ "GitHack Kademlia Node <kademlia@githack.local>"
  "The AUTHOR/COMMITTER signature every PERSIST-ROUTING-TABLE! commit
is made under.")

(defun %resolve (value)
  "Return a live, fully-typed CLOS instance for VALUE, a raw value as
handed back by WITH-TRANSACTION's receiver argument (see
%RESOLVE-PERSISTENT-OBJECT in examples/library.lisp for the identical
pattern this is copied from) -- VALUE is NIL if +KADEMLIA-BRANCH+ has
never been written to yet, in which case NIL is returned unchanged."
  (and value (deserialize-persistent-object value)))

(defun load-routing-table! (table repository-pathname)
  "Populate TABLE (a ROUTING-TABLE, normally freshly constructed by
MAKE-ROUTING-TABLE) with every contact currently persisted on
REPOSITORY-PATHNAME's +KADEMLIA-BRANCH+, via a single :READ-ONLY
transaction -- never writes a commit, so this is always safe to call,
even concurrently with another process's PERSIST-ROUTING-TABLE!, and
does nothing at all if that branch has never been written to (a
node's very first run against a fresh repository). Contacts loaded
this way have their original LAST-SEEN timestamps preserved (they are
not re-stamped to \"now\"), so a long-idle contact reloaded from disk
is immediately eligible for LRU eviction by a fresher live one, rather
than masquerading as freshly confirmed."
  (with-repository (repository) (repository-pathname :mode :read-only)
    (with-transaction (value) (repository :read-only :branch +kademlia-branch+)
      (let ((catalog (%resolve value)))
        (when catalog
          (phash-map (lambda (hex-node-id raw-persistent-contact)
                       (declare (ignore hex-node-id))
                       (routing-table-insert!
                        table
                        (persistent-contact->contact (%resolve raw-persistent-contact))))
                     catalog))
        value))))

(defun persist-routing-table! (table repository-pathname)
  "Checkpoint every contact currently in TABLE (see ROUTING-TABLE-ALL-
CONTACTS) into a fresh PERSISTENT-HASH-TABLE catalog (keyed by each
contact's hex node-id), committed as REPOSITORY-PATHNAME's
+KADEMLIA-BRANCH+ head, in a single :READ-WRITE transaction with
:RETRY conflict resolution (safe to call concurrently with another
process/thread checkpointing the very same repository). This always
writes a wholesale replacement snapshot rather than incrementally
patching the previous catalog -- see this file's header commentary for
why -- so it is idempotent and never accumulates stale entries for
contacts TABLE itself has since evicted."
  (with-repository (repository) (repository-pathname :mode :read-write)
    (with-transaction (value) (repository :read-write
                                :branch +kademlia-branch+
                                :author +kademlia-signature+
                                :message "Checkpoint Kademlia routing table."
                                :conflict-resolution :retry)
      (declare (ignore value))
      (let ((catalog (phash-make :repository repository-pathname :test 'equal)))
        (dolist (contact (routing-table-all-contacts table))
          (setf catalog (phash-put (node-id->hex-string (contact/node-id contact))
                                   (contact->persistent-contact contact repository-pathname)
                                   catalog)))
        catalog))))
