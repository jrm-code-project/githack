;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; The Kademlia wire protocol: message encode/decode. Messages are
;;; plain Lisp plists, printed/read as text -- deliberately the same
;;; "PRIN1/READ with *READ-EVAL* bound to NIL" discipline already
;;; established in distributed-transaction.lisp for parsing transaction
;;; manifests off of (also untrusted, in the multi-repository case) Git
;;; tag bodies: never blindly EVAL untrusted input, but plain data
;;; (keywords, strings, integers, lists) round-trips through PRIN1/READ
;;; perfectly well and needs no bespoke parser. Only two RPCs exist --
;;; PING/PONG (liveness) and FIND-NODE/FIND-NODE-REPLY (routing-table
;;; lookup) -- since this DHT exists purely for node *discovery*, not
;;; for storing or replicating arbitrary values (no STORE/FIND-VALUE).

(in-package "GITHACK-KADEMLIA")

(defun make-rpc-id ()
  "Return a fresh, effectively-unique RPC correlation id: a random
62-bit non-negative integer, used to match an outstanding request
(PING or FIND-NODE) to whichever inbound datagram is its reply, even
though several such requests may be in flight concurrently over the
same UDP socket (see node.lisp's %PENDING-REQUESTS table)."
  (random (ash 1 62) (make-random-state t)))

(defun %contact-triple (contact)
  "Return CONTACT's over-the-wire representation: a 3-element list of
its NODE-ID (as a hex-string, see NODE-ID->HEX-STRING), HOST, and
PORT. LAST-SEEN is deliberately not transmitted -- it is purely local
bookkeeping for the receiving node's own routing table, not a property
of the contact itself."
  (list (node-id->hex-string (contact-node-id contact)) (contact-host contact) (contact-port contact)))

(defun %triple->contact (triple)
  "Inverse of %CONTACT-TRIPLE: reconstruct a fresh CONTACT (with
LAST-SEEN defaulting to now) from a 3-element (HEX-NODE-ID HOST PORT)
list as received over the wire."
  (destructuring-bind (hex-node-id host port) triple
    (make-contact (hex-string->node-id hex-node-id) host port)))

(defun %encode-message (message-plist)
  "Render MESSAGE-PLIST (a plist of keywords, strings, integers, and
lists thereof -- see the ENCODE-* functions below) as a single line of
text suitable for one UDP datagram payload, via ordinary PRIN1."
  (let ((*print-readably* nil)
        (*print-circle* nil)
        (*print-pretty* nil))
    (prin1-to-string message-plist)))

(defun %decode-message (string)
  "Parse STRING (one UDP datagram payload, as produced by %ENCODE-
MESSAGE) back into a message plist, via READ with *READ-EVAL* bound to
NIL -- STRING arrives over the network from a peer that must never be
trusted enough to make this Lisp image evaluate arbitrary code merely
by receiving a packet from it."
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun encode-ping (rpc-id sender)
  "Encode a PING message: RPC-ID (see MAKE-RPC-ID) and SENDER (the
sending node's own CONTACT, so the recipient's PONG handler -- and
indeed any recipient at all, since every inbound datagram feeds
ROUTING-TABLE-INSERT! -- learns of SENDER)."
  (%encode-message (list :type :ping :rpc-id rpc-id :sender (%contact-triple sender))))

(defun encode-pong (rpc-id sender)
  "Encode a PONG message, replying to the PING whose RPC-ID this is."
  (%encode-message (list :type :pong :rpc-id rpc-id :sender (%contact-triple sender))))

(defun encode-find-node (rpc-id sender target-id)
  "Encode a FIND-NODE message: a request for SENDER's closest known
contacts to TARGET-ID (a NODE-ID)."
  (%encode-message (list :type :find-node :rpc-id rpc-id :sender (%contact-triple sender)
                         :target (node-id->hex-string target-id))))

(defun encode-find-node-reply (rpc-id sender contacts)
  "Encode a FIND-NODE-REPLY message: CONTACTS (a list of CONTACT
instances, at most +K+ of them) offered in reply to the FIND-NODE
whose RPC-ID this is."
  (%encode-message (list :type :find-node-reply :rpc-id rpc-id :sender (%contact-triple sender)
                         :contacts (mapcar #'%contact-triple contacts))))
