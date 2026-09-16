;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; Node-id arithmetic: 160-bit identifiers, the XOR distance metric, and
;;; the bucket-index computation every k-bucket routing table is built
;;; from. Kept dependency-free (no CLOS, no persistence) so it can be
;;; unit-tested in complete isolation from sockets and Git.

(in-package "GITHACK-KADEMLIA")

(defconstant +id-bits+ 160
  "The width, in bits, of a Kademlia NODE-ID -- 160 bits, matching the
original Kademlia paper's SHA-1-sized identifier space (chosen here
purely for its familiar size; GitHack does not derive node-ids from
SHA-1 of anything in particular, see GENERATE-NODE-ID).")

(defconstant +id-bytes+ (/ +id-bits+ 8)
  "+ID-BITS+ expressed in bytes (20), i.e. the width of a NODE-ID's
canonical hex-string encoding is (* 2 +ID-BYTES+) characters.")

(defconstant +k+ 20
  "The classic Kademlia bucket size K: the maximum number of contacts
kept in any single k-bucket, and the number of closest contacts an
iterative FIND-NODE lookup converges on.")

(defconstant +alpha+ 3
  "The classic Kademlia concurrency parameter ALPHA: the number of
not-yet-queried contacts an iterative FIND-NODE lookup queries per
round (see KADEMLIA-FIND-NODE in node.lisp).")

(defun generate-node-id ()
  "Return a fresh NODE-ID: a uniformly random integer in [0, 2^+ID-
BITS+), drawn from a freshly OS-entropy-seeded random state (SBCL's
(MAKE-RANDOM-STATE T) reads from the operating system's own entropy
source), so that concurrently-started nodes on different hosts (or
even the same host) get independent, effectively-unique ids without
any coordination. This is not a cryptographic identity -- there is no
proof of work or public-key binding -- it is exactly as trustworthy as
classic Kademlia's own self-assigned random ids."
  (random (ash 1 +id-bits+) (make-random-state t)))

(defun node-id-distance (a b)
  "The Kademlia XOR distance between NODE-IDs A and B -- symmetric,
zero exactly when A = B, and satisfying the (non-Euclidean, but still
useful) triangle inequality XOR relies on."
  (logxor a b))

(defun node-id-bucket-index (self-id other-id)
  "Return the 0-based k-bucket index, in [0, +ID-BITS+), that a contact
whose id is OTHER-ID falls into within a routing table rooted at SELF-
ID: bucket I holds every contact whose XOR distance from SELF-ID lies
in [2^I, 2^(I+1)) -- equivalently, I is the position (counting from
zero at the least-significant bit) of the highest set bit of the XOR
distance. Signals an error for OTHER-ID = SELF-ID: a node is never its
own routing-table entry, so there is no such bucket."
  (let ((distance (node-id-distance self-id other-id)))
    (when (zerop distance)
      (error "NODE-ID-BUCKET-INDEX: SELF-ID and OTHER-ID are identical (~D); a node has no bucket index relative to itself." self-id))
    (1- (integer-length distance))))

(defun node-id->hex-string (id)
  "Render NODE-ID ID as its canonical, fixed-width (2 * +ID-BYTES+
character), zero-padded uppercase hex-string encoding -- the form
NODE-IDs take on the wire (see protocol.lisp) and as PERSISTENT-
CONTACT keys/slots (see contact.lisp), since raw bignums are not a
convenient Git-tree-entry-name or catalog key."
  (format nil "~V,'0X" (* 2 +id-bytes+) id))

(defun hex-string->node-id (string)
  "Inverse of NODE-ID->HEX-STRING: parse STRING (a hex-string as
produced by NODE-ID->HEX-STRING, or any valid hex encoding of a
non-negative integer) back into a NODE-ID."
  (parse-integer string :radix 16))
