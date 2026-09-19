;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; Contacts: what a routing table actually stores. CONTACT is the
;;; lightweight, transient, hot-path runtime representation (never
;;; touches Git) that the routing table, the wire protocol, and RPC
;;; dispatch all pass around. PERSISTENT-CONTACT is its Git-persistable
;;; mirror -- a DEFINE-PERSISTENT-STRUCT, following this repository's
;;; existing convention (see examples/library.lisp's BOOK/LIBRARY) --
;;; used only by persistence.lisp when checkpointing/loading the routing
;;; table to/from a node's own local GitHack database.

(in-package "GITHACK-KADEMLIA")

(defstruct (contact
            (:constructor make-contact (node-id host port &optional (last-seen (get-universal-time))))
            (:predicate contact-p)
            (:conc-name contact/))
  "A single routing-table entry: NODE-ID is that peer's Kademlia
identity (see node-id.lisp), HOST/PORT its UDP contact address, and
LAST-SEEN the universal-time this contact was last confirmed live
(refreshed on every inbound datagram from it -- see node.lisp's
%HANDLE-DATAGRAM!). NODE-ID/HOST/PORT are immutable once a CONTACT is
constructed -- per this project's immutability convention, a slot is
:READ-ONLY unless it is genuinely intended to be mutated in place, and
none of these three ever legitimately change for a given live CONTACT
object; \"updating\" a contact instead means constructing (and
inserting) a new, otherwise-identical CONTACT with a fresher LAST-SEEN."
  (node-id 0 :type (integer 0 *) :read-only t)
  (host "" :type string :read-only t)
  (port 0 :type (integer 0 65535) :read-only t)
  (last-seen 0 :type (integer 0 *) :read-only t))

;;; PERSISTENT-CONTACT: the Git-persisted mirror of a CONTACT (above).
;;; NODE-ID is stored pre-rendered as its canonical hex-string (see
;;; NODE-ID->HEX-STRING) rather than as a raw bignum, both because it
;;; doubles as this contact's key in the routing-table catalog (see
;;; persistence.lisp) and because it keeps the persisted representation
;;; independent of any particular Lisp bignum's Git-blob encoding.
;;; DEFINE-PERSISTENT-STRUCT does not accept a struct-level docstring
;;; (unlike DEFSTRUCT), so this commentary lives here instead.
(define-persistent-struct persistent-contact
  (node-id "")
  (host "")
  (port 0)
  (last-seen 0))

(defun contact->persistent-contact (contact repository-pathname)
  "Return a fresh PERSISTENT-CONTACT mirroring CONTACT, rooted at
REPOSITORY-PATHNAME (the local Git repository/checkout a node
persists its routing table into -- see persistence.lisp), suitable
for storing as one entry of that node's routing-table catalog."
  (make-instance 'persistent-contact
                 :repository repository-pathname
                 :node-id (node-id->hex-string (contact/node-id contact))
                 :host (contact/host contact)
                 :port (contact/port contact)
                 :last-seen (contact/last-seen contact)))

(defun persistent-contact->contact (persistent-contact)
  "Return a fresh, transient CONTACT reconstructed from PERSISTENT-
CONTACT (a fully-resolved PERSISTENT-CONTACT instance, e.g. as
returned by DESERIALIZE-PERSISTENT-OBJECT) -- the inverse of
CONTACT->PERSISTENT-CONTACT."
  (make-contact (hex-string->node-id (persistent-contact-node-id persistent-contact))
                (persistent-contact-host persistent-contact)
                (persistent-contact-port persistent-contact)
                (persistent-contact-last-seen persistent-contact)))
