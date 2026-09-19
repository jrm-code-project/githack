;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; GITHACK-KADEMLIA: a Kademlia distributed hash table used purely for
;;; *node discovery* between remote GitHack nodes -- finding out which
;;; hosts/ports are running a GitHack node at all, not for storing or
;;; replicating arbitrary application data (there is no STORE/FIND-VALUE
;;; RPC; only PING and FIND-NODE, the minimal pair Kademlia needs for
;;; peer discovery via iterative lookups). Every node keeps its routing
;;; table (the set of known peer contacts, bucketed by XOR distance) in
;;; its OWN local GitHack-managed Git repository, on a dedicated orphan
;;; branch, exactly as GITHACK-EXAMPLE-LIBRARY keeps its catalog on
;;; "database-example" -- see persistence.lisp. This is what "GitHack
;;; nodes should use their own local GitHack database for node
;;; management" means: the DHT dogfoods GitHack's own persistence layer
;;; instead of an ad hoc in-memory-only or third-party store.
;;;
;;; This package is a separate, optional ASDF system ("githack/kademlia",
;;; see githack.asd) layered cleanly on top of the core "githack" system
;;; -- exactly the way "githack/example" is -- so that using GitHack as a
;;; plain persistent object database never requires sockets, threads, or
;;; any DHT machinery to be loaded at all.

(defpackage "GITHACK-KADEMLIA"
  (:use "COMMON-LISP")
  (:import-from "GITHACK"
                "DEFINE-PERSISTENT-STRUCT"
                "PHASH-MAKE"
                "PHASH-GET"
                "PHASH-PUT"
                "PHASH-REMOVE"
                "PHASH-MAP"
                "DESERIALIZE-PERSISTENT-OBJECT"
                "WITH-REPOSITORY"
                "WITH-TRANSACTION")
  (:export ;; node-id.lisp
           "+ID-BITS+" "+ID-BYTES+" "+K+" "+ALPHA+"
           "GENERATE-NODE-ID" "NODE-ID-DISTANCE" "NODE-ID-BUCKET-INDEX"
           "NODE-ID->HEX-STRING" "HEX-STRING->NODE-ID"
           ;; contact.lisp
           "CONTACT" "MAKE-CONTACT" "CONTACT-P"
           "CONTACT/NODE-ID" "CONTACT/HOST" "CONTACT/PORT" "CONTACT/LAST-SEEN"
           "PERSISTENT-CONTACT"
           "PERSISTENT-CONTACT-NODE-ID" "PERSISTENT-CONTACT-HOST"
           "PERSISTENT-CONTACT-PORT" "PERSISTENT-CONTACT-LAST-SEEN"
           "CONTACT->PERSISTENT-CONTACT" "PERSISTENT-CONTACT->CONTACT"
           ;; routing-table.lisp
           "ROUTING-TABLE" "MAKE-ROUTING-TABLE" "ROUTING-TABLE-P"
           "ROUTING-TABLE/SELF-ID"
           "ROUTING-TABLE-INSERT!" "ROUTING-TABLE-REMOVE!"
           "ROUTING-TABLE-ALL-CONTACTS" "ROUTING-TABLE-CLOSEST-CONTACTS"
           ;; persistence.lisp
           "+KADEMLIA-BRANCH+"
           "LOAD-ROUTING-TABLE!" "PERSIST-ROUTING-TABLE!"
           ;; node.lisp
           "KADEMLIA-NODE" "KADEMLIA-NODE-P"
           "GET-NODE-ID" "GET-HOST" "GET-PORT"
           "GET-ROUTING-TABLE" "GET-REPOSITORY-PATHNAME" "GET-FLUSH-INTERVAL"
           "START-KADEMLIA-NODE" "STOP-KADEMLIA-NODE"
           "KADEMLIA-PING" "KADEMLIA-FIND-NODE" "KADEMLIA-JOIN!"))
