;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; FiveAM test suite for GITHACK-KADEMLIA. Kept as its own separate
;;; ASDF system/test-package ("githack/kademlia-test", see
;;; githack.asd), independent of the main "githack/test" suite, since
;;; it is the only part of this codebase that opens real UDP sockets
;;; and spawns real background threads talking to each other over
;;; loopback -- consistent with this project's strong preference for
;;; genuine end-to-end tests over mocks (see end-to-end-tests.lisp,
;;; distributed-transaction-tests.lisp, githack-gc-tests.lisp), but
;;; kept decoupled so that a plain `git`-less/network-less CI run of
;;; the core object-database test suite is never made flaky by
;;; loopback-networking timing.

(defpackage "GITHACK-KADEMLIA-TEST"
  (:use "COMMON-LISP" "FIVEAM")
  (:import-from "ALEXANDRIA"
                "IOTA"
                "WITH-GENSYMS")
  (:import-from "GITHACK-KADEMLIA"
                "+ID-BITS+" "+K+" "+ALPHA+"
                "GENERATE-NODE-ID" "NODE-ID-DISTANCE" "NODE-ID-BUCKET-INDEX"
                "NODE-ID->HEX-STRING" "HEX-STRING->NODE-ID"
                "MAKE-CONTACT" "CONTACT/NODE-ID" "CONTACT/HOST" "CONTACT/PORT"
                "MAKE-ROUTING-TABLE" "ROUTING-TABLE-INSERT!" "ROUTING-TABLE-REMOVE!"
                "ROUTING-TABLE-ALL-CONTACTS" "ROUTING-TABLE-CLOSEST-CONTACTS"
                "LOAD-ROUTING-TABLE!" "PERSIST-ROUTING-TABLE!"
                "START-KADEMLIA-NODE" "STOP-KADEMLIA-NODE"
                "GET-NODE-ID" "GET-HOST" "GET-PORT" "GET-ROUTING-TABLE"
                "KADEMLIA-PING" "KADEMLIA-FIND-NODE" "KADEMLIA-JOIN!")
  (:export "KADEMLIA-SUITE" "RUN-KADEMLIA-TESTS"))

(in-package "GITHACK-KADEMLIA-TEST")

(def-suite kademlia-suite
  :description "Top-level suite for all GITHACK-KADEMLIA tests.")

(in-suite kademlia-suite)

(defun run-kademlia-tests ()
  "Run every GITHACK-KADEMLIA test and explain the results to
*STANDARD-OUTPUT*. Returns true iff every test passed."
  (run! 'kademlia-suite))

;;; --- A self-contained temporary bare-repository fixture -----------
;;; (independent of GITHACK-TEST's own WITH-TEMPORARY-GIT-REPOSITORY,
;;; since this test system deliberately does not depend on
;;; "githack/test" -- see this file's header commentary.)

(defun %unique-temp-repository-pathname (name-prefix)
  (merge-pathnames
   (format nil "~A~(~36,10,'0R~)/" name-prefix (random (expt 36 10) (make-random-state t)))
   (uiop:default-temporary-directory)))

(defmacro with-temporary-bare-repository ((repository-var) &body body)
  (with-gensyms (path)
    `(let ((,path (%unique-temp-repository-pathname "githack-kademlia-test-")))
       (ensure-directories-exist ,path)
       (uiop:run-program (list "git" "init" "--bare" (uiop:native-namestring ,path))
                          :output nil :error-output nil)
       (unwind-protect
            (let ((,repository-var ,path))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,path :validate t :if-does-not-exist :ignore))))))

(defmacro with-kademlia-node ((node-var &rest start-args) &body body)
  "Start a KADEMLIA-NODE (via START-KADEMLIA-NODE, given START-ARGS)
bound to NODE-VAR for the extent of BODY, unconditionally
STOP-KADEMLIA-NODE-ing it afterward."
  `(let ((,node-var (start-kademlia-node ,@start-args)))
     (unwind-protect (progn ,@body)
       (ignore-errors (stop-kademlia-node ,node-var)))))

;;; --- Node-id arithmetic ---------------------------------------------

(test node-id-distance-is-symmetric-and-zero-for-self
  (let ((a (generate-node-id)) (b (generate-node-id)))
    (is (= (node-id-distance a b) (node-id-distance b a)))
    (is (zerop (node-id-distance a a)))))

(test node-id-bucket-index-signals-for-identical-ids
  (let ((a (generate-node-id)))
    (signals error (node-id-bucket-index a a))))

(test node-id-bucket-index-matches-integer-length-of-distance
  (let ((self 0) (other #b1011))
    (is (= (node-id-bucket-index self other) 3))))

(test node-id-hex-string-round-trips
  (dotimes (i 20)
    (let ((id (generate-node-id)))
      (is (= id (hex-string->node-id (node-id->hex-string id)))))))

(test node-id-hex-string-has-fixed-width
  (is (= (length (node-id->hex-string 1)) (* 2 (/ +id-bits+ 8)))))

;;; --- Routing table (pure, in-memory) ---------------------------------

(test routing-table-insert-refuses-self
  (let* ((self-id (generate-node-id))
         (table (make-routing-table self-id)))
    (is (null (routing-table-insert! table (make-contact self-id "127.0.0.1" 1))))
    (is (null (routing-table-all-contacts table)))))

(test routing-table-insert-and-remove
  (let* ((self-id 0)
         (other-id 1)
         (table (make-routing-table self-id)))
    (routing-table-insert! table (make-contact other-id "127.0.0.1" 4000))
    (is (= (length (routing-table-all-contacts table)) 1))
    (routing-table-remove! table other-id)
    (is (null (routing-table-all-contacts table)))))

(test routing-table-insert-refreshes-existing-contact
  (let* ((self-id 0)
         (other-id 1)
         (table (make-routing-table self-id)))
    (routing-table-insert! table (make-contact other-id "127.0.0.1" 4000))
    (routing-table-insert! table (make-contact other-id "127.0.0.1" 5000))
    (let ((contacts (routing-table-all-contacts table)))
      (is (= (length contacts) 1))
      (is (= (contact/port (first contacts)) 5000)))))

(test routing-table-closest-contacts-sorted-by-xor-distance
  (let* ((self-id 0)
         (table (make-routing-table self-id)))
    (dolist (id '(1 2 4 8 16))
      (routing-table-insert! table (make-contact id "127.0.0.1" id)))
    (let ((closest (routing-table-closest-contacts table 3 3)))
      ;; distances from 3: 1->2, 2->1, 4->7, 8->11, 16->19
      (is (equal (mapcar #'contact/node-id closest) '(2 1 4))))))

(test routing-table-closest-contacts-excludes-given-id
  (let* ((self-id 0)
         (table (make-routing-table self-id)))
    (routing-table-insert! table (make-contact 1 "127.0.0.1" 1))
    (routing-table-insert! table (make-contact 2 "127.0.0.1" 2))
    (let ((closest (routing-table-closest-contacts table 1 10 1)))
      (is (equal (mapcar #'contact/node-id closest) '(2))))))

(test routing-table-bucket-eviction-keeps-live-lru
  (let* ((self-id 0)
         (base (ash 1 20))
         (table (make-routing-table self-id)))
    ;; All ids in [2^20, 2^21) share bucket index 20 (distance = id
    ;; itself, since self-id is 0, and integer-length is 21 throughout
    ;; that range); it comfortably holds more than +K+ distinct ids.
    (mapc (lambda (id) (routing-table-insert! table (make-contact id "127.0.0.1" 0))) (iota +k+ :start base))
    (is (= (length (routing-table-all-contacts table)) +k+))
    ;; Bucket is now full; PING-FN says the LRU is still alive, so the
    ;; newcomer must be discarded and every original id retained.
    (routing-table-insert! table (make-contact (+ base +k+) "127.0.0.1" 0) :ping-fn (constantly t))
    (is (= (length (routing-table-all-contacts table)) +k+))
    (is (null (find (+ base +k+) (routing-table-all-contacts table) :key #'contact/node-id)))
    ;; Now the LRU (BASE was refreshed to most-recently-seen by the
    ;; previous "alive" ping, so BASE+1 is the new LRU) is reported
    ;; dead: the newcomer must be admitted, and exactly BASE+1 gone.
    (routing-table-insert! table (make-contact (+ base +k+ 1) "127.0.0.1" 0) :ping-fn (constantly nil))
    (is (= (length (routing-table-all-contacts table)) +k+))
    (is (find base (routing-table-all-contacts table) :key #'contact/node-id))
    (is (null (find (1+ base) (routing-table-all-contacts table) :key #'contact/node-id)))
    (is (find (+ base +k+ 1) (routing-table-all-contacts table) :key #'contact/node-id))))

;;; --- Persistence round-trip (real Git, no network) -------------------

(test persist-and-load-routing-table-round-trips
  (with-temporary-bare-repository (repository)
    (let* ((self-id 0)
           (table (make-routing-table self-id)))
      (routing-table-insert! table (make-contact 1 "127.0.0.1" 4001))
      (routing-table-insert! table (make-contact 2 "127.0.0.1" 4002))
      (persist-routing-table! table repository)
      (let ((reloaded (make-routing-table self-id)))
        (load-routing-table! reloaded repository)
        (let ((ids (sort (mapcar #'contact/node-id (routing-table-all-contacts reloaded)) #'<)))
          (is (equal ids '(1 2))))))))

(test load-routing-table-on-fresh-repository-is-a-no-op
  (with-temporary-bare-repository (repository)
    (let ((table (make-routing-table 0)))
      (load-routing-table! table repository)
      (is (null (routing-table-all-contacts table))))))

;;; --- Real, live, loopback-networking integration tests --------------

(test two-nodes-ping-each-other
  (with-temporary-bare-repository (repo-a)
    (with-temporary-bare-repository (repo-b)
      (with-kademlia-node (node-a :repository-pathname repo-a :flush-interval nil)
        (with-kademlia-node (node-b :repository-pathname repo-b :flush-interval nil)
          (is (kademlia-ping node-a (get-host node-b) (get-port node-b)))
          (is (kademlia-ping node-b (get-host node-a) (get-port node-a)))
          ;; Each node's routing table must now know about the other,
          ;; learned purely as a side effect of PING/PONG handling.
          (is (find (get-node-id node-b) (routing-table-all-contacts (get-routing-table node-a)) :key #'contact/node-id))
          (is (find (get-node-id node-a) (routing-table-all-contacts (get-routing-table node-b)) :key #'contact/node-id)))))))

(test ping-a-silent-port-times-out
  (with-temporary-bare-repository (repo-a)
    (with-kademlia-node (node-a :repository-pathname repo-a :flush-interval nil)
      ;; Port 1 is (overwhelmingly likely to be) not a live Kademlia
      ;; listener; a PING to it must time out rather than hang or error.
      (is (null (kademlia-ping node-a "127.0.0.1" 1 :timeout 1))))))

(test kademlia-join-and-transitive-find-node-discovery
  (with-temporary-bare-repository (repo-a)
    (with-temporary-bare-repository (repo-b)
      (with-temporary-bare-repository (repo-c)
        (with-kademlia-node (node-a :repository-pathname repo-a :flush-interval nil)
          (with-kademlia-node (node-b :repository-pathname repo-b :flush-interval nil)
            (with-kademlia-node (node-c :repository-pathname repo-c :flush-interval nil)
              ;; B joins through A; C joins through A. Neither B nor C
              ;; ever contacts the other directly -- any knowledge C has
              ;; of B must have propagated transitively through A's own
              ;; FIND-NODE-REPLY contact lists.
              (is (kademlia-join! node-b (get-host node-a) (get-port node-a)))
              (is (kademlia-join! node-c (get-host node-a) (get-port node-a)))
              (let ((discovered (kademlia-find-node node-c (get-node-id node-b))))
                (is (find (get-node-id node-b) discovered :key #'contact/node-id))))))))))

(test kademlia-join-against-a-dead-bootstrap-fails-cleanly
  (with-temporary-bare-repository (repo-a)
    (with-kademlia-node (node-a :repository-pathname repo-a :flush-interval nil)
      (is (null (kademlia-join! node-a "127.0.0.1" 1 :timeout 1))))))

(test stopping-a-node-persists-its-routing-table
  (with-temporary-bare-repository (repo-a)
    (with-temporary-bare-repository (repo-b)
      (let ((node-b-id nil))
        (with-kademlia-node (node-a :repository-pathname repo-a :flush-interval nil)
          (let ((node-b (start-kademlia-node :repository-pathname repo-b :flush-interval nil)))
            (setf node-b-id (get-node-id node-b))
            (is (kademlia-ping node-a (get-host node-b) (get-port node-b)))
            ;; Stopping node-a must checkpoint its routing table (which
            ;; now knows about node-b) even though periodic flushing was
            ;; disabled for this test.
            (stop-kademlia-node node-a)))
        (let ((reloaded (make-routing-table (generate-node-id))))
          (load-routing-table! reloaded repo-a)
          (is (find node-b-id (routing-table-all-contacts reloaded) :key #'contact/node-id)))))))
