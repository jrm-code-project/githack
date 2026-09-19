;;; -*- Mode: Lisp; coding: utf-8; -*-

;;; KADEMLIA-NODE: the live, in-process runtime object tying a
;;; ROUTING-TABLE, a UDP socket, a listener thread, and a periodic
;;; persistence-flush thread together into the actual DHT participant.
;;; START-KADEMLIA-NODE/STOP-KADEMLIA-NODE bracket a node's lifetime;
;;; KADEMLIA-PING, KADEMLIA-FIND-NODE, and KADEMLIA-JOIN! are the public
;;; discovery operations built on top of it.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(in-package "GITHACK-KADEMLIA")

;;; A single outstanding RPC awaiting its reply: a semaphore the
;;; issuing thread blocks on, and a box the listener thread deposits
;;; the decoded reply payload into before signalling that semaphore.
(defstruct (%pending-request (:constructor %make-pending-request ())
                              (:conc-name %pending-request/))
  (semaphore (sb-thread:make-semaphore :count 0) :read-only t)
  (reply nil))

(defclass kademlia-node ()
  ((node-id :reader get-node-id :initarg :node-id
            :documentation "This node's own 160-bit NODE-ID.")
   (host :reader get-host :initarg :host
         :documentation "The local IP address (a string) this node's UDP socket is bound to.")
   (port :reader get-port :initarg :port
         :documentation "The local UDP port this node's socket is bound to (always concrete -- see START-KADEMLIA-NODE's :PORT 0 ephemeral-port handling -- never 0 once a node has started).")
   (socket :accessor %get-socket :initarg :socket
           :documentation "The underlying SB-BSD-SOCKETS:INET-SOCKET (:TYPE :DATAGRAM) this node sends/receives on.")
   (routing-table :reader get-routing-table :initarg :routing-table
                  :documentation "This node's in-memory ROUTING-TABLE.")
   (repository-pathname :reader get-repository-pathname :initarg :repository-pathname
                         :documentation "The local Git repository this node's routing table is persisted into -- see persistence.lisp.")
   (flush-interval :reader get-flush-interval :initarg :flush-interval :initform 30
                   :documentation "Seconds between periodic PERSIST-ROUTING-TABLE! checkpoints, or NIL to disable periodic flushing entirely (STOP-KADEMLIA-NODE still flushes once, unconditionally).")
   (running-p :accessor %running-p :initform nil
              :documentation "True from START-KADEMLIA-NODE until STOP-KADEMLIA-NODE; both background threads poll this to know when to exit.")
   (listener-thread :accessor %listener-thread :initform nil)
   (flush-thread :accessor %flush-thread :initform nil)
   (pending-lock :reader %pending-lock :initform (sb-thread:make-mutex :name "kademlia-pending-requests"))
   (pending-requests :reader %pending-requests :initform (make-hash-table)
                      :documentation "RPC-ID -> %PENDING-REQUEST, for every outstanding PING/FIND-NODE this node has sent and not yet gotten a reply (or timed out) for."))
  (:documentation
   "A single, live Kademlia DHT participant: one UDP socket, one
ROUTING-TABLE, one listener thread dispatching inbound PING/PONG and
FIND-NODE/FIND-NODE-REPLY datagrams (see %HANDLE-DATAGRAM!), and one
periodic flush thread checkpointing the routing table into this
node's own local GitHack database (see persistence.lisp). Always
constructed via START-KADEMLIA-NODE, never MAKE-INSTANCE directly."))

(defun kademlia-node-p (object) (typep object 'kademlia-node))

(defun %self-contact (node)
  "Return NODE's own current CONTACT (its node-id/host/port), as
advertised to peers in every outgoing PING/FIND-NODE/FIND-NODE-REPLY."
  (make-contact (get-node-id node) (get-host node) (get-port node)))

(defun %octets->dotted-string (octets)
  "Render OCTETS (a 4-element vector of (UNSIGNED-BYTE 8), as
SB-BSD-SOCKETS:SOCKET-RECEIVE returns for the sender's address on an
INET datagram socket) as a dotted-quad IPv4 string."
  (format nil "~D.~D.~D.~D" (aref octets 0) (aref octets 1) (aref octets 2) (aref octets 3)))

(defun %resolve-address (host)
  "Return an address suitable for SB-BSD-SOCKETS:SOCKET-SEND's
:ADDRESS argument for HOST: if HOST is already an address vector (as
%HANDLE-DATAGRAM! receives directly from SOCKET-RECEIVE), it is
returned as-is; if HOST is a string (a dotted-quad or hostname, as
every public API function in this file accepts), it is resolved via
SB-BSD-SOCKETS:MAKE-INET-ADDRESS."
  (if (stringp host) (sb-bsd-sockets:make-inet-address host) host))

(defun %send-string! (node string host port)
  "Send STRING as a single UDP datagram from NODE's socket to
HOST:PORT (HOST per %RESOLVE-ADDRESS)."
  (sb-bsd-sockets:socket-send (%get-socket node) string nil :address (list (%resolve-address host) port))
  (values))

;;; --- Pending-RPC bookkeeping -------------------------------------

(defun %register-pending! (node rpc-id)
  (let ((pending (%make-pending-request)))
    (sb-thread:with-mutex ((%pending-lock node))
      (setf (gethash rpc-id (%pending-requests node)) pending))
    pending))

(defun %forget-pending! (node rpc-id)
  (sb-thread:with-mutex ((%pending-lock node))
    (remhash rpc-id (%pending-requests node))))

(defun %complete-pending! (node rpc-id reply)
  "If RPC-ID is still an outstanding request NODE is awaiting a reply
for, deposit REPLY (a decoded message plist) into it and wake up
whichever thread is waiting in %AWAIT-PENDING!. Does nothing if RPC-ID
is unknown (already timed out, already completed, or was never this
node's own request -- e.g. a duplicate or spoofed reply)."
  (let ((pending (sb-thread:with-mutex ((%pending-lock node))
                   (gethash rpc-id (%pending-requests node)))))
    (when pending
      (setf (%pending-request/reply pending) reply)
      (sb-thread:signal-semaphore (%pending-request/semaphore pending)))))

(defun %await-pending! (node rpc-id timeout)
  "Register RPC-ID as an outstanding request, block up to TIMEOUT
seconds for %COMPLETE-PENDING! to be called for it, and return (VALUES
REPLY T) if it was, or (VALUES NIL NIL) on timeout. Always
unregisters RPC-ID before returning, whichever way."
  (let ((pending (%register-pending! node rpc-id)))
    (unwind-protect
        (if (sb-thread:wait-on-semaphore (%pending-request/semaphore pending) :timeout timeout)
            (values (%pending-request/reply pending) t)
            (values nil nil))
      (%forget-pending! node rpc-id))))

;;; --- Inbound datagram dispatch ------------------------------------

(defun %handle-datagram! (node payload source-address source-port)
  "Decode PAYLOAD (one inbound UDP datagram's contents) and dispatch
it: every message, of any type, first causes its claimed sender to be
inserted/refreshed in NODE's own routing table (using the UDP
packet's *observed* SOURCE-ADDRESS/SOURCE-PORT as that contact's
address, never whatever the message body itself might claim, so a
peer can never register a spoofed or NATted-wrong contact address for
itself merely by lying in its own message body); then PING is
answered with PONG, FIND-NODE is answered with FIND-NODE-REPLY (NODE's
own +K+ closest known contacts to the requested target, excluding
NODE itself), and PONG/FIND-NODE-REPLY are handed to %COMPLETE-
PENDING! to wake up whichever local thread is awaiting that RPC-ID.
Any malformed PAYLOAD (a decode error, or one missing expected plist
keys) is caught and ignored -- a malformed or hostile packet from one
peer must never take a node's listener thread down."
  (handler-case
      (let* ((message (%decode-message payload))
             (type (getf message :type))
             (rpc-id (getf message :rpc-id))
             (sender-triple (getf message :sender))
             (sender-id (hex-string->node-id (first sender-triple)))
             (observed-host (%octets->dotted-string source-address)))
        (unless (= sender-id (get-node-id node))
          (routing-table-insert!
           (get-routing-table node)
           (make-contact sender-id observed-host source-port)
           :ping-fn (lambda (contact) (kademlia-ping node (contact/host contact) (contact/port contact)))))
        (ecase type
          (:ping
           (%send-string! node (encode-pong rpc-id (%self-contact node)) source-address source-port))
          (:pong
           (%complete-pending! node rpc-id (list :type :pong)))
          (:find-node
           (let* ((target-id (hex-string->node-id (getf message :target)))
                  (closest (routing-table-closest-contacts (get-routing-table node) target-id +k+ (get-node-id node))))
             (%send-string! node (encode-find-node-reply rpc-id (%self-contact node) closest) source-address source-port)))
          (:find-node-reply
           (%complete-pending! node rpc-id (list :type :find-node-reply
                                                  :contacts (mapcar #'%triple->contact (getf message :contacts)))))))
    (error (condition)
      (declare (ignorable condition))
      (values))))

(defun %listener-loop (node)
  "NODE's listener thread body: repeatedly receive one UDP datagram
and dispatch it via %HANDLE-DATAGRAM!, until %RUNNING-P NODE is false
(set by STOP-KADEMLIA-NODE, which also closes the socket to unblock
whatever SOCKET-RECEIVE call is currently pending)."
  (let next ()
    (when (%running-p node)
      (handler-case
          (multiple-value-bind (buffer length address port) (sb-bsd-sockets:socket-receive (%get-socket node) nil 65507)
            ;; A socket closed (by STOP-KADEMLIA-NODE) while this thread
            ;; is blocked inside SOCKET-RECEIVE can, on some platforms,
            ;; unblock with a bogus out-of-range LENGTH rather than
            ;; signaling an error -- harmless, and only ever seen during
            ;; shutdown, so it is silently skipped rather than warned
            ;; about.
            (when (and (%running-p node) (<= 0 length (length buffer)))
              (%handle-datagram! node (subseq buffer 0 length) address port)))
        (error (condition)
          (when (%running-p node)
            (warn "GITHACK-KADEMLIA: listener error on node ~A:~A: ~A" (get-host node) (get-port node) condition))))
      (next))))

(defun %flush-loop (node)
  "NODE's periodic-persistence thread body: sleep GET-FLUSH-INTERVAL
seconds, then PERSIST-ROUTING-TABLE!, repeating until %RUNNING-P NODE
is false. A failed checkpoint (e.g. a transient Git error) is logged
via WARN and otherwise ignored -- it will simply be retried on the
next interval, and STOP-KADEMLIA-NODE always attempts one final
checkpoint regardless."
  (let next ()
    (when (%running-p node)
      (sleep (get-flush-interval node))
      (when (%running-p node)
        (handler-case (persist-routing-table! (get-routing-table node) (get-repository-pathname node))
          (error (condition)
            (warn "GITHACK-KADEMLIA: periodic routing-table checkpoint failed for node ~A:~A: ~A"
                  (get-host node) (get-port node) condition))))
      (next))))

;;; --- Lifecycle ------------------------------------------------------

(defun start-kademlia-node (&key (host "127.0.0.1") (port 0) repository-pathname node-id (flush-interval 30))
  "Start and return a new, live KADEMLIA-NODE bound to HOST:PORT (PORT
0, the default, asks the OS for an ephemeral port -- see GET-PORT on
the returned node for the actual port chosen), identified by NODE-ID
(a fresh GENERATE-NODE-ID if not supplied), whose routing table is
loaded from (see LOAD-ROUTING-TABLE!) and periodically checkpointed
to (see PERSIST-ROUTING-TABLE!, every FLUSH-INTERVAL seconds, or never
if FLUSH-INTERVAL is NIL) its own local Git repository at REPOSITORY-
PATHNAME (required -- this is the node's \"own local GitHack database
for node management\"). Spawns a background listener thread and, if
FLUSH-INTERVAL is non-NIL, a background flush thread; both are
genuine SB-THREAD threads, stopped together by STOP-KADEMLIA-NODE."
  (unless repository-pathname
    (error "START-KADEMLIA-NODE: :REPOSITORY-PATHNAME is required (a node's routing table is always persisted to its own local GitHack database)."))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
         (node-id (or node-id (generate-node-id))))
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address host) port)
    (let* ((actual-port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
           (table (make-routing-table node-id))
           (node (make-instance 'kademlia-node
                                :node-id node-id :host host :port actual-port :socket socket
                                :routing-table table :repository-pathname repository-pathname
                                :flush-interval flush-interval)))
      (load-routing-table! table repository-pathname)
      (setf (%running-p node) t)
      (setf (%listener-thread node)
            (sb-thread:make-thread (lambda () (%listener-loop node))
                                   :name (format nil "kademlia-listener-~A:~A" host actual-port)))
      (when flush-interval
        (setf (%flush-thread node)
              (sb-thread:make-thread (lambda () (%flush-loop node))
                                     :name (format nil "kademlia-flush-~A:~A" host actual-port))))
      node)))

(defun stop-kademlia-node (node)
  "Stop NODE: signal both background threads to exit, close its
socket (unblocking a listener thread currently parked in
SOCKET-RECEIVE), join both threads, and persist one final routing-
table checkpoint unconditionally (regardless of FLUSH-INTERVAL) so no
contact learned since the last periodic flush is lost. Safe to call
more than once. Returns no useful value."
  (setf (%running-p node) nil)
  (ignore-errors (sb-bsd-sockets:socket-close (%get-socket node)))
  (when (%listener-thread node)
    (ignore-errors (sb-thread:join-thread (%listener-thread node) :timeout 5)))
  (when (%flush-thread node)
    (ignore-errors (sb-thread:join-thread (%flush-thread node) :timeout 5)))
  (persist-routing-table! (get-routing-table node) (get-repository-pathname node))
  (values))

;;; --- Discovery operations --------------------------------------------

(defun kademlia-ping (node host port &key (timeout 2))
  "Send a PING to HOST:PORT from NODE and block up to TIMEOUT seconds
for a PONG. Returns true if one arrived in time, NIL on timeout (or
if HOST:PORT is unreachable/not a Kademlia node at all -- a timeout is
the only failure signal this protocol has)."
  (let ((rpc-id (make-rpc-id)))
    (%send-string! node (encode-ping rpc-id (%self-contact node)) host port)
    (nth-value 1 (%await-pending! node rpc-id timeout))))

(defun %find-node-rpc (node contact target-id &key (timeout 2))
  "Send a single FIND-NODE RPC to CONTACT asking for its closest known
contacts to TARGET-ID, and block up to TIMEOUT seconds for the reply.
Returns a fresh list of CONTACT instances (possibly empty) on a timely
reply, or NIL on timeout."
  (let ((rpc-id (make-rpc-id)))
    (%send-string! node (encode-find-node rpc-id (%self-contact node) target-id) (contact/host contact) (contact/port contact))
    (multiple-value-bind (reply found?) (%await-pending! node rpc-id timeout)
      (when found? (getf reply :contacts)))))

(defun kademlia-find-node (node target-id &key (alpha +alpha+) (k +k+) (timeout 2) (max-rounds 20))
  "Perform a classic iterative Kademlia FIND-NODE lookup for TARGET-ID
(a NODE-ID), starting from NODE's own current routing table, and
return NODE's +K+ (or K) closest known contacts to TARGET-ID once the
lookup converges. Each round queries up to ALPHA not-yet-queried
contacts from the current shortlist (sequentially, not in parallel --
a deliberately simple, easily-testable implementation choice over the
classic concurrent-ALPHA variant); every FIND-NODE-REPLY's offered
contacts are folded straight into NODE's own routing table (via
ROUTING-TABLE-INSERT!), so the shortlist for the next round is simply
re-derived from the (now possibly larger) routing table. Terminates
early, after at most MAX-ROUNDS rounds, once a round makes no further
progress (every queried contact in that round timed out) or every
known contact has already been queried."
  (let ((queried (make-hash-table))
        (shortlist (routing-table-closest-contacts (get-routing-table node) target-id k (get-node-id node))))
    (dotimes (round max-rounds)
      (declare (ignorable round))
      (let ((candidates (remove-if (lambda (c) (gethash (contact/node-id c) queried)) shortlist)))
        (setf candidates (subseq candidates 0 (min alpha (length candidates))))
        (when (null candidates) (return))
        (let ((progress? nil))
          (dolist (contact candidates)
            (setf (gethash (contact/node-id contact) queried) t)
            (let ((discovered (%find-node-rpc node contact target-id :timeout timeout)))
              (when discovered
                (setf progress? t)
                (dolist (rc discovered)
                  (unless (= (contact/node-id rc) (get-node-id node))
                    (routing-table-insert! (get-routing-table node) rc))))))
          (setf shortlist (routing-table-closest-contacts (get-routing-table node) target-id k (get-node-id node)))
          (unless progress? (return)))))
    (routing-table-closest-contacts (get-routing-table node) target-id k (get-node-id node))))

(defun kademlia-join! (node bootstrap-host bootstrap-port &key (timeout 2))
  "Bootstrap NODE's routing table by contacting a single already-known
peer at BOOTSTRAP-HOST:BOOTSTRAP-PORT: PING it (which, on a timely
PONG, has already caused %HANDLE-DATAGRAM! to insert it into NODE's
routing table as a side effect of processing the reply), then perform
an iterative KADEMLIA-FIND-NODE lookup for NODE's own NODE-ID -- the
standard Kademlia join procedure, populating NODE's routing table with
every contact discoverable transitively through the bootstrap peer.
Returns NIL (having changed nothing) if the bootstrap peer never
replies to the initial PING at all; returns T otherwise."
  (let ((rpc-id (make-rpc-id)))
    (%send-string! node (encode-ping rpc-id (%self-contact node)) bootstrap-host bootstrap-port)
    (multiple-value-bind (reply found?) (%await-pending! node rpc-id timeout)
      (declare (ignore reply))
      (unless found? (return-from kademlia-join! nil))))
  (kademlia-find-node node (get-node-id node) :timeout timeout)
  t)
