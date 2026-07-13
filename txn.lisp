;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defvar *transaction*)
(defvar *versioned-compare-view* :after)

(defclass txn ()
  ((repository
    :initarg :repository
    :accessor repository-transaction/repository)
   (stack-repository
    :initarg :stack-repository
    :reader repository-transaction/stack-repository)
   (underlying-transaction
    :initarg :underlying-transaction
    :reader repository-transaction/underlying-transaction)
   (uid
    :initarg :uid
    :reader repository-transaction/uid)
   (reason
    :initarg :reason
    :reader repository-transaction/reason
    :type string)
   (mode
    :initarg :mode
    :reader repository-transaction/mode)
   (status
    :initform :active
    :accessor repository-transaction/status)))

(defclass repository-transaction (txn) ())
(defclass nonversioned-repository-transaction
    (repository-transaction) ())
(defclass nonversioned-update-repository-transaction
    (repository-transaction) ())

(defclass versioned-repository-transaction
    (repository-transaction)
  ((cid-set
    :initarg :cid-set
    :reader repository-transaction/cid-set)))

(defclass versioned-compare-repository-transaction
    (repository-transaction)
  ((before-cid-set
    :initarg :before-cid-set
    :reader repository-transaction/before-cid-set)
   (after-cid-set
    :initarg :after-cid-set
    :reader repository-transaction/after-cid-set)))

(defclass versioned-update-repository-transaction
    (versioned-repository-transaction)
  ((cid
    :initarg :cid
    :reader repository-transaction/cid)
   (cid-object
    :initarg :cid-object
    :reader repository-transaction/cid-object)
   (cid-master-table-entry
    :initarg :cid-master-table-entry
    :accessor repository-transaction/cid-master-table-entry)
   (change-set
    :initform nil
    :accessor repository-transaction/change-set)
   (objects-changed
    :initform nil
    :accessor repository-transaction/objects-changed)))

(defgeneric transaction/cid-set (transaction))

(defmethod transaction/cid-set
    ((transaction versioned-repository-transaction))
  (repository-transaction/cid-set transaction))

(defmethod transaction/cid-set
    ((transaction versioned-compare-repository-transaction))
  (ecase *versioned-compare-view*
    (:before
     (repository-transaction/before-cid-set transaction))
    (:after
     (repository-transaction/after-cid-set transaction))))

(defun call-with-view (view thunk)
  (unless (typep *transaction*
                 'versioned-compare-repository-transaction)
    (error "No comparison transaction is active."))
  (let ((*versioned-compare-view* view))
    (funcall thunk)))

(defun call-with-before-view (thunk)
  (call-with-view :before thunk))

(defun call-with-after-view (thunk)
  (call-with-view :after thunk))

(defun txn-for-update? (transaction)
  (member
   (repository-transaction/mode transaction)
   '(:read-write :nonversioned-read-write)))

(defun repository-transaction/disposition (transaction)
  (repository-transaction/status transaction))

(defun repository-transaction/note-change-set
    (transaction change-set)
  (check-type transaction
              versioned-update-repository-transaction)
  (setf (repository-transaction/change-set transaction)
        change-set)
  transaction)

(defun %transaction-object-entry (transaction object)
  (assoc object
         (repository-transaction/objects-changed transaction)
         :test #'eq))

(defun repository-transaction/log-change
    (transaction versioned-object slot-identifier)
  (check-type transaction
              versioned-update-repository-transaction)
  (check-type slot-identifier symbol)
  (let ((entry (%transaction-object-entry
                transaction versioned-object)))
    (if entry
        (pushnew slot-identifier (cdr entry) :test #'eq)
        (push (list versioned-object slot-identifier)
              (repository-transaction/objects-changed transaction))))
  (multiple-value-bind (new-entry detail-entry)
      (cid-master-table-entry/log-change
       (repository-transaction/cid-master-table-entry transaction)
       versioned-object slot-identifier)
    (setf
     (repository-transaction/cid-master-table-entry transaction)
     new-entry)
    (values transaction detail-entry)))

(defun repository-transaction/update-versioned-value
    (transaction versioned-value new-value instance slot)
  (check-type transaction
              versioned-update-repository-transaction)
  (check-type versioned-value versioned-value)
  (let* ((cid (repository-transaction/cid transaction))
         (basis
           (cid-set/remove
            (transaction/cid-set transaction) cid))
         (updated
           (if (typep versioned-value 'cvi)
               (cvi/update
                versioned-value new-value cid basis instance slot)
               (versioned-value/update
                new-value versioned-value cid instance slot))))
    (repository-transaction/log-change
     transaction instance slot)
    updated))

(defun %uid-equal-p (left right)
  (cond
    ((and (typep left 'canonical-identifier)
          (typep right 'canonical-identifier))
     (canonical-identifier-equal-p left right))
    (t (equal left right))))

(defun %repository-stack-entry (repository uid)
  (find uid
        (repository-transaction-stacks repository)
        :key #'car :test #'%uid-equal-p))

(defun repository/txn-stack-for-uid-spec (repository uid)
  (let ((entry (%repository-stack-entry repository uid)))
    (and entry (cdr entry))))

(defun repository/innermost-txn-for-uid-spec
    (repository uid)
  (first (repository/txn-stack-for-uid-spec repository uid)))

(defun repository/outermost-txn-context-for-uid-spec
    (repository uid)
  (car
   (last
    (repository/txn-stack-for-uid-spec repository uid))))

(defun repository/add-transaction (repository transaction)
  (let* ((uid (repository-transaction/uid transaction))
         (entry (%repository-stack-entry repository uid)))
    (if entry
        (push transaction (cdr entry))
        (push (list uid transaction)
              (repository-transaction-stacks repository))))
  transaction)

(defun repository/remove-transaction (repository transaction)
  (let* ((uid (repository-transaction/uid transaction))
         (entry (%repository-stack-entry repository uid)))
    (unless (and entry (eq (second entry) transaction))
      (error "Transactions must end in LIFO order."))
    (pop (cdr entry))
    (unless (cdr entry)
      (setf (repository-transaction-stacks repository)
            (delete entry
                    (repository-transaction-stacks repository)
                    :test #'eq))))
  transaction)

(defun repository/resolve-uid-specifier (repository specifier)
  (cond
    ((eq specifier :nobody)
     (repository/anonymous-user repository))
    ((typep specifier 'distributed-identifier) specifier)
    ((null specifier) (repository/anonymous-user repository))
    (t (error "Invalid repository user specifier ~S." specifier))))

(defun repository/resolve-cid-set-specifier
    (repository specifier)
  (cond
    ((eq specifier :latest-version)
     (repository/master-cid-set repository))
    ((typep specifier 'cid-set)
     (unless
         (equal (cid-set/repository specifier)
                (repository/identity repository))
       (error "CID set belongs to another repository."))
     specifier)
    ((functionp specifier) (funcall specifier))
    (t (error "Invalid CID-set specifier ~S." specifier))))

(defun repository-transaction-type->transaction-mode (type)
  (ecase type
    ((:read-only :read-only-compare :read-only-nonversioned)
     :read-only)
    ((:read-write :read-write-nonversioned)
     :read-write)))

(defun repository/begin-txn
    (&key repository transaction-type underlying-transaction user
          cid-set-specifier aux-cid-set-specifier reason)
  (check-type repository repository)
  (check-type underlying-transaction transaction)
  (let ((transaction
          (ecase transaction-type
            (:read-only-nonversioned
             (make-instance
              'nonversioned-repository-transaction
              :repository repository
              :stack-repository repository
              :underlying-transaction underlying-transaction
              :uid user :reason reason
              :mode :nonversioned-read-only))
            (:read-write-nonversioned
             (make-instance
              'nonversioned-update-repository-transaction
              :repository repository
              :stack-repository repository
              :underlying-transaction underlying-transaction
              :uid user :reason reason
              :mode :nonversioned-read-write))
            (:read-only
             (make-instance
              'versioned-repository-transaction
              :repository repository
              :stack-repository repository
              :underlying-transaction underlying-transaction
              :uid user :reason reason :mode :read-only
              :cid-set
              (repository/resolve-cid-set-specifier
               repository cid-set-specifier)))
            (:read-only-compare
             (make-instance
              'versioned-compare-repository-transaction
              :repository repository
              :stack-repository repository
              :underlying-transaction underlying-transaction
              :uid user :reason reason :mode :read-only
              :before-cid-set
              (repository/resolve-cid-set-specifier
               repository cid-set-specifier)
              :after-cid-set
              (repository/resolve-cid-set-specifier
               repository aux-cid-set-specifier)))
            (:read-write
             (multiple-value-bind
                   (updated-repository cid cid-object)
                 (repository/allocate-cid repository)
               (let* ((basis
                        (repository/resolve-cid-set-specifier
                         repository cid-set-specifier))
                      (entry
                        (make-cid-master-table-entry
                         :cid cid :cid-set-basis basis
                         :who user :why reason)))
                 (make-instance
                  'versioned-update-repository-transaction
                  :repository updated-repository
                  :stack-repository repository
                  :underlying-transaction underlying-transaction
                  :uid user :reason reason :mode :read-write
                  :cid cid :cid-object cid-object
                  :cid-master-table-entry entry
                  :cid-set (cid-set/adjoin basis cid))))))))
    (repository/add-transaction repository transaction)
    transaction))

(defun %finalize-update-transaction (transaction)
  (let* ((repository
           (repository-transaction/repository transaction))
         (finished-entry
           (cid-master-table-entry/note-finish
            (repository-transaction/cid-master-table-entry transaction)
            (repository-transaction/reason transaction)
            (repository-transaction/change-set transaction)))
         (master
           (cid-master-table/add-entry
            (repository/cid-master-table repository)
            finished-entry))
         (master-with-object
           (cid-master-table/set-cid-object
            master
            (repository-transaction/cid transaction)
            (repository-transaction/cid-object transaction)))
         (finished-repository
           (repository/with-cid-master-table
            repository master-with-object)))
    (setf (repository-transaction/repository transaction)
          finished-repository)
    (repository/save
     (repository-transaction/underlying-transaction transaction)
     finished-repository)
    finished-repository))

(defun repository/end-txn
    (repository transaction &key commit)
  (check-type transaction repository-transaction)
  (unless
      (eq transaction
          (repository/innermost-txn-for-uid-spec
           repository (repository-transaction/uid transaction)))
    (error "Attempt to end a transaction out of sequence."))
  (when (and commit
             (typep transaction
                    'versioned-update-repository-transaction))
    (%finalize-update-transaction transaction))
  (repository/remove-transaction repository transaction)
  (setf (repository-transaction/status transaction)
        (if commit :committed :aborted))
  transaction)

(defun repository-transaction/commit (transaction)
  (unless (eq (repository-transaction/status transaction) :active)
    (error "Repository transaction is not active."))
  (repository/end-txn
   (repository-transaction/stack-repository transaction)
   transaction :commit t)
  (transaction/commit
   (repository-transaction/underlying-transaction transaction)))

(defun repository-transaction/abort (transaction)
  (unless (eq (repository-transaction/status transaction) :active)
    (error "Repository transaction is not active."))
  (repository/end-txn
   (repository-transaction/stack-repository transaction)
   transaction :commit nil)
  (transaction/abort
   (repository-transaction/underlying-transaction transaction)))

(defun call-with-repository-transaction
    (&key repository
          (transaction-type :read-only)
          (user-id-specifier :nobody)
          (reason "")
          (cid-set-specifier :latest-version)
          aux-cid-set-specifier
          receiver)
  (check-type reason string)
  (check-type receiver function)
  (let ((results nil)
        (repository-transaction nil)
        (completed-p nil))
    (call-with-transaction
     user-id-specifier reason
     (lambda (underlying)
       (let* ((current-repository
                (or repository
                    (repository/load
                     underlying :error-if-missing t)))
              (user
                (repository/resolve-uid-specifier
                 current-repository user-id-specifier)))
         (setf repository-transaction
               (repository/begin-txn
                :repository current-repository
                :transaction-type transaction-type
                :underlying-transaction underlying
                :user user
                :reason reason
                :cid-set-specifier cid-set-specifier
                :aux-cid-set-specifier aux-cid-set-specifier))
         (let ((*transaction* repository-transaction))
           (unwind-protect
                (progn
                  (setf results
                        (multiple-value-list
                         (funcall receiver repository-transaction)))
                  (when
                      (eq (repository-transaction/status
                           repository-transaction)
                          :active)
                    (repository/end-txn
                     current-repository repository-transaction
                     :commit t))
                  (setf completed-p t))
             (when
                 (and repository-transaction
                      (eq (repository-transaction/status
                           repository-transaction)
                          :active))
               (repository/end-txn
                current-repository repository-transaction
                :commit nil)))))))
    (if completed-p
        (values-list results)
        repository-transaction)))
