;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defparameter +version-techniques+
  '(:nonversioned :scalar :logged :nonlogged
    :composite-set :composite-sequence :composite-file))

(deftype version-technique ()
  '(member :nonversioned :scalar :logged :nonlogged
    :composite-set :composite-sequence :composite-file))

(defun version-technique->versioned-value-class (technique)
  (ecase technique
    (:nonversioned nil)
    (:scalar 'scalar-versioned-value)
    (:logged 'logged-versioned-value)
    (:nonlogged 'nonlogged-versioned-value)
    ((:composite-set :composite-sequence) 'cvi)
    (:composite-file 'cvfile)))

(defclass versioned-standard-class (standard-class) ())

(defclass versioned-slot-definition ()
  ((version-technique
    :initarg :version-technique
    :initform :nonversioned
    :accessor slot-definition-version-technique)))

(defclass versioned-direct-slot-definition
    (versioned-slot-definition
     sb-mop:standard-direct-slot-definition)
  ())

(defclass versioned-effective-slot-definition
    (versioned-slot-definition
     sb-mop:standard-effective-slot-definition)
  ())

(defmethod sb-mop:validate-superclass
    ((class versioned-standard-class) (superclass standard-class))
  (declare (ignore class superclass))
  t)

(defmethod sb-mop:validate-superclass
    ((class standard-class) (superclass versioned-standard-class))
  (declare (ignore class superclass))
  t)

(defclass versioned-standard-object ()
  ((birth-cid-object
    :initarg :birth-cid-object
    :initform nil
    :reader versioned-object/birth-cid-object))
  (:metaclass versioned-standard-class))

(defmethod sb-mop:direct-slot-definition-class
    ((class versioned-standard-class) &rest initargs)
  (if (getf initargs :version-technique)
      (find-class 'versioned-direct-slot-definition)
      (call-next-method)))

(defmethod sb-mop:effective-slot-definition-class
    ((class versioned-standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'versioned-effective-slot-definition))

(defmethod sb-mop:compute-effective-slot-definition
    :around
    ((class versioned-standard-class) name direct-slots)
  (declare (ignore name))
  (let ((result (call-next-method)))
    (when (typep result 'versioned-effective-slot-definition)
      (let ((versioned
              (find-if
               (lambda (slot)
                 (typep slot 'versioned-slot-definition))
               direct-slots)))
        (setf (slot-definition-version-technique result)
              (if versioned
                  (slot-definition-version-technique versioned)
                  :nonversioned))))
    result))

(defun slot-name->versioned-initarg (slot-name)
  (intern
   (concatenate
    'string "VERSIONED-INITARG-FOR-"
    (symbol-name slot-name))
   (symbol-package slot-name)))

(defun versioned-slot-definition/versioned-initarg (slot)
  (slot-name->versioned-initarg
   (sb-mop:slot-definition-name slot)))

(defun scan-class-versioned-effective-slots (class)
  (remove-if-not
   (lambda (slot)
     (and (typep slot 'versioned-effective-slot-definition)
          (not
           (eq (slot-definition-version-technique slot)
               :nonversioned))))
   (sb-mop:class-slots class)))

(defvar *disable-versioning* nil)
(defvar *versioned-value-cid-set-override* nil)

(defun %active-update-transaction ()
  (and (boundp '*transaction*)
       (typep *transaction*
              'versioned-update-repository-transaction)
       *transaction*))

(defun %initial-version-cid ()
  (let ((transaction (%active-update-transaction)))
    (if transaction
        (repository-transaction/cid transaction)
        1)))

(defun %initial-version-basis ()
  (let ((transaction (%active-update-transaction)))
    (if transaction
        (cid-set/remove
         (transaction/cid-set transaction)
         (repository-transaction/cid transaction))
        (cid-set/empty nil))))

(defun %make-slot-versioned-value
    (technique assigned-p value)
  (ecase technique
    (:scalar
     (if assigned-p
         (make-scalar-versioned-value :initial-value value)
         (make-scalar-versioned-value)))
    (:logged
     (if assigned-p
         (make-logged-versioned-value
          :initial-value value :cid (%initial-version-cid))
         (make-logged-versioned-value)))
    (:nonlogged
     (if assigned-p
         (make-nonlogged-versioned-value
          :initial-value value :cid (%initial-version-cid))
         (make-nonlogged-versioned-value)))
    ((:composite-set :composite-sequence)
     (let ((result
             (if (and assigned-p (null value))
                 (make-cvi :initial-value nil)
                 (make-cvi))))
       (if (and assigned-p value)
           (cvi/update
            result value (%initial-version-cid)
            (%initial-version-basis))
           result)))
    (:composite-file
     (if assigned-p
         (make-cvfile
          :initial-value value :cid (%initial-version-cid))
         (make-cvfile :initial-value #() :cid 1)))))

(defmethod initialize-instance :around
    ((instance versioned-standard-object) &rest initargs)
  (let ((*disable-versioning* t))
    (apply #'call-next-method instance initargs))
  (dolist (slot
           (scan-class-versioned-effective-slots
            (class-of instance)))
    (let ((*disable-versioning* t))
      (let* ((name (sb-mop:slot-definition-name slot))
             (bound-p (slot-boundp instance name))
             (raw (and bound-p (slot-value instance name))))
        (unless (typep raw 'versioned-value)
          (setf (slot-value instance name)
                (%make-slot-versioned-value
                 (slot-definition-version-technique slot)
                 bound-p raw))))))
  (let ((transaction (%active-update-transaction)))
    (when (and transaction
               (null (versioned-object/birth-cid-object instance)))
      (setf (slot-value instance 'birth-cid-object)
            (repository-transaction/cid-object transaction))))
  instance)

(defun %versioned-slot-cid-set ()
  (or *versioned-value-cid-set-override*
      (and (boundp '*transaction*)
           (typep *transaction* 'versioned-repository-transaction)
           (transaction/cid-set *transaction*))
      (cid-set/empty nil)))

(defmethod sb-mop:slot-value-using-class
    ((class versioned-standard-class)
     (instance versioned-standard-object)
     (slot versioned-effective-slot-definition))
  (let ((raw (call-next-method)))
    (if (or *disable-versioning*
            (eq (slot-definition-version-technique slot)
                :nonversioned))
        raw
        (versioned-value/view
         raw (%versioned-slot-cid-set) instance
         (sb-mop:slot-definition-name slot)))))

(defmethod (setf sb-mop:slot-value-using-class)
    (new-value
     (class versioned-standard-class)
     (instance versioned-standard-object)
     (slot versioned-effective-slot-definition))
  (cond
    ((or *disable-versioning*
         (eq (slot-definition-version-technique slot)
             :nonversioned))
     (call-next-method))
    (*versioned-value-cid-set-override*
     (error "Cannot modify a slot in an overridden CID-set view."))
    (t
     (let ((transaction (%active-update-transaction)))
       (unless transaction
         (error "A versioned slot update requires an update transaction."))
       (call-next-method
        (repository-transaction/update-versioned-value
         transaction
         (let ((*disable-versioning* t))
           (slot-value instance
                       (sb-mop:slot-definition-name slot)))
         new-value instance
         (sb-mop:slot-definition-name slot))
        class instance slot)))))

(defun slot-value-unversioned (instance slot-name)
  (let ((*disable-versioning* t))
    (slot-value instance slot-name)))

(defun (setf slot-value-unversioned)
    (new-value instance slot-name)
  (let ((*disable-versioning* t))
    (setf (slot-value instance slot-name) new-value)))

(defun call-with-cid-set-view (cid-set receiver)
  (check-type cid-set cid-set)
  (let ((*versioned-value-cid-set-override* cid-set))
    (funcall receiver cid-set)))

(defun call-comparing-views
    (left-cid-set right-cid-set thunk)
  (values
   (call-with-cid-set-view left-cid-set
                          (lambda (ignored)
                            (declare (ignore ignored))
                            (funcall thunk)))
   (call-with-cid-set-view right-cid-set
                          (lambda (ignored)
                            (declare (ignore ignored))
                            (funcall thunk)))))

(defun versioned-object/scan-composite-versioned-slot
    (instance slot-name)
  (composite-versioned-value/scan
   (slot-value-unversioned instance slot-name)
   (%versioned-slot-cid-set) instance slot-name))

(defun versioned-object/most-recent-slot-cid
    (instance slot-name cid-set)
  (versioned-value/most-recent-cid
   (slot-value-unversioned instance slot-name)
   cid-set))

(defun versioned-object-slot/cid-set
    (repository instance slot-name)
  (versioned-value/cid-set
   (repository/identity repository)
   (slot-value-unversioned instance slot-name)))

(defun versioned-object/cid-set (repository instance)
  (let ((result
          (cid-set/empty (repository/identity repository))))
    (dolist (slot
             (scan-class-versioned-effective-slots
              (class-of instance))
             result)
      (setf result
            (cid-set/union
             result
             (versioned-value/cid-set
              (repository/identity repository)
              (slot-value-unversioned
               instance
               (sb-mop:slot-definition-name slot))))))))
