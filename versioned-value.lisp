;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass versioned-value ()
  ((sha :initarg :sha :reader versioned-value-sha :type string)))

(defclass nonlogged-versioned-value (versioned-value) ())
(defclass logged-versioned-value (nonlogged-versioned-value) ())
(defclass scalar-versioned-value (versioned-value) ())

(define-condition unbound-versioned-value (error)
  ((versioned-value
    :initarg :versioned-value
    :reader unbound-versioned-value/object))
  (:report
   (lambda (condition stream)
     (format stream "Versioned value ~S has no visible value."
             (unbound-versioned-value/object condition)))))

(define-condition versioned-object-no-change-condition (warning)
  ((object-being-changed
    :initarg :object-being-changed
    :reader versioned-object-no-change-condition/object-being-changed)
   (slot-name
    :initarg :slot-name
    :reader versioned-object-no-change-condition/slot-name)
   (slot-value
    :initarg :slot-value
    :reader versioned-object-no-change-condition/slot-value))
  (:report
   (lambda (condition stream)
     (format stream "No change to slot ~S on ~S; value remains ~S."
             (versioned-object-no-change-condition/slot-name condition)
             (versioned-object-no-change-condition/object-being-changed
              condition)
             (versioned-object-no-change-condition/slot-value condition)))))

(defgeneric vi-value-same? (new-value old-value))

(defmethod vi-value-same? ((new-value t) (old-value t))
  (equal new-value old-value))

(defmethod vi-value-same? ((new-value string) (old-value string))
  (string= new-value old-value))

(defmethod vi-value-same? ((new-value array) (old-value array))
  (and (equalp (array-element-type new-value)
               (array-element-type old-value))
       (equalp new-value old-value)))

(defun %signal-no-versioned-change (instance slot value)
  (warn 'versioned-object-no-change-condition
        :object-being-changed instance
        :slot-name slot
        :slot-value value))

(defun %check-version-cid (cid)
  (check-type cid (integer 1 *))
  cid)

(defun %versioned-value-type-class (type)
  (ecase type
    (:nonlogged 'nonlogged-versioned-value)
    (:logged 'logged-versioned-value)
    (:scalar 'scalar-versioned-value)
    (:cvi 'cvi)
    (:cvfile 'cvfile)))

(defun %make-single-versioned-value-root
    (repo-ptr type assigned-p value cid)
  (create-tree
   repo-ptr
   (list
    (list "assigned"
          (%stored-object repo-ptr assigned-p)
          +git-filemode-blob+)
    (list "cid" (%stored-object repo-ptr cid)
          +git-filemode-blob+)
    (list "type" (%stored-object repo-ptr type)
          +git-filemode-blob+)
    (list "value" (%stored-object repo-ptr value)
          +git-filemode-blob+))))

(defun %single-versioned-value-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 4)
                 (equal (mapcar #'first entries)
                        '("assigned" "cid" "type" "value"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  entries))
      (error "Git tree ~S is not a single versioned value." sha))
    (let ((assigned-p
            (%loaded-object repo-ptr (second (first entries))))
          (cid (%loaded-object repo-ptr (second (second entries))))
          (type (%loaded-object repo-ptr (second (third entries))))
          (value (%loaded-object repo-ptr (second (fourth entries)))))
      (unless (member type '(:nonlogged :logged))
        (error "Invalid single versioned value type ~S." type))
      (unless (typep assigned-p 'boolean)
        (error "Invalid assigned flag ~S." assigned-p))
      (if assigned-p
          (%check-version-cid cid)
          (unless (zerop cid)
            (error "Unassigned versioned value has CID ~S." cid)))
      (values type assigned-p value cid))))

(defun %make-scalar-versioned-value-root
    (repo-ptr initial-assigned-p initial-value cids-sha values-sha)
  (let ((cids (%persistent-vector-data repo-ptr cids-sha))
        (values (%persistent-vector-data repo-ptr values-sha)))
    (unless (= (persistent-vector-data-length cids)
               (persistent-vector-data-length values))
      (error "Scalar version vectors have different lengths."))
    (create-tree
     repo-ptr
     (list
      (list "cids" cids-sha +git-filemode-tree+)
      (list "initial-assigned"
            (%stored-object repo-ptr initial-assigned-p)
            +git-filemode-blob+)
      (list "initial-value"
            (%stored-object repo-ptr initial-value)
            +git-filemode-blob+)
      (list "type" (%stored-object repo-ptr :scalar)
            +git-filemode-blob+)
      (list "values" values-sha +git-filemode-tree+)))))

(defun %scalar-versioned-value-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 5)
                 (equal (mapcar #'first entries)
                        '("cids" "initial-assigned"
                          "initial-value" "type" "values"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (second entries)) +git-filemode-blob+)
                 (= (third (third entries)) +git-filemode-blob+)
                 (= (third (fourth entries)) +git-filemode-blob+)
                 (= (third (fifth entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a scalar versioned value." sha))
    (let* ((cids-sha (second (first entries)))
           (values-sha (second (fifth entries)))
           (cids (%persistent-vector-data repo-ptr cids-sha))
           (values (%persistent-vector-data repo-ptr values-sha))
           (initial-assigned-p
             (%loaded-object repo-ptr (second (second entries))))
           (initial-value
             (%loaded-object repo-ptr (second (third entries))))
           (type (%loaded-object repo-ptr (second (fourth entries)))))
      (unless (eq type :scalar)
        (error "Invalid scalar versioned value type ~S." type))
      (unless (typep initial-assigned-p 'boolean)
        (error "Invalid initial assigned flag ~S."
               initial-assigned-p))
      (unless (= (persistent-vector-data-length cids)
                 (persistent-vector-data-length values))
        (error "Scalar version vectors have different lengths."))
      (values initial-assigned-p initial-value cids-sha values-sha))))

(defun %versioned-value-type (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (let ((type-entry (find "type" entries
                            :key #'first :test #'string=)))
      (unless (and type-entry
                   (= (third type-entry) +git-filemode-blob+))
        (error "Git tree ~S has no versioned value type." sha))
      (%loaded-object repo-ptr (second type-entry)))))

(defgeneric %validate-versioned-value-sha (type repo-ptr sha))

(defmethod %validate-versioned-value-sha
    ((type (eql :nonlogged)) repo-ptr sha)
  (declare (ignore type))
  (%single-versioned-value-data repo-ptr sha))

(defmethod %validate-versioned-value-sha
    ((type (eql :logged)) repo-ptr sha)
  (declare (ignore type))
  (%single-versioned-value-data repo-ptr sha))

(defmethod %validate-versioned-value-sha
    ((type (eql :scalar)) repo-ptr sha)
  (declare (ignore type))
  (%scalar-versioned-value-data repo-ptr sha))

(defun versioned-value-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (type (%versioned-value-type repo-ptr sha)))
      (%validate-versioned-value-sha type repo-ptr sha)
      (make-instance (%versioned-value-type-class type) :sha sha))))

(defun %make-single-versioned-value
    (type initial-value initial-value-supplied-p cid)
  (when initial-value-supplied-p
    (%check-version-cid cid))
  (with-repository ()
    (make-instance
     (%versioned-value-type-class type)
     :sha (%make-single-versioned-value-root
           (current-repository) type
           initial-value-supplied-p initial-value
           (if initial-value-supplied-p cid 0)))))

(defun make-nonlogged-versioned-value
    (&key (initial-value nil initial-value-supplied-p) (cid 0))
  (%make-single-versioned-value
   :nonlogged initial-value initial-value-supplied-p cid))

(defun make-logged-versioned-value
    (&key (initial-value nil initial-value-supplied-p) (cid 0))
  (%make-single-versioned-value
   :logged initial-value initial-value-supplied-p cid))

(defun make-scalar-versioned-value
    (&key (initial-value nil initial-value-supplied-p))
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (cids (%persistent-vector-object
                  repo-ptr (%persistent-node-null-sha repo-ptr) 0))
           (values (%persistent-vector-object
                    repo-ptr (%persistent-node-null-sha repo-ptr) 0)))
      (make-instance
       'scalar-versioned-value
       :sha (%make-scalar-versioned-value-root
             repo-ptr initial-value-supplied-p initial-value
             (persistent-vector-sha cids)
             (persistent-vector-sha values))))))

(defun nonlogged-versioned-value/value (value)
  (check-type value nonlogged-versioned-value)
  (with-repository ()
    (multiple-value-bind (type assigned-p stored-value cid)
        (%single-versioned-value-data
         (current-repository) (versioned-value-sha value))
      (declare (ignore type cid))
      (if assigned-p
          stored-value
          (error 'unbound-versioned-value :versioned-value value)))))

(defun nonlogged-versioned-value/cid (value)
  (check-type value nonlogged-versioned-value)
  (with-repository ()
    (multiple-value-bind (type assigned-p stored-value cid)
        (%single-versioned-value-data
         (current-repository) (versioned-value-sha value))
      (declare (ignore type assigned-p stored-value))
      (and (plusp cid) cid))))

(defun scalar-versioned-value/default-value (value)
  (check-type value scalar-versioned-value)
  (with-repository ()
    (multiple-value-bind
        (assigned-p initial-value cids-sha values-sha)
        (%scalar-versioned-value-data
         (current-repository) (versioned-value-sha value))
      (declare (ignore cids-sha values-sha))
      (if assigned-p
          initial-value
          (error 'unbound-versioned-value :versioned-value value)))))

(defgeneric versioned-value/cid-list (versioned-value))
(defgeneric versioned-value/cid-set (repository versioned-value))
(defgeneric versioned-value/contains-cid? (versioned-value cid))
(defgeneric versioned-value/most-recent-cid
    (versioned-value cid-set))
(defgeneric versioned-value/view
    (versioned-value cid-set instance slot))
(defgeneric versioned-value/update
    (new-value versioned-value context instance slot))

(defmethod versioned-value/cid-list
    ((value nonlogged-versioned-value))
  (let ((cid (nonlogged-versioned-value/cid value)))
    (if cid (list cid) nil)))

(defmethod versioned-value/cid-set
    (repository (value versioned-value))
  (list->cid-set repository (versioned-value/cid-list value)))

(defmethod versioned-value/contains-cid?
    ((value nonlogged-versioned-value) cid)
  (eql (nonlogged-versioned-value/cid value) cid))

(defmethod versioned-value/most-recent-cid
    ((value nonlogged-versioned-value) cid-set)
  (let ((cid (nonlogged-versioned-value/cid value)))
    (and cid (cid-set/member cid-set cid) cid)))

(defmethod versioned-value/view
    ((value nonlogged-versioned-value) cid-set instance slot)
  (declare (ignore cid-set instance slot))
  (nonlogged-versioned-value/value value))

(defmethod versioned-value/update
    (new-value (value nonlogged-versioned-value)
     (cid integer) instance slot)
  (%check-version-cid cid)
  (let ((old-cid (nonlogged-versioned-value/cid value)))
    (when (and old-cid
               (/= old-cid cid)
               (vi-value-same?
                new-value
                (nonlogged-versioned-value/value value)))
      (%signal-no-versioned-change instance slot new-value))
    (with-repository ()
      (let* ((repo-ptr (current-repository))
             (type (%versioned-value-type
                    repo-ptr (versioned-value-sha value))))
        (make-instance
         (%versioned-value-type-class type)
         :sha (%make-single-versioned-value-root
               repo-ptr type t new-value cid))))))

(defun %scalar-version-vectors (value)
  (with-repository ()
    (multiple-value-bind
        (assigned-p initial-value cids-sha values-sha)
        (%scalar-versioned-value-data
         (current-repository) (versioned-value-sha value))
      (values assigned-p initial-value
              (make-instance 'persistent-vector :sha cids-sha)
              (make-instance 'persistent-vector :sha values-sha)))))

(defmethod versioned-value/cid-list
    ((value scalar-versioned-value))
  (multiple-value-bind
      (assigned-p initial-value cids values)
      (%scalar-version-vectors value)
    (declare (ignore assigned-p initial-value values))
    (coerce (persistent-vector->vector cids) 'list)))

(defmethod versioned-value/contains-cid?
    ((value scalar-versioned-value) cid)
  (not
   (null
    (find cid (versioned-value/cid-list value) :test #'=))))

(defun scalar-versioned-value/active-value-pair
    (value cid-set)
  (multiple-value-bind
      (initial-assigned-p initial-value cids values)
      (%scalar-version-vectors value)
    (declare (ignore initial-assigned-p initial-value))
    (loop for index downfrom
          (1- (persistent-vector-length cids)) to 0
          for cid = (persistent-vector-ref cids index)
          when (cid-set/member cid-set cid)
            return (values cid
                           (persistent-vector-ref values index))
          finally (return (values nil nil)))))

(defmethod versioned-value/most-recent-cid
    ((value scalar-versioned-value) cid-set)
  (nth-value 0
             (scalar-versioned-value/active-value-pair
              value cid-set)))

(defmethod versioned-value/view
    ((value scalar-versioned-value) cid-set instance slot)
  (declare (ignore instance slot))
  (multiple-value-bind (cid active-value)
      (scalar-versioned-value/active-value-pair value cid-set)
    (cond
      (cid active-value)
      (t
       (handler-case
           (scalar-versioned-value/default-value value)
         (unbound-versioned-value ()
           (error 'unbound-versioned-value
                  :versioned-value value)))))))

(defun scalar-versioned-value/add-value (value cid new-value)
  (%check-version-cid cid)
  (multiple-value-bind
      (initial-assigned-p initial-value cids values)
      (%scalar-version-vectors value)
    (let* ((length (persistent-vector-length cids))
           (last-cid
             (and (plusp length)
                  (persistent-vector-ref cids (1- length)))))
      (when (and last-cid (< cid last-cid))
        (error "Cannot append CID ~D after CID ~D." cid last-cid))
      (let* ((replace-p (and last-cid (= last-cid cid)))
             (new-cids
               (if replace-p
                   cids
                   (nth-value
                    0 (persistent-vector-push-extend cid cids))))
             (new-values
               (if replace-p
                   (persistent-vector-update
                    values (1- length) new-value)
                   (nth-value
                    0
                    (persistent-vector-push-extend
                     new-value values)))))
        (with-repository ()
          (make-instance
           'scalar-versioned-value
           :sha (%make-scalar-versioned-value-root
                 (current-repository)
                 initial-assigned-p initial-value
                 (persistent-vector-sha new-cids)
                 (persistent-vector-sha new-values))))))))

(defmethod versioned-value/update
    (new-value (value scalar-versioned-value)
     (cid integer) instance slot)
  (%check-version-cid cid)
  (let* ((cid-list (versioned-value/cid-list value))
         (last-cid (car (last cid-list))))
    (when (and last-cid
               (/= last-cid cid))
      (multiple-value-bind
          (assigned-p initial-value cids values)
          (%scalar-version-vectors value)
        (declare (ignore assigned-p initial-value cids))
        (when (vi-value-same?
               new-value
               (persistent-vector-ref
                values
                (1- (persistent-vector-length values))))
          (%signal-no-versioned-change instance slot new-value))))
    (scalar-versioned-value/add-value value cid new-value)))
