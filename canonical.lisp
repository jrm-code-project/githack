;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(declaim (ftype function cid-object-sha cid-object-from-sha))

(defclass canonical-object () ())

(defclass canonical-class-dictionary ()
  ((sha
    :initarg :sha
    :reader canonical-class-dictionary-sha
    :type string)))

(defgeneric canonical-object-equal-p (left right))

(defmethod canonical-object-equal-p
    ((left canonical-object) (right canonical-object))
  (eq left right))

(defun %canonical-object-reference (object)
  (if (and (find-class 'cid-object nil)
           (typep object 'cid-object))
      (list :cid-object (cid-object-sha object))
      (error "Canonical object type ~S has no persistent reference."
             (type-of object))))

(defun %canonical-reference-object (reference)
  (unless (and (consp reference)
               (consp (cdr reference))
               (null (cddr reference)))
    (error "Invalid canonical object reference ~S." reference))
  (ecase (first reference)
    (:cid-object (cid-object-from-sha (second reference)))))

(defun %make-canonical-dictionary-root (repo-ptr entries-sha)
  (%persistent-hash-table-data repo-ptr entries-sha)
  (create-tree
   repo-ptr
   (list (list "entries" entries-sha +git-filemode-tree+))))

(defun %canonical-dictionary-entries-sha (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 1)
                 (string= (first (first entries)) "entries")
                 (= (third (first entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a canonical class dictionary." sha))
    (let ((entries-sha (second (first entries))))
      (%persistent-hash-table-data repo-ptr entries-sha)
      entries-sha)))

(defun make-canonical-class-dictionary ()
  (let ((entries
          (make-persistent-hash-table
           :test 'eql :size 8)))
    (with-repository ()
      (make-instance
       'canonical-class-dictionary
       :sha (%make-canonical-dictionary-root
             (current-repository)
             (persistent-hash-table-sha entries))))))

(defun canonical-class-dictionary-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%canonical-dictionary-entries-sha
     (current-repository) sha)
    (make-instance 'canonical-class-dictionary :sha sha)))

(defun canonical-class-dictionary/entries (dictionary)
  (check-type dictionary canonical-class-dictionary)
  (with-repository ()
    (make-instance
     'persistent-hash-table
     :sha
     (%canonical-dictionary-entries-sha
      (current-repository)
      (canonical-class-dictionary-sha dictionary)))))

(defun %canonical-class-vector (dictionary class-symbol)
  (multiple-value-bind (sha present-p)
      (persistent-gethash
       class-symbol
       (canonical-class-dictionary/entries dictionary))
    (and present-p (persistent-vector-from-sha sha))))

(defun canonical-class-dictionary/find-object-class-symbol
    (dictionary class-symbol)
  (check-type class-symbol symbol)
  (%canonical-class-vector dictionary class-symbol))

(defgeneric canonical-class-dictionary-entry/find-object
    (entry canonical-object))

(defmethod canonical-class-dictionary-entry/find-object
    ((entry persistent-vector) (object canonical-object))
  (loop for index below (persistent-vector-length entry)
        for candidate =
          (%canonical-reference-object
           (persistent-vector-ref entry index))
        when (canonical-object-equal-p candidate object)
          return candidate))

(defgeneric canonical-class-dictionary-entry/add-object
    (entry canonical-object))

(defmethod canonical-class-dictionary-entry/add-object
    ((entry persistent-vector) (object canonical-object))
  (nth-value
   0
   (persistent-vector-push-extend
    (%canonical-object-reference object) entry)))

(defun canonical-class-dictionary/find-object
    (dictionary object)
  (check-type object canonical-object)
  (let ((entry
          (%canonical-class-vector dictionary (type-of object))))
    (and entry
         (canonical-class-dictionary-entry/find-object
          entry object))))

(defun canonical-class-dictionary/add-object
    (dictionary object)
  "Returns the updated dictionary and canonical object."
  (check-type dictionary canonical-class-dictionary)
  (check-type object canonical-object)
  (let* ((class-symbol (type-of object))
         (entry
           (or (%canonical-class-vector dictionary class-symbol)
               (make-persistent-vector 0)))
         (new-entry
           (canonical-class-dictionary-entry/add-object
            entry object))
         (new-entries
           (persistent-hash-table-set
            (canonical-class-dictionary/entries dictionary)
            class-symbol (persistent-vector-sha new-entry))))
    (with-repository ()
      (values
       (make-instance
        'canonical-class-dictionary
        :sha (%make-canonical-dictionary-root
              (current-repository)
              (persistent-hash-table-sha new-entries)))
       object))))

(defun canonical-class-dictionary/find-or-add-object
    (dictionary object)
  "Returns the canonical object, updated dictionary, and an added flag."
  (let ((existing
          (canonical-class-dictionary/find-object dictionary object)))
    (if existing
        (values existing dictionary nil)
        (multiple-value-bind (updated added-object)
            (canonical-class-dictionary/add-object dictionary object)
          (values added-object updated t)))))

(defun canonical-object/find-or-create
    (dictionary instance)
  (canonical-class-dictionary/find-or-add-object
   dictionary instance))
