;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass cid-detail-table-entry ()
  ((sha :initarg :sha :reader cid-detail-table-entry-sha :type string)))

(defclass cid-detail-table ()
  ((sha :initarg :sha :reader cid-detail-table-sha :type string)))

(defstruct (cid-detail-table-entry-data
             (:constructor make-cid-detail-table-entry-data
                 (object-changed slots-sha)))
  object-changed
  slots-sha)

(defun %cid-detail-object-reference (object)
  (typecase object
    (cid-object
     (list :cid-object (cid-object-sha object)))
    (cid-set
     (list :cid-set (cid-set-sha object)))
    (persistent-vector
     (list :persistent-vector (persistent-vector-sha object)))
    (persistent-hash-table
     (list :persistent-hash-table
           (persistent-hash-table-sha object)))
    (t (list :value object))))

(defun %cid-detail-object-key (object)
  (typecase object
    ;; Resolution adds a local CID but does not change distributed identity.
    (cid-object
     (let ((did (cid-object/did object)))
       (list
        :cid-object-did
        (if (typep did 'distributed-identifier)
            (did->list did)
            did))))
    (t (%cid-detail-object-reference object))))

(defun %cid-detail-reference-object (reference)
  (unless (and (consp reference)
               (consp (cdr reference))
               (null (cddr reference)))
    (error "Invalid CID detail object reference ~S." reference))
  (ecase (first reference)
    (:cid-object (cid-object-from-sha (second reference)))
    (:cid-set (cid-set-from-sha (second reference)))
    (:persistent-vector
     (persistent-vector-from-sha (second reference)))
    (:persistent-hash-table
     (persistent-hash-table-from-sha (second reference)))
    (:value (second reference))))

(defun %make-empty-persistent-vector (repo-ptr)
  (%persistent-vector-object
   repo-ptr (%persistent-node-null-sha repo-ptr) 0))

(defun %make-cid-detail-table-entry-root
    (repo-ptr object-changed slots-sha)
  (%persistent-vector-data repo-ptr slots-sha)
  (create-tree
   repo-ptr
   (list
    (list "object"
         (%stored-object
          repo-ptr
          (%cid-detail-object-reference object-changed))
          +git-filemode-blob+)
    (list "slots" slots-sha +git-filemode-tree+))))

(defun %cid-detail-table-entry-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("object" "slots"))
                 (= (third (first entries)) +git-filemode-blob+)
                 (= (third (second entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a CID detail table entry." sha))
    (let ((slots-sha (second (second entries))))
      (%persistent-vector-data repo-ptr slots-sha)
      (make-cid-detail-table-entry-data
       (%cid-detail-reference-object
        (%loaded-object repo-ptr (second (first entries))))
       slots-sha))))

(defun %cid-detail-table-entry-object
    (repo-ptr object-changed slots-sha)
  (make-instance
   'cid-detail-table-entry
   :sha (%make-cid-detail-table-entry-root
         repo-ptr object-changed slots-sha)))

(defun make-cid-detail-table-entry
    (object-changed &key slots-modified)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (slots (make-persistent-vector
                   (length slots-modified)
                   :initial-contents slots-modified)))
      (%cid-detail-table-entry-object
       repo-ptr object-changed (persistent-vector-sha slots)))))

(defun cid-detail-table-entry-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-detail-table-entry-data (current-repository) sha)
    (make-instance 'cid-detail-table-entry :sha sha)))

(defun cid-detail-table-entry/object-changed (entry)
  (check-type entry cid-detail-table-entry)
  (with-repository ()
    (cid-detail-table-entry-data-object-changed
     (%cid-detail-table-entry-data
      (current-repository)
      (cid-detail-table-entry-sha entry)))))

(defun cid-detail-table-entry/slots-modified (entry)
  (check-type entry cid-detail-table-entry)
  (with-repository ()
    (make-instance
     'persistent-vector
     :sha
     (cid-detail-table-entry-data-slots-sha
      (%cid-detail-table-entry-data
       (current-repository)
       (cid-detail-table-entry-sha entry))))))

(defun %make-cid-detail-table-root (repo-ptr entries-sha)
  (%persistent-hash-table-data repo-ptr entries-sha)
  (create-tree
   repo-ptr
   (list (list "entries" entries-sha +git-filemode-tree+))))

(defun %cid-detail-table-entries-sha (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 1)
                 (string= (first (first entries)) "entries")
                 (= (third (first entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a CID detail table." sha))
    (let ((entries-sha (second (first entries))))
      (%persistent-hash-table-data repo-ptr entries-sha)
      entries-sha)))

(defun %cid-detail-table-object (repo-ptr entries-sha)
  (make-instance
   'cid-detail-table
   :sha (%make-cid-detail-table-root repo-ptr entries-sha)))

(defun make-cid-detail-table (&key (size 16))
  (let ((entries
          (make-persistent-hash-table
           :test 'equal :size size)))
    (with-repository ()
      (%cid-detail-table-object
       (current-repository)
       (persistent-hash-table-sha entries)))))

(defun cid-detail-table-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-detail-table-entries-sha (current-repository) sha)
    (make-instance 'cid-detail-table :sha sha)))

(defun cid-detail-table/entries (table)
  (check-type table cid-detail-table)
  (with-repository ()
    (make-instance
     'persistent-hash-table
     :sha (%cid-detail-table-entries-sha
           (current-repository)
           (cid-detail-table-sha table)))))

(defun cid-detail-table/find-entry (table object-changed)
  (multiple-value-bind (entry-sha present-p)
      (persistent-gethash
       (%cid-detail-object-key object-changed)
       (cid-detail-table/entries table))
    (and present-p
         (cid-detail-table-entry-from-sha entry-sha))))

(defun %persistent-vector-contains-p (vector item)
  (loop for element across (persistent-vector->vector vector)
        thereis (eql element item)))

(defun cid-detail-table/log-change
    (table object-changed slot-identifier)
  "Returns an updated detail table and the affected entry as two values."
  (check-type table cid-detail-table)
  (check-type slot-identifier symbol)
  (let* ((entries (cid-detail-table/entries table))
         (entry (cid-detail-table/find-entry table object-changed))
         (slots
           (if entry
               (cid-detail-table-entry/slots-modified entry)
               (make-persistent-vector 0))))
    (if (%persistent-vector-contains-p slots slot-identifier)
        (values table entry)
        (multiple-value-bind (new-slots index)
            (persistent-vector-push-extend slot-identifier slots)
          (declare (ignore index))
          (with-repository ()
            (let* ((repo-ptr (current-repository))
                   (new-entry
                     (%cid-detail-table-entry-object
                      repo-ptr object-changed
                      (persistent-vector-sha new-slots)))
                   (new-entries
                     (persistent-hash-table-set
                      entries
                      (%cid-detail-object-key object-changed)
                      (cid-detail-table-entry-sha new-entry)))
                   (new-table
                     (%cid-detail-table-object
                      repo-ptr
                      (persistent-hash-table-sha new-entries))))
              (values new-table new-entry)))))))
