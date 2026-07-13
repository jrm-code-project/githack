;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(declaim
 (ftype function
        distributed-object-sha distributed-object-from-sha))

(defclass mapper ()
  ((sha :initarg :sha :reader mapper-sha :type string)))

(defclass unordered-mapper (mapper) ())
(defclass ordered-mapper (mapper) ())

(defstruct (mapper-data
             (:constructor make-mapper-data
                 (type mapping-level key prefix-sha entries-sha)))
  type
  mapping-level
  key
  prefix-sha
  entries-sha)

(defun %mapper-type-class (type)
  (ecase type
    (:unordered 'unordered-mapper)
    (:ordered 'ordered-mapper)))

(defun %mapper-value-reference (value)
  (cond
    ((and (find-class 'distributed-object nil)
          (typep value 'distributed-object))
     (list :distributed-object (distributed-object-sha value)))
    (t
     (typecase value
    (mapper
     (list :mapper (mapper-sha value)))
    (distributed-identifier
     (list :distributed-identifier (canonical-identifier-sha value)))
    (cid-object
     (list :cid-object (cid-object-sha value)))
    (cid-set
     (list :cid-set (cid-set-sha value)))
    (persistent-vector
     (list :persistent-vector (persistent-vector-sha value)))
    (persistent-hash-table
     (list :persistent-hash-table
           (persistent-hash-table-sha value)))
       (t (list :value value))))))

(defun %mapper-reference-value (reference)
  (unless (and (consp reference)
               (consp (cdr reference))
               (null (cddr reference)))
    (error "Invalid mapper value reference ~S." reference))
  (ecase (first reference)
    (:mapper (mapper-from-sha (second reference)))
    (:distributed-identifier
     (distributed-identifier-from-sha (second reference)))
    (:cid-object (cid-object-from-sha (second reference)))
    (:distributed-object
     (distributed-object-from-sha (second reference)))
    (:cid-set (cid-set-from-sha (second reference)))
    (:persistent-vector
     (persistent-vector-from-sha (second reference)))
    (:persistent-hash-table
     (persistent-hash-table-from-sha (second reference)))
    (:value (second reference))))

(defun %make-mapper-root
    (repo-ptr type mapping-level key prefix-sha entries-sha)
  (create-tree
   repo-ptr
   (list
    (list "entries" entries-sha +git-filemode-tree+)
    (list "key" (%stored-object repo-ptr key) +git-filemode-blob+)
    (list "level" (%stored-object repo-ptr mapping-level)
          +git-filemode-blob+)
    (list "prefix" prefix-sha +git-filemode-tree+)
    (list "type" (%stored-object repo-ptr type)
          +git-filemode-blob+))))

(defun %mapper-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 5)
                 (equal (mapcar #'first entries)
                        '("entries" "key" "level" "prefix" "type"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (second entries)) +git-filemode-blob+)
                 (= (third (third entries)) +git-filemode-blob+)
                 (= (third (fourth entries)) +git-filemode-tree+)
                 (= (third (fifth entries)) +git-filemode-blob+))
      (error "Git tree ~S is not a mapper." sha))
    (let* ((type (%loaded-object repo-ptr (second (fifth entries))))
           (prefix-sha (second (fourth entries)))
           (entries-sha (second (first entries))))
      (%mapper-type-class type)
      (%canonical-identifier-components repo-ptr prefix-sha)
      (ecase type
        (:unordered
         (%persistent-hash-table-data repo-ptr entries-sha))
        (:ordered
         (%persistent-vector-data repo-ptr entries-sha)))
      (make-mapper-data
       type
       (%loaded-object repo-ptr (second (third entries)))
       (%loaded-object repo-ptr (second (second entries)))
       prefix-sha entries-sha))))

(defun %mapper-object
    (repo-ptr type mapping-level key prefix-sha entries-sha)
  (make-instance
   (%mapper-type-class type)
   :sha (%make-mapper-root
         repo-ptr type mapping-level key prefix-sha entries-sha)))

(defun mapper-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (let ((data (%mapper-data (current-repository) sha)))
      (make-instance
       (%mapper-type-class (mapper-data-type data))
       :sha sha))))

(defun mapper/mapping-level (mapper)
  (check-type mapper mapper)
  (with-repository ()
    (mapper-data-mapping-level
     (%mapper-data (current-repository) (mapper-sha mapper)))))

(defun mapper/key (mapper)
  (check-type mapper mapper)
  (with-repository ()
    (mapper-data-key
     (%mapper-data (current-repository) (mapper-sha mapper)))))

(defun mapper/prefix (mapper)
  (check-type mapper mapper)
  (with-repository ()
    (let ((data (%mapper-data
                 (current-repository) (mapper-sha mapper))))
      (canonical-identifier-from-sha
       (mapper-data-prefix-sha data)))))

(defun mapper/parent (mapper)
  "Returns the canonical prefix of the conceptual parent mapper."
  (let ((components
          (canonical-identifier-components (mapper/prefix mapper))))
    (and components
         (make-canonical-identifier (butlast components)))))

(defun %mapper-prefix-for-child (parent key)
  (append
   (if parent
       (canonical-identifier-components (mapper/prefix parent))
       nil)
   (if key (list key) nil)))

(defun make-unordered-mapper
    (&key (mapping-level "Unknown") key parent (size 16)
          (test 'equal))
  (let* ((prefix
           (make-canonical-identifier
            (%mapper-prefix-for-child parent key)))
         (entries
           (make-persistent-hash-table :size size :test test)))
    (with-repository ()
      (%mapper-object
       (current-repository) :unordered mapping-level key
       (canonical-identifier-sha prefix)
       (persistent-hash-table-sha entries)))))

(defun make-ordered-mapper
    (&key (mapping-level "Unknown") key parent)
  (let* ((prefix
           (make-canonical-identifier
            (%mapper-prefix-for-child parent key)))
         (entries (make-persistent-vector 1 :initial-element nil)))
    (with-repository ()
      (%mapper-object
       (current-repository) :ordered mapping-level key
       (canonical-identifier-sha prefix)
       (persistent-vector-sha entries)))))

(defun unordered-mapper/hash-table (mapper)
  (check-type mapper unordered-mapper)
  (with-repository ()
    (make-instance
     'persistent-hash-table
     :sha
     (mapper-data-entries-sha
      (%mapper-data
       (current-repository) (mapper-sha mapper))))))

(defun ordered-mapper/instance-vector (mapper)
  (check-type mapper ordered-mapper)
  (with-repository ()
    (make-instance
     'persistent-vector
     :sha
     (mapper-data-entries-sha
      (%mapper-data
       (current-repository) (mapper-sha mapper))))))

(defgeneric mapper/resolve (mapper key))

(defmethod mapper/resolve ((mapper unordered-mapper) key)
  (multiple-value-bind (reference present-p)
      (persistent-gethash key (unordered-mapper/hash-table mapper))
    (and present-p (%mapper-reference-value reference))))

(defun unordered-mapper/set-entry (mapper key value)
  "Returns an unordered mapper with KEY mapped to VALUE."
  (check-type mapper unordered-mapper)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data (%mapper-data repo-ptr (mapper-sha mapper)))
           (entries
             (persistent-hash-table-set
              (unordered-mapper/hash-table mapper)
              key (%mapper-value-reference value))))
      (%mapper-object
       repo-ptr :unordered
       (mapper-data-mapping-level data)
       (mapper-data-key data)
       (mapper-data-prefix-sha data)
       (persistent-hash-table-sha entries)))))

(defmethod mapper/resolve ((mapper ordered-mapper) (key integer))
  (let ((vector (ordered-mapper/instance-vector mapper)))
    (when (and (>= key 0)
               (< key (persistent-vector-length vector)))
      (let ((reference (persistent-vector-ref vector key)))
        (and reference (%mapper-reference-value reference))))))

(defgeneric mapper/install-child-mapper (parent child))

(defmethod mapper/install-child-mapper
    ((parent unordered-mapper) (child mapper))
  (let ((key (mapper/key child)))
    (when (null key)
      (error "A child mapper requires a key."))
    (when (mapper/resolve parent key)
      (error "Mapper already contains child key ~S." key))
    (unless
        (equal
         (canonical-identifier-components (mapper/prefix child))
         (%mapper-prefix-for-child parent key))
      (error "Child mapper prefix does not match its parent and key."))
    (with-repository ()
      (let* ((repo-ptr (current-repository))
             (data (%mapper-data repo-ptr (mapper-sha parent)))
             (entries
               (persistent-hash-table-set
                (unordered-mapper/hash-table parent)
                key (%mapper-value-reference child))))
        (%mapper-object
         repo-ptr :unordered
         (mapper-data-mapping-level data)
         (mapper-data-key data)
         (mapper-data-prefix-sha data)
         (persistent-hash-table-sha entries))))))

(defmethod mapper/install-child-mapper
    ((parent ordered-mapper) (child mapper))
  (declare (ignore child))
  (error "Ordered mappers cannot contain child mappers."))

(defun ordered-mapper/reserve-entry (mapper)
  "Returns the updated mapper and reserved positive index."
  (check-type mapper ordered-mapper)
  (let* ((vector (ordered-mapper/instance-vector mapper))
         (index (persistent-vector-length vector)))
    (multiple-value-bind (new-vector appended-index)
        (persistent-vector-push-extend nil vector)
      (assert (= index appended-index))
      (with-repository ()
        (let* ((repo-ptr (current-repository))
               (data (%mapper-data repo-ptr (mapper-sha mapper))))
          (values
           (%mapper-object
            repo-ptr :ordered
            (mapper-data-mapping-level data)
            (mapper-data-key data)
            (mapper-data-prefix-sha data)
            (persistent-vector-sha new-vector))
           index))))))

(defun ordered-mapper/set-entry (mapper numeric-id value)
  (check-type mapper ordered-mapper)
  (check-type numeric-id (integer 1 *))
  (let ((vector (ordered-mapper/instance-vector mapper)))
    (when (>= numeric-id (persistent-vector-length vector))
      (error "Ordered mapper index ~D has not been reserved."
             numeric-id))
    (let ((new-vector
            (persistent-vector-update
             vector numeric-id (%mapper-value-reference value))))
      (with-repository ()
        (let* ((repo-ptr (current-repository))
               (data (%mapper-data repo-ptr (mapper-sha mapper))))
          (%mapper-object
           repo-ptr :ordered
           (mapper-data-mapping-level data)
           (mapper-data-key data)
           (mapper-data-prefix-sha data)
           (persistent-vector-sha new-vector)))))))

(defun mapper-distributed-identifier (mapper)
  (let ((components
          (canonical-identifier-components (mapper/prefix mapper))))
    (destructuring-bind
        (&optional domain repository class numeric-id)
        components
      (make-distributed-identifier
       :domain domain
       :repository repository
       :class class
       :numeric-id (or numeric-id 0)))))

(defun distributed-identifier-create-mapper-hierarchy (did)
  (check-type did distributed-identifier)
  (let* ((root
           (make-unordered-mapper
            :mapping-level "Root" :test 'equal))
         (domain
           (make-unordered-mapper
            :mapping-level "Domain"
            :key (did/domain did)
            :parent root :test 'equal))
         (repository
           (make-unordered-mapper
            :mapping-level "Repository"
            :key (did/repository did)
            :parent domain :test 'equal))
         (updated-domain
           (mapper/install-child-mapper domain repository))
         (updated-root
           (mapper/install-child-mapper root updated-domain)))
    (values updated-root repository)))

(defun distributed-identifier/resolve
    (did root &key repository-only (error-if-missing t))
  (check-type did distributed-identifier)
  (check-type root mapper)
  (labels ((missing (component)
             (when error-if-missing
               (error "DID resolution failed for ~A in ~S."
                      component did))))
    (let ((domain (mapper/resolve root (did/domain did))))
      (if (null domain)
          (missing "domain")
          (let ((repository
                  (mapper/resolve domain (did/repository did))))
            (cond
              ((null repository) (missing "repository"))
              (repository-only repository)
              ((null (did/class did)) repository)
              (t
               (let ((class
                       (mapper/resolve repository (did/class did))))
                 (if (null class)
                     (missing "class")
                     (or
                      (mapper/resolve class (did/numeric-id did))
                      (missing "numeric ID")))))))))))
