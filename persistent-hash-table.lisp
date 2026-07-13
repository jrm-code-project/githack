;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass persistent-hash-table ()
  ((sha :initarg :sha :reader persistent-hash-table-sha :type string)))

(defstruct (persistent-hash-table-data
             (:constructor make-persistent-hash-table-data
                 (buckets-sha metadata-sha count test)))
  buckets-sha
  metadata-sha
  count
  test)

(defun %canonical-persistent-hash-test (test)
  (case test
    ((eql :eql) 'eql)
    ((equal :equal) 'equal)
    ((equalp :equalp) 'equalp)
    (otherwise
     (error "Persistent hash table test must be EQL, EQUAL, or EQUALP, not ~S."
            test))))

(defun %persistent-hash-test-function (test)
  (ecase test
    (eql #'eql)
    (equal #'equal)
    (equalp #'equalp)))

(defun %persistent-hash-index (key test size)
  ;; SXHASH is compatible with EQUAL. EQUALP has no corresponding portable
  ;; hash function, so a single collision chain preserves correct semantics.
  (mod (if (eq test 'equalp) 0 (sxhash key)) size))

(defun %make-persistent-hash-metadata (repo-ptr count test)
  (create-tree
   repo-ptr
   (list
    (list "count"
          (%stored-object repo-ptr count)
          +git-filemode-blob+)
    (list "test"
          (%stored-object repo-ptr test)
          +git-filemode-blob+))))

(defun %persistent-hash-metadata (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries) '("count" "test"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  entries))
      (error "Git tree ~S is not persistent hash table metadata." sha))
    (let ((count (%loaded-object repo-ptr (second (first entries))))
          (test
            (%canonical-persistent-hash-test
             (%loaded-object repo-ptr (second (second entries))))))
      (check-type count (integer 0 *))
      (values count test))))

(defun %persistent-hash-table-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("buckets" "metadata"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-tree+))
                  entries))
      (error "Git tree ~S is not a persistent hash table." sha))
    (let ((buckets-sha (second (first entries)))
          (metadata-sha (second (second entries))))
      (unless
          (plusp
           (persistent-vector-data-length
            (%persistent-vector-data repo-ptr buckets-sha)))
        (error "Persistent hash table ~S has no buckets." sha))
      (multiple-value-bind (count test)
          (%persistent-hash-metadata repo-ptr metadata-sha)
        (make-persistent-hash-table-data
         buckets-sha metadata-sha count test)))))

(defun %make-persistent-hash-root
    (repo-ptr buckets-sha count test)
  (let ((metadata-sha
          (%make-persistent-hash-metadata repo-ptr count test)))
    (create-tree
     repo-ptr
     (list (list "buckets" buckets-sha +git-filemode-tree+)
           (list "metadata" metadata-sha +git-filemode-tree+)))))

(defun %persistent-hash-object
    (repo-ptr buckets-sha count test)
  (make-instance
   'persistent-hash-table
   :sha (%make-persistent-hash-root
         repo-ptr buckets-sha count test)))

(defun %persistent-vector-value (repo-ptr vector-data index)
  (let ((node-sha
          (%persistent-node-find
           repo-ptr #'<
           (persistent-vector-data-contents-sha vector-data)
           index)))
    (unless node-sha
      (error "Persistent bucket vector has no bucket at index ~D." index))
    (persistent-node-data-value
     (%persistent-node-data repo-ptr node-sha))))

(defun %persistent-vector-replace
    (repo-ptr vector-data index value)
  (%persistent-vector-object
   repo-ptr
   (%persistent-node-add
    repo-ptr #'<
    (persistent-vector-data-contents-sha vector-data)
    index value)
   (persistent-vector-data-length vector-data)))

(defun %persistent-bucket-entry (repo-ptr sha)
  (let* ((entries (persistent-cons-entries repo-ptr sha))
         (entry
           (%loaded-object repo-ptr (second (first entries))))
         (next (second (second entries))))
    (unless (consp entry)
      (error "Persistent hash bucket cell ~S does not contain a key/value pair."
             sha))
    (values entry next)))

(defun %persistent-bucket-get (repo-ptr sha key test)
  (let ((null-sha (%persistent-null repo-ptr))
        (predicate (%persistent-hash-test-function test)))
    (loop until (string-equal sha null-sha)
          do (multiple-value-bind (entry next)
                 (%persistent-bucket-entry repo-ptr sha)
               (when (funcall predicate key (car entry))
                 (return (values (cdr entry) t)))
               (setf sha next))
          finally (return (values nil nil)))))

(defun %persistent-bucket-set
    (repo-ptr sha key value test)
  (if (string-equal sha (%persistent-null repo-ptr))
      (values (%persistent-cons
               repo-ptr (cons key value) sha)
              t)
      (multiple-value-bind (entry next)
          (%persistent-bucket-entry repo-ptr sha)
        (if (funcall (%persistent-hash-test-function test)
                     key (car entry))
            (values
             (%persistent-cons
              repo-ptr (cons (car entry) value) next)
             nil)
            (multiple-value-bind (new-next inserted-p)
                (%persistent-bucket-set
                 repo-ptr next key value test)
              (values
               (%persistent-cons repo-ptr entry new-next)
               inserted-p))))))

(defun %persistent-bucket-remove (repo-ptr sha key test)
  (if (string-equal sha (%persistent-null repo-ptr))
      (values sha nil)
      (multiple-value-bind (entry next)
          (%persistent-bucket-entry repo-ptr sha)
        (if (funcall (%persistent-hash-test-function test)
                     key (car entry))
            (values next t)
            (multiple-value-bind (new-next removed-p)
                (%persistent-bucket-remove
                 repo-ptr next key test)
              (if removed-p
                  (values
                   (%persistent-cons repo-ptr entry new-next)
                   t)
                  (values sha nil)))))))

(defun %make-persistent-bucket-vector (repo-ptr size)
  (check-type size (integer 1 *))
  (let ((contents-sha (%persistent-node-null-sha repo-ptr))
        (empty-bucket (%persistent-null repo-ptr)))
    (dotimes (index size)
      (setf contents-sha
            (%persistent-node-add
             repo-ptr #'< contents-sha index empty-bucket)))
    (%persistent-vector-object repo-ptr contents-sha size)))

(defun make-persistent-hash-table
    (&key (test 'eql) (size 16) initial-contents)
  "Creates an immutable persistent hash table."
  (check-type size (integer 1 *))
  (let ((test (%canonical-persistent-hash-test test)))
    (with-repository ()
      (let* ((repo-ptr (current-repository))
             (buckets (%make-persistent-bucket-vector repo-ptr size))
             (table
               (%persistent-hash-object
                repo-ptr (persistent-vector-sha buckets) 0 test)))
        (dolist (entry initial-contents table)
          (unless (consp entry)
            (error "Expected an alist entry, got ~S." entry))
          (setf table
                (persistent-hash-table-set
                 table (car entry) (cdr entry))))))))

(defun %persistent-hash-entry-count (repo-ptr data)
  (let* ((vector-data
           (%persistent-vector-data
            repo-ptr
            (persistent-hash-table-data-buckets-sha data)))
         (bucket-count (persistent-vector-data-length vector-data))
         (null-sha (%persistent-null repo-ptr))
         (count 0))
    (dotimes (index bucket-count count)
      (let ((bucket
              (%persistent-vector-value
               repo-ptr vector-data index)))
        (loop until (string-equal bucket null-sha)
              do (multiple-value-bind (entry next)
                     (%persistent-bucket-entry repo-ptr bucket)
                   (declare (ignore entry))
                   (incf count)
                   (setf bucket next)))))))

(defun persistent-hash-table-from-sha (sha)
  "Validates SHA and returns its persistent hash table wrapper."
  (check-type sha string)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data (%persistent-hash-table-data repo-ptr sha))
           (actual-count (%persistent-hash-entry-count repo-ptr data)))
      (unless (= actual-count
                 (persistent-hash-table-data-count data))
        (error "Persistent hash table ~S records ~D entries but contains ~D."
               sha
               (persistent-hash-table-data-count data)
               actual-count))
      (make-instance 'persistent-hash-table :sha sha))))

(defun persistent-hash-table-count (table)
  (check-type table persistent-hash-table)
  (with-repository ()
    (persistent-hash-table-data-count
     (%persistent-hash-table-data
      (current-repository)
      (persistent-hash-table-sha table)))))

(defun persistent-hash-table-test (table)
  (check-type table persistent-hash-table)
  (with-repository ()
    (persistent-hash-table-data-test
     (%persistent-hash-table-data
      (current-repository)
      (persistent-hash-table-sha table)))))

(defun persistent-hash-table-buckets-sha (table)
  (check-type table persistent-hash-table)
  (with-repository ()
    (persistent-hash-table-data-buckets-sha
     (%persistent-hash-table-data
      (current-repository)
      (persistent-hash-table-sha table)))))

(defun persistent-hash-table-buckets (table)
  (make-instance
   'persistent-vector
   :sha (persistent-hash-table-buckets-sha table)))

(defun persistent-hash-table-size (table)
  (persistent-vector-length
   (persistent-hash-table-buckets table)))

(defun persistent-hash-table-get (table key &optional default)
  "Returns the value and a presence flag, like GETHASH."
  (check-type table persistent-hash-table)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-hash-table-data
              repo-ptr (persistent-hash-table-sha table)))
           (vector-data
             (%persistent-vector-data
              repo-ptr
              (persistent-hash-table-data-buckets-sha data)))
           (size (persistent-vector-data-length vector-data))
           (index
             (%persistent-hash-index
              key
              (persistent-hash-table-data-test data)
              size))
           (bucket
             (%persistent-vector-value
              repo-ptr vector-data index)))
      (multiple-value-bind (value present-p)
          (%persistent-bucket-get
           repo-ptr bucket key
           (persistent-hash-table-data-test data))
        (values (if present-p value default) present-p)))))

(defun persistent-gethash (key table &optional default)
  (persistent-hash-table-get table key default))

(defun persistent-hash-table-set (table key value)
  "Returns a new table mapping KEY to VALUE."
  (check-type table persistent-hash-table)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-hash-table-data
              repo-ptr (persistent-hash-table-sha table)))
           (vector-data
             (%persistent-vector-data
              repo-ptr
              (persistent-hash-table-data-buckets-sha data)))
           (size (persistent-vector-data-length vector-data))
           (index
             (%persistent-hash-index
              key
              (persistent-hash-table-data-test data)
              size))
           (bucket
             (%persistent-vector-value
              repo-ptr vector-data index)))
      (multiple-value-bind (new-bucket inserted-p)
          (%persistent-bucket-set
           repo-ptr bucket key value
           (persistent-hash-table-data-test data))
        (let ((new-buckets
                (%persistent-vector-replace
                 repo-ptr vector-data index new-bucket)))
          (%persistent-hash-object
           repo-ptr
           (persistent-vector-sha new-buckets)
           (if inserted-p
               (1+ (persistent-hash-table-data-count data))
               (persistent-hash-table-data-count data))
           (persistent-hash-table-data-test data)))))))

(defun persistent-puthash (key value table)
  (persistent-hash-table-set table key value))

(defun persistent-hash-table-remove (table key)
  "Returns the resulting table and a flag indicating whether KEY was removed."
  (check-type table persistent-hash-table)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-hash-table-data
              repo-ptr (persistent-hash-table-sha table)))
           (vector-data
             (%persistent-vector-data
              repo-ptr
              (persistent-hash-table-data-buckets-sha data)))
           (size (persistent-vector-data-length vector-data))
           (index
             (%persistent-hash-index
              key
              (persistent-hash-table-data-test data)
              size))
           (bucket
             (%persistent-vector-value
              repo-ptr vector-data index)))
      (multiple-value-bind (new-bucket removed-p)
          (%persistent-bucket-remove
           repo-ptr bucket key
           (persistent-hash-table-data-test data))
        (if removed-p
            (let ((new-buckets
                    (%persistent-vector-replace
                     repo-ptr vector-data index new-bucket)))
              (values
               (%persistent-hash-object
                repo-ptr
                (persistent-vector-sha new-buckets)
                (1- (persistent-hash-table-data-count data))
                (persistent-hash-table-data-test data))
               t))
            (values table nil))))))

(defun persistent-remhash (key table)
  (persistent-hash-table-remove table key))

(defun persistent-hash-table->alist (table)
  (check-type table persistent-hash-table)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-hash-table-data
              repo-ptr (persistent-hash-table-sha table)))
           (vector-data
             (%persistent-vector-data
              repo-ptr
              (persistent-hash-table-data-buckets-sha data)))
           (size (persistent-vector-data-length vector-data))
           (null-sha (%persistent-null repo-ptr))
           (result nil))
      (dotimes (index size (nreverse result))
        (let ((bucket
                (%persistent-vector-value
                 repo-ptr vector-data index)))
          (loop until (string-equal bucket null-sha)
                do (multiple-value-bind (entry next)
                       (%persistent-bucket-entry repo-ptr bucket)
                     (push entry result)
                     (setf bucket next))))))))
