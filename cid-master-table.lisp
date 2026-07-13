;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass cid-master-table-entry ()
  ((sha :initarg :sha :reader cid-master-table-entry-sha :type string)))

(defclass cid-master-table ()
  ((sha :initarg :sha :reader cid-master-table-sha :type string)))

(defstruct (cid-master-table-entry-data
             (:constructor make-cid-master-table-entry-data
                 (cid detail-sha basis-sha who start finish reason
                  change-information)))
  cid detail-sha basis-sha who start finish reason change-information)

(defun %master-value-reference (value)
  (%mapper-value-reference value))

(defun %master-reference-value (reference)
  (%mapper-reference-value reference))

(defun %make-cid-master-table-entry-root
    (repo-ptr cid detail-sha basis-sha who start finish reason
     change-information)
  (%check-version-cid cid)
  (%cid-detail-table-entries-sha repo-ptr detail-sha)
  (%cid-set-data repo-ptr basis-sha)
  (check-type start integer)
  (check-type finish (or null integer))
  (check-type reason string)
  (create-tree
   repo-ptr
   (list
    (list "basis" basis-sha +git-filemode-tree+)
    (list "change"
          (%stored-object
           repo-ptr (%master-value-reference change-information))
          +git-filemode-blob+)
    (list "cid" (%stored-object repo-ptr cid)
          +git-filemode-blob+)
    (list "detail" detail-sha +git-filemode-tree+)
    (list "finish" (%stored-object repo-ptr finish)
          +git-filemode-blob+)
    (list "reason" (%stored-object repo-ptr reason)
          +git-filemode-blob+)
    (list "start" (%stored-object repo-ptr start)
          +git-filemode-blob+)
    (list "who"
          (%stored-object repo-ptr (%master-value-reference who))
          +git-filemode-blob+))))

(defun %cid-master-table-entry-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 8)
                 (equal (mapcar #'first entries)
                        '("basis" "change" "cid" "detail"
                          "finish" "reason" "start" "who"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (fourth entries)) +git-filemode-tree+)
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  (list (second entries) (third entries)
                        (fifth entries) (sixth entries)
                        (seventh entries) (eighth entries))))
      (error "Git tree ~S is not a CID master table entry." sha))
    (let ((basis-sha (second (first entries)))
          (detail-sha (second (fourth entries)))
          (cid (%loaded-object repo-ptr (second (third entries))))
          (finish (%loaded-object repo-ptr (second (fifth entries))))
          (reason (%loaded-object repo-ptr (second (sixth entries))))
          (start (%loaded-object repo-ptr (second (seventh entries)))))
      (%check-version-cid cid)
      (%cid-set-data repo-ptr basis-sha)
      (%cid-detail-table-entries-sha repo-ptr detail-sha)
      (check-type start integer)
      (check-type finish (or null integer))
      (check-type reason string)
      (make-cid-master-table-entry-data
       cid detail-sha basis-sha
       (%master-reference-value
        (%loaded-object repo-ptr (second (eighth entries))))
       start finish reason
       (%master-reference-value
        (%loaded-object repo-ptr (second (second entries))))))))

(defun %cid-master-table-entry-object
    (repo-ptr cid detail-sha basis-sha who start finish reason
     change-information)
  (make-instance
   'cid-master-table-entry
   :sha (%make-cid-master-table-entry-root
         repo-ptr cid detail-sha basis-sha who start finish reason
         change-information)))

(defun make-cid-master-table-entry
    (&key cid cid-set-basis who why
          (when-start (get-universal-time))
          when-finish versioned-change-information
          cid-detail-table)
  (%check-version-cid cid)
  (check-type cid-set-basis cid-set)
  (check-type why string)
  (let ((detail
          (or cid-detail-table (make-cid-detail-table))))
    (check-type detail cid-detail-table)
    (with-repository ()
      (%cid-master-table-entry-object
       (current-repository) cid
       (cid-detail-table-sha detail)
       (cid-set-sha cid-set-basis)
       who when-start when-finish why
       versioned-change-information))))

(defun cid-master-table-entry-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-master-table-entry-data (current-repository) sha)
    (make-instance 'cid-master-table-entry :sha sha)))

(defmacro define-cid-master-entry-reader (name accessor)
  `(defun ,name (entry)
     (check-type entry cid-master-table-entry)
     (with-repository ()
       (,accessor
        (%cid-master-table-entry-data
         (current-repository)
         (cid-master-table-entry-sha entry))))))

(define-cid-master-entry-reader
 cid-master-table-entry/cid
 cid-master-table-entry-data-cid)
(define-cid-master-entry-reader
 cid-master-table-entry/who
 cid-master-table-entry-data-who)
(define-cid-master-entry-reader
 cid-master-table-entry/when-start
 cid-master-table-entry-data-start)
(define-cid-master-entry-reader
 cid-master-table-entry/when-finish
 cid-master-table-entry-data-finish)
(define-cid-master-entry-reader
 cid-master-table-entry/why
 cid-master-table-entry-data-reason)
(define-cid-master-entry-reader
 cid-master-table-entry/versioned-change-information
 cid-master-table-entry-data-change-information)

(defun cid-master-table-entry/cid-detail-table (entry)
  (check-type entry cid-master-table-entry)
  (with-repository ()
    (make-instance
     'cid-detail-table
     :sha
     (cid-master-table-entry-data-detail-sha
      (%cid-master-table-entry-data
       (current-repository)
       (cid-master-table-entry-sha entry))))))

(defun cid-master-table-entry/cid-set-basis (entry)
  (check-type entry cid-master-table-entry)
  (with-repository ()
    (make-instance
     'cid-set
     :sha
     (cid-master-table-entry-data-basis-sha
      (%cid-master-table-entry-data
       (current-repository)
       (cid-master-table-entry-sha entry))))))

(defun cid-master-table-entry/comparison-timestamp (entry)
  (or (cid-master-table-entry/when-finish entry)
      (cid-master-table-entry/when-start entry)))

(defun %remake-cid-master-table-entry
    (entry &key detail-sha
                (finish nil finish-supplied-p)
                (reason nil reason-supplied-p)
                (change nil change-supplied-p))
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data (%cid-master-table-entry-data
                  repo-ptr (cid-master-table-entry-sha entry))))
      (%cid-master-table-entry-object
       repo-ptr
       (cid-master-table-entry-data-cid data)
       (or detail-sha
           (cid-master-table-entry-data-detail-sha data))
       (cid-master-table-entry-data-basis-sha data)
       (cid-master-table-entry-data-who data)
       (cid-master-table-entry-data-start data)
       (if finish-supplied-p finish
           (cid-master-table-entry-data-finish data))
       (if reason-supplied-p reason
           (cid-master-table-entry-data-reason data))
       (if change-supplied-p change
           (cid-master-table-entry-data-change-information data))))))

(defun cid-master-table-entry/log-change
    (entry changed-object slot-identifier)
  (multiple-value-bind (detail-table detail-entry)
      (cid-detail-table/log-change
       (cid-master-table-entry/cid-detail-table entry)
       changed-object slot-identifier)
    (values
     (%remake-cid-master-table-entry
      entry :detail-sha (cid-detail-table-sha detail-table))
     detail-entry)))

(defun cid-master-table-entry/note-finish
    (entry reason change-information
     &optional (finish-time (get-universal-time)))
  (check-type reason string)
  (check-type finish-time integer)
  (%remake-cid-master-table-entry
   entry :finish finish-time :reason reason
   :change change-information))

(defun %make-cid-master-table-root
    (repo-ptr entries-sha cid-objects-sha)
  (%persistent-vector-data repo-ptr entries-sha)
  (%persistent-vector-data repo-ptr cid-objects-sha)
  (create-tree
   repo-ptr
   (list
    (list "cid-objects" cid-objects-sha +git-filemode-tree+)
    (list "entries" entries-sha +git-filemode-tree+))))

(defun %cid-master-table-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("cid-objects" "entries"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-tree+))
                  entries))
      (error "Git tree ~S is not a CID master table." sha))
    (let ((cid-objects-sha (second (first entries)))
          (entries-sha (second (second entries))))
      (%persistent-vector-data repo-ptr entries-sha)
      (%persistent-vector-data repo-ptr cid-objects-sha)
      (values entries-sha cid-objects-sha))))

(defun %cid-master-table-object
    (repo-ptr entries-sha cid-objects-sha)
  (make-instance
   'cid-master-table
   :sha (%make-cid-master-table-root
         repo-ptr entries-sha cid-objects-sha)))

(defun make-cid-master-table ()
  (let ((entries (make-persistent-vector 1 :initial-element nil))
        (cid-objects
          (make-persistent-vector 1 :initial-element nil)))
    (with-repository ()
      (%cid-master-table-object
       (current-repository)
       (persistent-vector-sha entries)
       (persistent-vector-sha cid-objects)))))

(defun cid-master-table-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-master-table-data (current-repository) sha)
    (make-instance 'cid-master-table :sha sha)))

(defun cid-master-table/entries (table)
  (check-type table cid-master-table)
  (with-repository ()
    (multiple-value-bind (entries-sha cid-objects-sha)
        (%cid-master-table-data
         (current-repository) (cid-master-table-sha table))
      (declare (ignore cid-objects-sha))
      (make-instance 'persistent-vector :sha entries-sha))))

(defun cid-master-table/cid-objects-by-cid (table)
  (check-type table cid-master-table)
  (with-repository ()
    (multiple-value-bind (entries-sha cid-objects-sha)
        (%cid-master-table-data
         (current-repository) (cid-master-table-sha table))
      (declare (ignore entries-sha))
      (make-instance 'persistent-vector :sha cid-objects-sha))))

(defun %persistent-vector-grow-to (vector length)
  (loop with result = vector
        while (< (persistent-vector-length result) length)
        do (setf result
                 (nth-value
                  0 (persistent-vector-push-extend nil result)))
        finally (return result)))

(defun %cid-master-table-with-vectors
    (table entries cid-objects)
  (declare (ignore table))
  (with-repository ()
    (%cid-master-table-object
     (current-repository)
     (persistent-vector-sha entries)
     (persistent-vector-sha cid-objects))))

(defun cid-master-table/add-entry (table entry)
  (check-type table cid-master-table)
  (check-type entry cid-master-table-entry)
  (let* ((cid (cid-master-table-entry/cid entry))
         (entries
           (%persistent-vector-grow-to
            (cid-master-table/entries table) (1+ cid)))
         (new-entries
           (persistent-vector-update
            entries cid (cid-master-table-entry-sha entry))))
    (%cid-master-table-with-vectors
     table new-entries
     (cid-master-table/cid-objects-by-cid table))))

(defun cid-master-table/set-cid-object
    (table cid cid-object)
  (%check-version-cid cid)
  (check-type table cid-master-table)
  (check-type cid-object cid-object)
  (let* ((cid-objects
           (%persistent-vector-grow-to
            (cid-master-table/cid-objects-by-cid table)
            (1+ cid)))
         (new-cid-objects
           (persistent-vector-update
            cid-objects cid (cid-object-sha cid-object))))
    (%cid-master-table-with-vectors
     table (cid-master-table/entries table)
     new-cid-objects)))

(defun cid-master-table/cid-object (table cid)
  (%check-version-cid cid)
  (let ((vector (cid-master-table/cid-objects-by-cid table)))
    (when (< cid (persistent-vector-length vector))
      (let ((sha (persistent-vector-ref vector cid)))
        (and sha (cid-object-from-sha sha))))))

(defun cid-master-table/entry-for-cid
    (table cid &key missing-cid-okay)
  (%check-version-cid cid)
  (let ((entries (cid-master-table/entries table)))
    (let ((sha
            (and (< cid (persistent-vector-length entries))
                 (persistent-vector-ref entries cid))))
      (cond
        (sha (cid-master-table-entry-from-sha sha))
        (missing-cid-okay nil)
        (t (error "No CID master table entry for CID ~D." cid))))))

(defun cid-master-table/last-allocated-cid (table)
  (let ((entries (cid-master-table/entries table)))
    (loop for cid downfrom
          (1- (persistent-vector-length entries)) to 1
          when (persistent-vector-ref entries cid)
            return cid
          finally (return 0))))

(defun cid-master-table/contains-cid? (table cid)
  (and (typep cid '(integer 1 *))
       (not
        (null
         (cid-master-table/entry-for-cid
          table cid :missing-cid-okay t)))))

(defun cid-master-table/cid-comparison-timestamp (table cid)
  (let ((entry
          (cid-master-table/entry-for-cid
           table cid :missing-cid-okay t)))
    (and entry
         (cid-master-table-entry/comparison-timestamp entry))))

(defun cid-master-table/cid-information (table cid)
  (let ((entry (cid-master-table/entry-for-cid table cid)))
    (values
     (cid-master-table-entry/why entry)
     (cid-master-table-entry/comparison-timestamp entry)
     (cid-master-table-entry/versioned-change-information entry)
     (cid-master-table-entry/cid-set-basis entry))))

(defun cid-master-table/cid-versioned-change-information
    (table cid)
  (cid-master-table-entry/versioned-change-information
   (cid-master-table/entry-for-cid table cid)))

(defun cid-master-table/active-cids
    (repository table &key end-time)
  (check-type end-time (or null integer))
  (let ((result (cid-set/empty repository))
        (last (cid-master-table/last-allocated-cid table)))
    (loop for cid from 1 to last
          for entry = (cid-master-table/entry-for-cid
                       table cid :missing-cid-okay t)
          when (and entry
                    (or (null end-time)
                        (<=
                         (cid-master-table-entry/comparison-timestamp
                          entry)
                         end-time)))
            do (setf result (cid-set/adjoin result cid)))
    result))
