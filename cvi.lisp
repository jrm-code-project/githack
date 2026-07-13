;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass composite-set-versioned-value (versioned-value) ())
(defclass cvi (composite-set-versioned-value) ())

(defclass cvi-insertion-record ()
  ((sha :initarg :sha :reader cvi-insertion-record-sha :type string)))

(defclass cvi-deletion-record ()
  ((sha :initarg :sha :reader cvi-deletion-record-sha :type string)))

(defclass cvi-change-record ()
  ((sha :initarg :sha :reader cvi-change-record-sha :type string)))

(defstruct (cvi-data
             (:constructor make-cvi-data
                 (changes-sha default-allowed-p next-ion)))
  changes-sha default-allowed-p next-ion)

(defstruct (cvi-insertion-data
             (:constructor make-cvi-insertion-data
                 (start-ion insertion-point values-sha)))
  start-ion insertion-point values-sha)

(defstruct (cvi-deletion-data
             (:constructor make-cvi-deletion-data (start-ion limit-ion)))
  start-ion limit-ion)

(defstruct (cvi-change-data
             (:constructor make-cvi-change-data
                 (cid insertions-sha deletions-sha)))
  cid insertions-sha deletions-sha)

(defun %sequence->persistent-vector (sequence)
  (make-persistent-vector
   (length sequence) :initial-contents sequence))

(defun %sha-vector (objects sha-reader)
  (%sequence->persistent-vector (mapcar sha-reader objects)))

(defun %make-cvi-insertion-root
    (repo-ptr start-ion insertion-point values-sha)
  (check-type start-ion (integer 1 *))
  (check-type insertion-point (integer 0 *))
  (let ((values (%persistent-vector-data repo-ptr values-sha)))
    (when (zerop (persistent-vector-data-length values))
      (error "A CVI insertion record must contain at least one value.")))
  (create-tree
   repo-ptr
   (list
    (list "insertion-point"
          (%stored-object repo-ptr insertion-point)
          +git-filemode-blob+)
    (list "start-ion"
          (%stored-object repo-ptr start-ion)
          +git-filemode-blob+)
    (list "values" values-sha +git-filemode-tree+))))

(defun %cvi-insertion-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 3)
                 (equal (mapcar #'first entries)
                        '("insertion-point" "start-ion" "values"))
                 (= (third (first entries)) +git-filemode-blob+)
                 (= (third (second entries)) +git-filemode-blob+)
                 (= (third (third entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a CVI insertion record." sha))
    (let* ((insertion-point
             (%loaded-object repo-ptr (second (first entries))))
           (start-ion
             (%loaded-object repo-ptr (second (second entries))))
           (values-sha (second (third entries)))
           (values (%persistent-vector-data repo-ptr values-sha)))
      (check-type insertion-point (integer 0 *))
      (check-type start-ion (integer 1 *))
      (when (zerop (persistent-vector-data-length values))
        (error "CVI insertion record ~S has no values." sha))
      (make-cvi-insertion-data
       start-ion insertion-point values-sha))))

(defun make-cvi-insertion-record (start-ion insertion-point values)
  (check-type values sequence)
  (let ((persistent-values (%sequence->persistent-vector values)))
    (with-repository ()
      (make-instance
       'cvi-insertion-record
       :sha (%make-cvi-insertion-root
             (current-repository) start-ion insertion-point
             (persistent-vector-sha persistent-values))))))

(defun cvi-insertion-record-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cvi-insertion-data (current-repository) sha)
    (make-instance 'cvi-insertion-record :sha sha)))

(defun cvi-insertion-record/start-ion (record)
  (check-type record cvi-insertion-record)
  (with-repository ()
    (cvi-insertion-data-start-ion
     (%cvi-insertion-data
      (current-repository) (cvi-insertion-record-sha record)))))

(defun cvi-insertion-record/insertion-point (record)
  (check-type record cvi-insertion-record)
  (with-repository ()
    (cvi-insertion-data-insertion-point
     (%cvi-insertion-data
      (current-repository) (cvi-insertion-record-sha record)))))

(defun cvi-insertion-record/values (record)
  (check-type record cvi-insertion-record)
  (with-repository ()
    (make-instance
     'persistent-vector
     :sha
     (cvi-insertion-data-values-sha
      (%cvi-insertion-data
       (current-repository) (cvi-insertion-record-sha record))))))

(defun cvi-insertion-record/get-value-for-ion (record ion)
  (let ((index (- ion (cvi-insertion-record/start-ion record))))
    (persistent-vector-ref
     (cvi-insertion-record/values record) index)))

(defun %make-cvi-deletion-root (repo-ptr start-ion limit-ion)
  (check-type start-ion (integer 1 *))
  (check-type limit-ion (integer 2 *))
  (unless (< start-ion limit-ion)
    (error "CVI deletion range [~D, ~D) is empty or reversed."
           start-ion limit-ion))
  (create-tree
   repo-ptr
   (list
    (list "limit-ion" (%stored-object repo-ptr limit-ion)
          +git-filemode-blob+)
    (list "start-ion" (%stored-object repo-ptr start-ion)
          +git-filemode-blob+))))

(defun %cvi-deletion-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("limit-ion" "start-ion"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  entries))
      (error "Git tree ~S is not a CVI deletion record." sha))
    (let ((limit-ion
            (%loaded-object repo-ptr (second (first entries))))
          (start-ion
            (%loaded-object repo-ptr (second (second entries)))))
      (check-type start-ion (integer 1 *))
      (check-type limit-ion (integer 2 *))
      (unless (< start-ion limit-ion)
        (error "CVI deletion record ~S has invalid range." sha))
      (make-cvi-deletion-data start-ion limit-ion))))

(defun make-cvi-deletion-record (start-ion limit-ion)
  (with-repository ()
    (make-instance
     'cvi-deletion-record
     :sha (%make-cvi-deletion-root
           (current-repository) start-ion limit-ion))))

(defun cvi-deletion-record-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cvi-deletion-data (current-repository) sha)
    (make-instance 'cvi-deletion-record :sha sha)))

(defun cvi-deletion-record/start-ion (record)
  (check-type record cvi-deletion-record)
  (with-repository ()
    (cvi-deletion-data-start-ion
     (%cvi-deletion-data
      (current-repository) (cvi-deletion-record-sha record)))))

(defun cvi-deletion-record/limit-ion (record)
  (check-type record cvi-deletion-record)
  (with-repository ()
    (cvi-deletion-data-limit-ion
     (%cvi-deletion-data
      (current-repository) (cvi-deletion-record-sha record)))))

(defun %validate-cvi-record-vector
    (repo-ptr vector-sha validator)
  (let* ((data (%persistent-vector-data repo-ptr vector-sha))
         (vector
           (make-instance 'persistent-vector :sha vector-sha)))
    (dotimes (index (persistent-vector-data-length data))
      (let ((sha (persistent-vector-ref vector index)))
        (check-type sha string)
        (funcall validator repo-ptr sha)))
    data))

(defun %make-cvi-change-root
    (repo-ptr cid insertions-sha deletions-sha)
  (%check-version-cid cid)
  (%validate-cvi-record-vector
   repo-ptr insertions-sha #'%cvi-insertion-data)
  (%validate-cvi-record-vector
   repo-ptr deletions-sha #'%cvi-deletion-data)
  (create-tree
   repo-ptr
   (list
    (list "cid" (%stored-object repo-ptr cid)
          +git-filemode-blob+)
    (list "deletions" deletions-sha +git-filemode-tree+)
    (list "insertions" insertions-sha +git-filemode-tree+))))

(defun %cvi-change-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 3)
                 (equal (mapcar #'first entries)
                        '("cid" "deletions" "insertions"))
                 (= (third (first entries)) +git-filemode-blob+)
                 (= (third (second entries)) +git-filemode-tree+)
                 (= (third (third entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a CVI change record." sha))
    (let ((cid (%loaded-object repo-ptr (second (first entries))))
          (deletions-sha (second (second entries)))
          (insertions-sha (second (third entries))))
      (%check-version-cid cid)
      (%validate-cvi-record-vector
       repo-ptr insertions-sha #'%cvi-insertion-data)
      (%validate-cvi-record-vector
       repo-ptr deletions-sha #'%cvi-deletion-data)
      (make-cvi-change-data cid insertions-sha deletions-sha))))

(defun make-cvi-change-record
    (cid &key insertion-records deletion-records)
  (dolist (record insertion-records)
    (check-type record cvi-insertion-record))
  (dolist (record deletion-records)
    (check-type record cvi-deletion-record))
  (let ((insertions
          (%sha-vector insertion-records
                       #'cvi-insertion-record-sha))
        (deletions
          (%sha-vector deletion-records
                       #'cvi-deletion-record-sha)))
    (with-repository ()
      (make-instance
       'cvi-change-record
       :sha (%make-cvi-change-root
             (current-repository) cid
             (persistent-vector-sha insertions)
             (persistent-vector-sha deletions))))))

(defun cvi-change-record-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cvi-change-data (current-repository) sha)
    (make-instance 'cvi-change-record :sha sha)))

(defun cvi-change-record/cid (record)
  (check-type record cvi-change-record)
  (with-repository ()
    (cvi-change-data-cid
     (%cvi-change-data
      (current-repository) (cvi-change-record-sha record)))))

(defun %cvi-record-wrapper-vector
    (record vector-sha accessor)
  (declare (ignore record))
  (let* ((sha-vector
           (make-instance 'persistent-vector :sha vector-sha))
         (length (persistent-vector-length sha-vector))
         (result (make-array length)))
    (dotimes (index length result)
      (setf (aref result index)
            (funcall accessor
                     (persistent-vector-ref sha-vector index))))))

(defun cvi-change-record/insertion-records (record)
  (check-type record cvi-change-record)
  (with-repository ()
    (let ((data
            (%cvi-change-data
             (current-repository)
             (cvi-change-record-sha record))))
      (%cvi-record-wrapper-vector
       record (cvi-change-data-insertions-sha data)
       #'cvi-insertion-record-from-sha))))

(defun cvi-change-record/deletion-records (record)
  (check-type record cvi-change-record)
  (with-repository ()
    (let ((data
            (%cvi-change-data
             (current-repository)
             (cvi-change-record-sha record))))
      (%cvi-record-wrapper-vector
       record (cvi-change-data-deletions-sha data)
       #'cvi-deletion-record-from-sha))))

(defun %make-cvi-root
    (repo-ptr changes-sha default-allowed-p next-ion)
  (unless (typep default-allowed-p 'boolean)
    (error "Invalid CVI default flag ~S." default-allowed-p))
  (check-type next-ion (integer 1 *))
  (%validate-cvi-history repo-ptr changes-sha next-ion)
  (create-tree
   repo-ptr
   (list
    (list "changes" changes-sha +git-filemode-tree+)
    (list "default-allowed"
          (%stored-object repo-ptr default-allowed-p)
          +git-filemode-blob+)
    (list "next-ion" (%stored-object repo-ptr next-ion)
          +git-filemode-blob+)
    (list "type" (%stored-object repo-ptr :cvi)
          +git-filemode-blob+))))

(defun %validate-cvi-history (repo-ptr changes-sha next-ion)
  (%validate-cvi-record-vector
   repo-ptr changes-sha #'%cvi-change-data)
  (let* ((changes
           (make-instance 'persistent-vector :sha changes-sha))
         (previous-cid 0)
         (seen-ions (make-hash-table :test #'eql)))
    (dotimes (change-index (persistent-vector-length changes))
      (let* ((change-sha
               (persistent-vector-ref changes change-index))
             (change (%cvi-change-data repo-ptr change-sha))
             (cid (cvi-change-data-cid change)))
        (unless (> cid previous-cid)
          (error "CVI change CIDs are not strictly increasing at ~D."
                 cid))
        (setf previous-cid cid)
        (let ((insertions
                (make-instance
                 'persistent-vector
                 :sha (cvi-change-data-insertions-sha change))))
          (dotimes (index (persistent-vector-length insertions))
            (let* ((insertion
                     (%cvi-insertion-data
                      repo-ptr
                      (persistent-vector-ref insertions index)))
                   (start (cvi-insertion-data-start-ion insertion))
                   (count
                     (persistent-vector-length
                      (make-instance
                       'persistent-vector
                       :sha
                       (cvi-insertion-data-values-sha insertion))))
                   (point
                     (cvi-insertion-data-insertion-point insertion)))
              (unless (< point next-ion)
                (error "CVI insertion point ~D exceeds its ION range."
                       point))
              (unless (<= (+ start count) next-ion)
                (error "CVI insertion range beginning at ~D exceeds ~D."
                       start (1- next-ion)))
              (dotimes (offset count)
                (let ((ion (+ start offset)))
                  (when (gethash ion seen-ions)
                    (error "CVI ION ~D is inserted more than once." ion))
                  (setf (gethash ion seen-ions) t))))))
        (let ((deletions
                (make-instance
                 'persistent-vector
                 :sha (cvi-change-data-deletions-sha change))))
          (dotimes (index (persistent-vector-length deletions))
            (let ((deletion
                    (%cvi-deletion-data
                     repo-ptr
                     (persistent-vector-ref deletions index))))
              (when (> (cvi-deletion-data-limit-ion deletion)
                       next-ion)
                (error "CVI deletion range exceeds ION ~D."
                       (1- next-ion)))))))))
  t)

(defun %cvi-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 4)
                 (equal (mapcar #'first entries)
                        '("changes" "default-allowed"
                          "next-ion" "type"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  (rest entries)))
      (error "Git tree ~S is not a CVI." sha))
    (let ((changes-sha (second (first entries)))
          (default-allowed-p
            (%loaded-object repo-ptr (second (second entries))))
          (next-ion
            (%loaded-object repo-ptr (second (third entries))))
          (type (%loaded-object repo-ptr (second (fourth entries)))))
      (unless (eq type :cvi)
        (error "Invalid CVI type ~S." type))
      (unless (typep default-allowed-p 'boolean)
        (error "Invalid CVI default flag ~S." default-allowed-p))
      (check-type next-ion (integer 1 *))
      (%validate-cvi-history repo-ptr changes-sha next-ion)
      (make-cvi-data changes-sha default-allowed-p next-ion))))

(defmethod %validate-versioned-value-sha
    ((type (eql :cvi)) repo-ptr sha)
  (declare (ignore type))
  (%cvi-data repo-ptr sha))

(defun make-cvi (&key (initial-value nil initial-value-supplied-p))
  (when (and initial-value-supplied-p initial-value)
    (error "A CVI initial value may only be NIL."))
  (let ((changes (make-persistent-vector 0)))
    (with-repository ()
      (make-instance
       'cvi
       :sha (%make-cvi-root
             (current-repository)
             (persistent-vector-sha changes)
             initial-value-supplied-p 1)))))

(defun cvi-from-sha (sha)
  (let ((value (versioned-value-from-sha sha)))
    (check-type value cvi)
    value))

(defun cvi/change-records (value)
  (check-type value cvi)
  (with-repository ()
    (let* ((data
             (%cvi-data
              (current-repository) (versioned-value-sha value)))
           (sha-vector
             (make-instance
              'persistent-vector :sha (cvi-data-changes-sha data)))
           (result
             (make-array (persistent-vector-length sha-vector))))
      (dotimes (index (length result) result)
        (setf (aref result index)
              (cvi-change-record-from-sha
               (persistent-vector-ref sha-vector index)))))))

(defun cvi/default-allowed (value)
  (check-type value cvi)
  (with-repository ()
    (cvi-data-default-allowed-p
     (%cvi-data
      (current-repository) (versioned-value-sha value)))))

(defun cvi/max-ion (value)
  (check-type value cvi)
  (with-repository ()
    (1-
     (cvi-data-next-ion
      (%cvi-data
       (current-repository) (versioned-value-sha value))))))

(defun %cvi-global-state (value cid-set)
  (let ((order nil)
        (active (make-hash-table :test #'eql))
        (ion-values (make-hash-table :test #'eql))
        (applicable-p nil))
    (loop for change across (cvi/change-records value)
          for change-active-p =
            (cid-set/member cid-set (cvi-change-record/cid change))
          do
             (when change-active-p
               (setf applicable-p t))
             (loop for insertion across
                   (cvi-change-record/insertion-records change)
                   for start = (cvi-insertion-record/start-ion insertion)
                   for values = (cvi-insertion-record/values insertion)
                   for count = (persistent-vector-length values)
                   for ions =
                     (loop for offset below count
                           for ion = (+ start offset)
                           do
                              (multiple-value-bind (ignored present-p)
                                  (gethash ion ion-values)
                                (declare (ignore ignored))
                                (when present-p
                                  (error
                                   "CVI ION ~D is inserted more than once."
                                   ion)))
                              (setf (gethash ion ion-values)
                                    (persistent-vector-ref values offset)
                                    (gethash ion active)
                                    change-active-p)
                           collect ion)
                   for insertion-point =
                     (cvi-insertion-record/insertion-point insertion)
                   do
                      (if (zerop insertion-point)
                          (setf order (append ions order))
                          (let ((position
                                  (position insertion-point order)))
                            (unless position
                              (error "CVI insertion point ION ~D is absent."
                                     insertion-point))
                            (setf order
                                  (append
                                   (subseq order 0 (1+ position))
                                   ions
                                   (subseq order (1+ position)))))))
             (when change-active-p
               (loop for deletion across
                     (cvi-change-record/deletion-records change)
                     do
                        (loop for ion
                              from (cvi-deletion-record/start-ion deletion)
                              below (cvi-deletion-record/limit-ion deletion)
                              do (setf (gethash ion active) nil)))))
    (values order active ion-values applicable-p)))

(defun cvi/active-ion-vector (value cid-set)
  (check-type value cvi)
  (check-type cid-set cid-set)
  (multiple-value-bind (order active ion-values applicable-p)
      (%cvi-global-state value cid-set)
    (declare (ignore ion-values))
    (values
     (and applicable-p
          (coerce
           (cons 0
                 (remove-if-not
                  (lambda (ion) (gethash ion active))
                  order))
           'vector))
     applicable-p)))

(defun cvi-create-insertion-record-ion-index (value)
  (let ((result (make-array (1+ (cvi/max-ion value))
                            :initial-element nil)))
    (loop for change across (cvi/change-records value)
          do
             (loop for insertion across
                   (cvi-change-record/insertion-records change)
                   for start = (cvi-insertion-record/start-ion insertion)
                   for count =
                     (persistent-vector-length
                      (cvi-insertion-record/values insertion))
                   do
                      (dotimes (offset count)
                        (setf (aref result (+ start offset))
                              insertion))))
    result))

(defun cvi-get-ion-vector-and-index (value cid-set)
  (multiple-value-bind (active-ions bound-p)
      (cvi/active-ion-vector value cid-set)
    (values active-ions bound-p
            (cvi-create-insertion-record-ion-index value))))

(defun cvi/reconstruct-value (value cid-set instance slot)
  (declare (ignore instance slot))
  (multiple-value-bind (order active ion-values applicable-p)
      (%cvi-global-state value cid-set)
    (unless (or applicable-p (cvi/default-allowed value))
      (error 'unbound-versioned-value :versioned-value value))
    (loop for ion in order
          when (gethash ion active)
            collect (gethash ion ion-values))))

(defun cvi/scan-reconstructed-value (value cid-set instance slot)
  (cvi/reconstruct-value value cid-set instance slot))

(defun composite-versioned-value/scan
    (value cid-set instance slot)
  (cvi/scan-reconstructed-value value cid-set instance slot))

(defmethod versioned-value/view
    ((value cvi) cid-set instance slot)
  (cvi/reconstruct-value value cid-set instance slot))

(defmethod versioned-value/cid-list ((value cvi))
  (loop for change across (cvi/change-records value)
        collect (cvi-change-record/cid change)))

(defmethod versioned-value/contains-cid? ((value cvi) cid)
  (not (null (find cid (versioned-value/cid-list value) :test #'=))))

(defmethod versioned-value/cid-set (repository (value cvi))
  (list->cid-set repository (versioned-value/cid-list value)))

(defmethod versioned-value/most-recent-cid
    ((value cvi) cid-set)
  (loop for cid in (reverse (versioned-value/cid-list value))
        when (cid-set/member cid-set cid)
          return cid))

(defun cvi-find-cid-change-record
    (value cid &key most-recent-only)
  (%check-version-cid cid)
  (let ((records (cvi/change-records value)))
    (loop for index downfrom (1- (length records)) to 0
          for record = (aref records index)
          when (= (cvi-change-record/cid record) cid)
            return (values record index)
          when most-recent-only
            return (values nil nil)
          finally (return (values nil nil)))))

(defun %cvi-with-data (changes next-ion default-allowed-p)
  (with-repository ()
    (make-instance
     'cvi
     :sha (%make-cvi-root
           (current-repository)
           (persistent-vector-sha changes)
           default-allowed-p next-ion))))

(defun %cvi-base-for-cid (value cid)
  (let* ((records (cvi/change-records value))
         (length (length records))
         (last-cid
           (and (plusp length)
                (cvi-change-record/cid
                 (aref records (1- length))))))
    (when (and last-cid (< cid last-cid))
      (error "Cannot append CVI CID ~D after CID ~D." cid last-cid))
    (if (and last-cid (= cid last-cid))
        (let ((remaining
                (loop for index below (1- length)
                      collect
                      (cvi-change-record-sha
                       (aref records index)))))
          (%cvi-with-data
           (%sequence->persistent-vector remaining)
           (1+ (cvi/max-ion value))
           (cvi/default-allowed value)))
        value)))

(defun %cvi-equal-prefix-length (left right)
  (loop for left-value in left
        for right-value in right
        while (vi-value-same? left-value right-value)
        count t))

(defun %cvi-values-same-p (left right)
  (and (= (length left) (length right))
       (every #'vi-value-same? left right)))

(defun %cvi-equal-suffix-length (left right prefix)
  (loop with left-length = (length left)
        with right-length = (length right)
        for offset from 1
        while (and (<= offset (- left-length prefix))
                   (<= offset (- right-length prefix))
                   (vi-value-same?
                    (nth (- left-length offset) left)
                    (nth (- right-length offset) right)))
        count t))

(defun %cvi-deletion-ranges (ions)
  (let ((ranges nil)
        (start nil)
        (previous nil))
    (dolist (ion ions)
      (if (and previous (= ion (1+ previous)))
          (setf previous ion)
          (progn
            (when start
              (push (make-cvi-deletion-record start (1+ previous))
                    ranges))
            (setf start ion
                  previous ion))))
    (when start
      (push (make-cvi-deletion-record start (1+ previous)) ranges))
    (nreverse ranges)))

(defun cvi/update
    (value new-value cid basis-cid-set
     &optional instance slot)
  (check-type value cvi)
  (check-type new-value sequence)
  (%check-version-cid cid)
  (check-type basis-cid-set cid-set)
  (let* ((base (%cvi-base-for-cid value cid))
         (old-value
           (multiple-value-bind (active-ions bound-p)
               (cvi/active-ion-vector base basis-cid-set)
             (declare (ignore bound-p))
             (if active-ions
                 (cvi/reconstruct-value
                  base basis-cid-set instance slot)
                 nil)))
         (new-list (coerce new-value 'list)))
    (when (and (%cvi-values-same-p old-value new-list)
               (or old-value (cvi/default-allowed base)))
      (%signal-no-versioned-change instance slot new-value)
      (return-from cvi/update base))
    (multiple-value-bind (active-ions bound-p)
        (cvi/active-ion-vector base basis-cid-set)
      (declare (ignore bound-p))
      (let* ((prefix (%cvi-equal-prefix-length old-value new-list))
             (suffix (%cvi-equal-suffix-length
                      old-value new-list prefix))
             (old-limit (- (length old-value) suffix))
             (new-limit (- (length new-list) suffix))
             (deleted-ions
               (loop for index from prefix below old-limit
                     collect (aref active-ions (1+ index))))
             (inserted-values (subseq new-list prefix new-limit))
             (insertion-point
               (if (zerop prefix) 0
                   (aref active-ions prefix)))
             (next-ion (1+ (cvi/max-ion base)))
             (insertions
               (if inserted-values
                   (list
                    (make-cvi-insertion-record
                     next-ion insertion-point inserted-values))
                   nil))
             (deletions (%cvi-deletion-ranges deleted-ions))
             (change
               (make-cvi-change-record
                cid :insertion-records insertions
                :deletion-records deletions))
             (change-shas
               (map 'list #'cvi-change-record-sha
                    (cvi/change-records base)))
             (changes
               (%sequence->persistent-vector
                (append change-shas
                        (list (cvi-change-record-sha change)))))
             (new-next-ion
               (+ next-ion (length inserted-values))))
        (%cvi-with-data
         changes new-next-ion (cvi/default-allowed base))))))

(defmethod versioned-value/update
    (new-value (value cvi) (cid integer) instance slot)
  (let ((basis
          (cid-set/remove
           (versioned-value/cid-set nil value)
           cid)))
    (cvi/update value new-value cid basis instance slot)))
