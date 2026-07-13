;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass cvfile (versioned-value) ())

(defstruct (cvfile-data
             (:constructor make-cvfile-data
                 (cids-sha file-cids-sha versions-sha guid fuid)))
  cids-sha file-cids-sha versions-sha guid fuid)

(defun %generate-cvfile-guid ()
  (format nil "~32,'0X" (random (ash 1 128))))

(defun %validate-cvfile-components
    (repo-ptr cids-sha file-cids-sha versions-sha)
  (let ((cids (%persistent-vector-data repo-ptr cids-sha))
        (file-cids (%persistent-vector-data repo-ptr file-cids-sha))
        (versions (%persistent-vector-data repo-ptr versions-sha)))
    (unless (= (persistent-vector-data-length cids)
               (persistent-vector-data-length file-cids))
      (error "CVFILE CID mapping vectors have different lengths."))
    (let ((cid-vector (make-instance 'persistent-vector :sha cids-sha))
          (file-cid-vector
            (make-instance 'persistent-vector :sha file-cids-sha))
          (version-vector
            (make-instance 'persistent-vector :sha versions-sha))
          (previous-cid 0))
      (when (zerop (persistent-vector-data-length cids))
        (error "A CVFILE must contain at least one CID mapping."))
      (dotimes (index (persistent-vector-data-length versions))
        (unless (vectorp (persistent-vector-ref version-vector index))
          (error "CVFILE version ~D is not a vector." index)))
      (dotimes (index (persistent-vector-data-length cids))
        (let ((cid (persistent-vector-ref cid-vector index))
              (file-cid
                (persistent-vector-ref file-cid-vector index)))
          (%check-version-cid cid)
          (unless (> cid previous-cid)
            (error "CVFILE CIDs are not strictly increasing at ~D." cid))
          (setf previous-cid cid)
          (unless (typep file-cid '(integer 0 *))
            (error "Invalid CVFILE file CID ~S." file-cid))
          (when (>= file-cid
                    (persistent-vector-data-length versions))
            (error "CVFILE file CID ~D has no stored version."
                   file-cid))))))
  t)

(defun %make-cvfile-root
    (repo-ptr cids-sha file-cids-sha versions-sha guid fuid)
  (%validate-cvfile-components
   repo-ptr cids-sha file-cids-sha versions-sha)
  (create-tree
   repo-ptr
   (list
    (list "cids" cids-sha +git-filemode-tree+)
    (list "file-cids" file-cids-sha +git-filemode-tree+)
    (list "fuid" (%stored-object repo-ptr fuid)
          +git-filemode-blob+)
    (list "guid" (%stored-object repo-ptr guid)
          +git-filemode-blob+)
    (list "type" (%stored-object repo-ptr :cvfile)
          +git-filemode-blob+)
    (list "versions" versions-sha +git-filemode-tree+))))

(defun %cvfile-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 6)
                 (equal (mapcar #'first entries)
                        '("cids" "file-cids" "fuid"
                          "guid" "type" "versions"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (second entries)) +git-filemode-tree+)
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  (subseq entries 2 5))
                 (= (third (sixth entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a CVFILE." sha))
    (let ((cids-sha (second (first entries)))
          (file-cids-sha (second (second entries)))
          (fuid (%loaded-object repo-ptr (second (third entries))))
          (guid (%loaded-object repo-ptr (second (fourth entries))))
          (type (%loaded-object repo-ptr (second (fifth entries))))
          (versions-sha (second (sixth entries))))
      (unless (eq type :cvfile)
        (error "Invalid CVFILE type ~S." type))
      (%validate-cvfile-components
       repo-ptr cids-sha file-cids-sha versions-sha)
      (make-cvfile-data
       cids-sha file-cids-sha versions-sha guid fuid))))

(defmethod %validate-versioned-value-sha
    ((type (eql :cvfile)) repo-ptr sha)
  (declare (ignore type))
  (%cvfile-data repo-ptr sha))

(defun %cvfile-object
    (cids file-cids versions guid fuid)
  (with-repository ()
    (make-instance
     'cvfile
     :sha (%make-cvfile-root
           (current-repository)
           (persistent-vector-sha cids)
           (persistent-vector-sha file-cids)
           (persistent-vector-sha versions)
           guid fuid))))

(defun make-cvfile
    (&key (initial-value #()) (cid 1)
          (guid (%generate-cvfile-guid))
          (fuid guid))
  (check-type initial-value sequence)
  (%check-version-cid cid)
  (%cvfile-object
   (%sequence->persistent-vector (list cid))
   (%sequence->persistent-vector '(0))
   (%sequence->persistent-vector
    (list (coerce initial-value 'vector)))
   guid fuid))

(defun cvfile-from-sha (sha)
  (let ((value (versioned-value-from-sha sha)))
    (check-type value cvfile)
    value))

(defun %cvfile-vector (value accessor)
  (check-type value cvfile)
  (with-repository ()
    (make-instance
     'persistent-vector
     :sha
     (funcall accessor
              (%cvfile-data
               (current-repository)
               (versioned-value-sha value))))))

(defun cvfile/guid (value)
  (check-type value cvfile)
  (with-repository ()
    (cvfile-data-guid
     (%cvfile-data
      (current-repository) (versioned-value-sha value)))))

(defun cvfile/fuid (value)
  (check-type value cvfile)
  (with-repository ()
    (cvfile-data-fuid
     (%cvfile-data
      (current-repository) (versioned-value-sha value)))))

(defun cvfile/cid-vector (value)
  (%cvfile-vector value #'cvfile-data-cids-sha))

(defun cvfile/file-cid-vector (value)
  (%cvfile-vector value #'cvfile-data-file-cids-sha))

(defun %cvfile/versions (value)
  (%cvfile-vector value #'cvfile-data-versions-sha))

(defun cvfile/map-cid-set (value cid-set)
  (check-type cid-set cid-set)
  (let ((cids (cvfile/cid-vector value))
        (file-cids (cvfile/file-cid-vector value)))
    (loop for index below (persistent-vector-length cids)
          for cid = (persistent-vector-ref cids index)
          when (cid-set/member cid-set cid)
            collect (persistent-vector-ref file-cids index))))

(defmethod versioned-value/cid-list ((value cvfile))
  (coerce
   (persistent-vector->vector (cvfile/cid-vector value))
   'list))

(defmethod versioned-value/contains-cid? ((value cvfile) cid)
  (not (null (find cid (versioned-value/cid-list value) :test #'=))))

(defmethod versioned-value/cid-set (repository (value cvfile))
  (list->cid-set repository (versioned-value/cid-list value)))

(defmethod versioned-value/most-recent-cid
    ((value cvfile) cid-set)
  (loop for cid in (reverse (versioned-value/cid-list value))
        when (cid-set/member cid-set cid)
          return cid))

(defmethod versioned-value/view
    ((value cvfile) cid-set instance slot)
  (declare (ignore instance slot))
  (let ((file-cids (cvfile/map-cid-set value cid-set)))
    (unless file-cids
      (error 'unbound-versioned-value :versioned-value value))
    (persistent-vector-ref
     (%cvfile/versions value) (car (last file-cids)))))

(defun cvfile/update (value new-value cid &optional instance slot)
  (check-type value cvfile)
  (check-type new-value sequence)
  (%check-version-cid cid)
  (let* ((cids (cvfile/cid-vector value))
         (file-cids (cvfile/file-cid-vector value))
         (versions (%cvfile/versions value))
         (cid-list (versioned-value/cid-list value))
         (last-cid (car (last cid-list)))
         (existing-index (position cid cid-list :test #'=)))
    (when (and (null existing-index) (< cid last-cid))
      (error "Cannot append CVFILE CID ~D after CID ~D." cid last-cid))
    (let* ((new-vector (coerce new-value 'vector))
           (latest-value
             (persistent-vector-ref
              versions
              (persistent-vector-ref
               file-cids
               (if existing-index existing-index
                   (1- (persistent-vector-length file-cids)))))))
      (when (vi-value-same? new-vector latest-value)
        (%signal-no-versioned-change instance slot new-value)
        (return-from cvfile/update value))
      (multiple-value-bind (new-versions file-cid)
          (persistent-vector-push-extend new-vector versions)
        (if existing-index
            (%cvfile-object
             cids
             (persistent-vector-update
              file-cids existing-index file-cid)
             new-versions (cvfile/guid value) (cvfile/fuid value))
            (multiple-value-bind (new-cids ignored-index)
                (persistent-vector-push-extend cid cids)
              (declare (ignore ignored-index))
              (multiple-value-bind (new-file-cids ignored-file-index)
                  (persistent-vector-push-extend file-cid file-cids)
                (declare (ignore ignored-file-index))
                (%cvfile-object
                 new-cids new-file-cids new-versions
                 (cvfile/guid value) (cvfile/fuid value)))))))))

(defmethod versioned-value/update
    (new-value (value cvfile) (cid integer) instance slot)
  (cvfile/update value new-value cid instance slot))
