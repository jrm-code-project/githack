;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defparameter +repository-types+
  '(:basic :master :satellite :transport :extent :workspace))

(deftype repository-type ()
  '(member :basic :master :satellite
    :transport :extent :workspace))

(defclass repository-persistent-information ()
  ((sha :initarg :sha :reader repository-sha :type string)))

(defclass repository (repository-persistent-information)
  (
   (transaction-stacks
    :initform nil :accessor repository-transaction-stacks)))

(defstruct (repository-data
             (:constructor make-repository-data
                 (domain name type parent satellites-sha
                  canonical-dictionary-sha
                  root-mapper-sha local-mapper-sha cid-mapper-sha
                  cid-master-table-sha locally-named-roots-sha
                  anonymous-user-reference)))
  domain
  name
  type
  parent
  satellites-sha
  canonical-dictionary-sha
  root-mapper-sha
  local-mapper-sha
  cid-mapper-sha
  cid-master-table-sha
  locally-named-roots-sha
  anonymous-user-reference)

(defun repository-type-keyword->string-extension (type)
  (ecase type
    (:basic "ydb")
    (:master "ydm")
    (:satellite "yds")
    (:transport "ydt")
    (:extent "ydx")
    (:workspace "ydw")))

(defun %repository-value-reference (value)
  (cond
    ((and (find-class 'distributed-object nil)
          (typep value 'distributed-object))
     (list :distributed-object (distributed-object-sha value)))
    (t
     (typecase value
    (repository (list :repository (repository-sha value)))
    (versioned-value
     (list :versioned-value (versioned-value-sha value)))
    (cid-master-table
     (list :cid-master-table (cid-master-table-sha value)))
    (cid-master-table-entry
     (list :cid-master-table-entry
           (cid-master-table-entry-sha value)))
    (cid-detail-table
     (list :cid-detail-table (cid-detail-table-sha value)))
       (t (%mapper-value-reference value))))))

(defun %repository-reference-value (reference)
  (unless (and (consp reference)
               (consp (cdr reference))
               (null (cddr reference)))
    (error "Invalid repository value reference ~S." reference))
  (case (first reference)
    (:repository (repository-from-sha (second reference)))
    (:versioned-value
     (versioned-value-from-sha (second reference)))
    (:cid-master-table
     (cid-master-table-from-sha (second reference)))
    (:cid-master-table-entry
     (cid-master-table-entry-from-sha (second reference)))
    (:cid-detail-table
     (cid-detail-table-from-sha (second reference)))
    (:distributed-object
     (distributed-object-from-sha (second reference)))
    (otherwise (%mapper-reference-value reference))))

(defun %validate-repository-satellites (repo-ptr sha)
  (let* ((data (%persistent-vector-data repo-ptr sha))
         (vector (make-instance 'persistent-vector :sha sha)))
    (dotimes (index (persistent-vector-data-length data))
      (%repository-reference-value
       (persistent-vector-ref vector index)))
    data))

(defun %make-repository-root
    (repo-ptr domain name type parent satellites-sha
     canonical-dictionary-sha
     root-mapper-sha local-mapper-sha cid-mapper-sha
     cid-master-table-sha locally-named-roots-sha
     anonymous-user-reference)
  (check-type domain string)
  (check-type name string)
  (check-type type repository-type)
  (%validate-repository-satellites repo-ptr satellites-sha)
  (%canonical-dictionary-entries-sha
   repo-ptr canonical-dictionary-sha)
  (%mapper-data repo-ptr root-mapper-sha)
  (%mapper-data repo-ptr local-mapper-sha)
  (let ((cid-mapper (%mapper-data repo-ptr cid-mapper-sha)))
    (unless (eq (mapper-data-type cid-mapper) :ordered)
      (error "Repository CID mapper must be ordered.")))
  (%cid-master-table-data repo-ptr cid-master-table-sha)
  (%persistent-hash-table-data repo-ptr locally-named-roots-sha)
  (%repository-reference-value anonymous-user-reference)
  (create-tree
   repo-ptr
   (list
    (list "anonymous-user"
          (%stored-object repo-ptr anonymous-user-reference)
          +git-filemode-blob+)
    (list "canonical-dictionary" canonical-dictionary-sha
          +git-filemode-tree+)
    (list "cid-mapper" cid-mapper-sha +git-filemode-tree+)
    (list "cid-master-table" cid-master-table-sha
          +git-filemode-tree+)
    (list "domain" (%stored-object repo-ptr domain)
          +git-filemode-blob+)
    (list "local-mapper" local-mapper-sha +git-filemode-tree+)
    (list "locally-named-roots" locally-named-roots-sha
          +git-filemode-tree+)
    (list "name" (%stored-object repo-ptr name)
          +git-filemode-blob+)
    (list "parent" (%stored-object repo-ptr parent)
          +git-filemode-blob+)
    (list "root-mapper" root-mapper-sha +git-filemode-tree+)
    (list "satellites" satellites-sha +git-filemode-tree+)
    (list "type" (%stored-object repo-ptr type)
          +git-filemode-blob+))))

(defun %repository-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless
        (and
         (= (length entries) 12)
         (equal
          (mapcar #'first entries)
          '("anonymous-user" "canonical-dictionary"
            "cid-mapper" "cid-master-table"
            "domain" "local-mapper" "locally-named-roots"
            "name" "parent" "root-mapper" "satellites" "type"))
         (= (third (first entries)) +git-filemode-blob+)
         (every
          (lambda (index)
            (= (third (nth index entries)) +git-filemode-tree+))
          '(1 2 3 5 6 9 10))
         (every
          (lambda (index)
            (= (third (nth index entries)) +git-filemode-blob+))
          '(4 7 8 11)))
      (error "Git tree ~S is not a repository object." sha))
    (let* ((anonymous-user-reference
             (%loaded-object repo-ptr (second (first entries))))
    (canonical-dictionary-sha (second (second entries)))
    (cid-mapper-sha (second (third entries)))
    (cid-master-table-sha (second (fourth entries)))
    (domain (%loaded-object repo-ptr (second (fifth entries))))
    (local-mapper-sha (second (sixth entries)))
    (locally-named-roots-sha (second (seventh entries)))
    (name (%loaded-object repo-ptr (second (eighth entries))))
    (parent (%loaded-object repo-ptr (second (ninth entries))))
    (root-mapper-sha (second (tenth entries)))
    (satellites-sha (second (nth 10 entries)))
    (type (%loaded-object repo-ptr (second (nth 11 entries)))))
      (check-type domain string)
      (check-type name string)
      (check-type type repository-type)
      (%repository-reference-value anonymous-user-reference)
      (%validate-repository-satellites repo-ptr satellites-sha)
      (%canonical-dictionary-entries-sha
       repo-ptr canonical-dictionary-sha)
      (%mapper-data repo-ptr root-mapper-sha)
      (%mapper-data repo-ptr local-mapper-sha)
      (let ((cid-mapper (%mapper-data repo-ptr cid-mapper-sha)))
        (unless (eq (mapper-data-type cid-mapper) :ordered)
          (error "Repository CID mapper must be ordered.")))
      (%cid-master-table-data repo-ptr cid-master-table-sha)
      (%persistent-hash-table-data
       repo-ptr locally-named-roots-sha)
      (make-repository-data
       domain name type parent satellites-sha
       canonical-dictionary-sha
       root-mapper-sha local-mapper-sha cid-mapper-sha
       cid-master-table-sha locally-named-roots-sha
       anonymous-user-reference))))

(defun %repository-object
    (domain name type parent satellites
     canonical-dictionary
     root-mapper local-mapper cid-mapper cid-master-table
     locally-named-roots anonymous-user)
  (with-repository ()
    (make-instance
     'repository
     :sha
     (%make-repository-root
      (current-repository)
      domain name type parent
      (persistent-vector-sha satellites)
      (canonical-class-dictionary-sha canonical-dictionary)
      (mapper-sha root-mapper)
      (mapper-sha local-mapper)
      (mapper-sha cid-mapper)
      (cid-master-table-sha cid-master-table)
      (persistent-hash-table-sha locally-named-roots)
      (%repository-value-reference anonymous-user)))))

(defun make-repository
    (&key (domain "") name (type :basic) parent anonymous-user)
  (check-type name string)
  (check-type domain string)
  (check-type type repository-type)
  (let ((identity
          (make-distributed-identifier
           :domain domain :repository name)))
    (multiple-value-bind (root-mapper local-mapper)
        (distributed-identifier-create-mapper-hierarchy identity)
      (let ((cid-mapper
              (make-ordered-mapper
               :mapping-level "CID" :key :cid
               :parent local-mapper))
            (master-table (make-cid-master-table))
            (canonical-dictionary
              (make-canonical-class-dictionary))
            (named-roots
              (make-persistent-hash-table
               :test 'eql :size 8))
            (satellites (make-persistent-vector 0))
            (user
              (or anonymous-user
                  (make-distributed-identifier
                   :domain domain :repository name
                   :class :user :numeric-id 0))))
        (%repository-object
         domain name type parent satellites canonical-dictionary
         root-mapper local-mapper
         cid-mapper master-table named-roots user)))))

(defun repository-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%repository-data (current-repository) sha)
    (make-instance 'repository :sha sha)))

(defmacro define-repository-reader (name accessor)
  `(defun ,name (repository)
     (check-type repository repository)
     (with-repository ()
       (,accessor
        (%repository-data
         (current-repository) (repository-sha repository))))))

(define-repository-reader repository/domain repository-data-domain)
(define-repository-reader repository/name repository-data-name)
(define-repository-reader repository/type repository-data-type)
(define-repository-reader repository/parent repository-data-parent)

(defun repository/identity (repository)
  (list (repository/domain repository)
        (repository/name repository)))

(defun %repository-linked-object
    (repository data-accessor constructor)
  (check-type repository repository)
  (with-repository ()
    (funcall
     constructor
     (funcall
      data-accessor
      (%repository-data
       (current-repository) (repository-sha repository))))))

(defun repository/root-mapper (repository)
  (%repository-linked-object
   repository #'repository-data-root-mapper-sha #'mapper-from-sha))

(defun repository/local-mapper (repository)
  (%repository-linked-object
   repository #'repository-data-local-mapper-sha #'mapper-from-sha))

(defun repository/cid-mapper (repository)
  (%repository-linked-object
   repository #'repository-data-cid-mapper-sha #'mapper-from-sha))

(defun repository/cid-master-table (repository)
  (%repository-linked-object
   repository
   #'repository-data-cid-master-table-sha
   #'cid-master-table-from-sha))

(defun repository/canonical-class-dictionary (repository)
  (%repository-linked-object
   repository
   #'repository-data-canonical-dictionary-sha
   #'canonical-class-dictionary-from-sha))

(defun repository/locally-named-roots (repository)
  (%repository-linked-object
   repository
   #'repository-data-locally-named-roots-sha
   #'persistent-hash-table-from-sha))

(defun repository/anonymous-user (repository)
  (check-type repository repository)
  (with-repository ()
    (%repository-reference-value
     (repository-data-anonymous-user-reference
      (%repository-data
       (current-repository) (repository-sha repository))))))

(defun repository/satellite-repositories (repository)
  (check-type repository repository)
  (with-repository ()
    (let* ((data
             (%repository-data
              (current-repository) (repository-sha repository)))
           (vector
             (make-instance
              'persistent-vector
              :sha (repository-data-satellites-sha data))))
      (loop for index below (persistent-vector-length vector)
            collect
            (%repository-reference-value
             (persistent-vector-ref vector index))))))

(defun %remake-repository
    (repository &key canonical-dictionary
                     root-mapper local-mapper
                     cid-mapper cid-master-table
                     locally-named-roots satellites)
  (let ((data
          (with-repository ()
            (%repository-data
             (current-repository) (repository-sha repository)))))
    (%repository-object
     (repository-data-domain data)
     (repository-data-name data)
     (repository-data-type data)
     (repository-data-parent data)
     (or satellites
         (make-instance
          'persistent-vector
          :sha (repository-data-satellites-sha data)))
     (or canonical-dictionary
         (canonical-class-dictionary-from-sha
          (repository-data-canonical-dictionary-sha data)))
     (or root-mapper
         (mapper-from-sha (repository-data-root-mapper-sha data)))
     (or local-mapper
         (mapper-from-sha (repository-data-local-mapper-sha data)))
     (or cid-mapper
         (mapper-from-sha (repository-data-cid-mapper-sha data)))
     (or cid-master-table
         (cid-master-table-from-sha
          (repository-data-cid-master-table-sha data)))
     (or locally-named-roots
         (persistent-hash-table-from-sha
          (repository-data-locally-named-roots-sha data)))
     (%repository-reference-value
      (repository-data-anonymous-user-reference data)))))

(defun repository/persistent-data (repository)
  repository)

(defun repository/with-canonical-class-dictionary
    (repository dictionary)
  (check-type dictionary canonical-class-dictionary)
  (%remake-repository
   repository :canonical-dictionary dictionary))

(defun repository/canonical-object-find-or-create
    (repository object)
  (multiple-value-bind (canonical dictionary added-p)
      (canonical-object/find-or-create
       (repository/canonical-class-dictionary repository)
       object)
    (values
     canonical
     (if added-p
         (repository/with-canonical-class-dictionary
          repository dictionary)
         repository)
     added-p)))

(defun repository/with-mappers
    (repository root-mapper local-mapper)
  (check-type root-mapper mapper)
  (check-type local-mapper mapper)
  (%remake-repository
   repository :root-mapper root-mapper
   :local-mapper local-mapper))

(defun repository/with-cid-master-table (repository master-table)
  (check-type master-table cid-master-table)
  (%remake-repository repository :cid-master-table master-table))

(defun repository/locally-named-root (repository name)
  (check-type name symbol)
  (multiple-value-bind (reference present-p)
      (persistent-gethash
       name (repository/locally-named-roots repository))
    (values
     (and present-p (%repository-reference-value reference))
     present-p)))

(defun repository/add-locally-named-root
    (repository object name &key (if-exists :error))
  (check-type name symbol)
  (unless (member if-exists '(:error :supersede))
    (error "Invalid :IF-EXISTS policy ~S." if-exists))
  (multiple-value-bind (old-value present-p)
      (repository/locally-named-root repository name)
    (declare (ignore old-value))
    (when (and present-p (eq if-exists :error))
      (error "Repository root ~S already exists." name))
    (%remake-repository
     repository
     :locally-named-roots
     (persistent-hash-table-set
      (repository/locally-named-roots repository)
      name (%repository-value-reference object)))))

(defun repository/add-satellite-repository
    (repository satellite)
  (let ((references
          (mapcar #'%repository-value-reference
                  (repository/satellite-repositories repository))))
    (%remake-repository
     repository
     :satellites
     (make-persistent-vector
      (1+ (length references))
      :initial-contents
      (append references
              (list (%repository-value-reference satellite)))))))

(defun repository/cid-distributed-identifier (repository cid)
  (%check-version-cid cid)
  (make-distributed-identifier
   :domain (repository/domain repository)
   :repository (repository/name repository)
   :class :cid :numeric-id cid))

(defun repository/allocate-cid (repository)
  (multiple-value-bind (reserved-mapper cid)
      (ordered-mapper/reserve-entry
       (repository/cid-mapper repository))
    (let* ((cid-object
             (make-cid-object
              (repository/cid-distributed-identifier repository cid)
              :cid cid))
           (populated-mapper
             (ordered-mapper/set-entry
              reserved-mapper cid cid-object)))
      (values
       (%remake-repository
        repository :cid-mapper populated-mapper)
       cid
       cid-object))))

(defun repository/next-available-cid (repository)
  (persistent-vector-length
   (ordered-mapper/instance-vector
    (repository/cid-mapper repository))))

(defun repository/last-allocated-cid (repository)
  (cid-master-table/last-allocated-cid
   (repository/cid-master-table repository)))

(defun repository/contains-cid? (repository cid)
  (cid-master-table/contains-cid?
   (repository/cid-master-table repository) cid))

(defun repository/master-cid-set (repository &key end-time)
  (cid-master-table/active-cids
   (repository/identity repository)
   (repository/cid-master-table repository)
   :end-time end-time))

(defun repository/cid-comparison-timestamp (repository cid)
  (cid-master-table/cid-comparison-timestamp
   (repository/cid-master-table repository) cid))

(defun repository/cid-more-recent? (repository cid-1 cid-2)
  (let ((time-1
          (repository/cid-comparison-timestamp repository cid-1))
        (time-2
          (repository/cid-comparison-timestamp repository cid-2)))
    (unless (and time-1 time-2)
      (error "Both CIDs require comparison timestamps."))
    (> time-1 time-2)))

(defun repository/cid-information (repository cid)
  (cid-master-table/cid-information
   (repository/cid-master-table repository) cid))

(defun repository/cid-versioned-change-information
    (repository cid)
  (cid-master-table/cid-versioned-change-information
   (repository/cid-master-table repository) cid))

(defun repository/resolve-distributed-identifier
    (repository did &key (error-if-missing t))
  (check-type did distributed-identifier)
  (unless (and
           (string= (did/domain did) (repository/domain repository))
           (string= (did/repository did) (repository/name repository)))
    (when error-if-missing
      (error "DID ~S belongs to another repository." did))
    (return-from repository/resolve-distributed-identifier nil))
  (if (eq (did/class did) :cid)
      (or (mapper/resolve
           (repository/cid-mapper repository)
           (did/numeric-id did))
          (when error-if-missing
            (error "Repository CID DID ~S is unresolved." did)))
      (distributed-identifier/resolve
       did (repository/root-mapper repository)
       :error-if-missing error-if-missing)))

(defun repository/save (transaction repository)
  (check-type transaction transaction)
  (check-type repository repository)
  (transaction-put
   transaction :repository-sha (repository-sha repository))
  repository)

(defun repository/load (transaction &key error-if-missing)
  (check-type transaction transaction)
  (let ((sha (transaction-get transaction :repository-sha)))
    (cond
      (sha (repository-from-sha sha))
      (error-if-missing
       (error "The transaction has no repository root."))
      (t nil))))
