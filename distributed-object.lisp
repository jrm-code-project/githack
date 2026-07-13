;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass distributed-object ()
  ((sha
    :initarg :sha
    :reader distributed-object-sha
    :type string)))

(defclass core-user (distributed-object) ())

(defstruct (distributed-object-data
             (:constructor make-distributed-object-data
                 (type repository-mapper-sha numeric-id name)))
  type repository-mapper-sha numeric-id name)

(defun %distributed-object-class-key (class)
  (intern (symbol-name class) "KEYWORD"))

(defun %make-core-user-root
    (repo-ptr repository-mapper-sha numeric-id name)
  (%mapper-data repo-ptr repository-mapper-sha)
  (check-type numeric-id (integer 1 *))
  (check-type name string)
  (create-tree
   repo-ptr
   (list
    (list "name" (%stored-object repo-ptr name)
          +git-filemode-blob+)
    (list "numeric-id" (%stored-object repo-ptr numeric-id)
          +git-filemode-blob+)
    (list "repository-mapper" repository-mapper-sha
          +git-filemode-tree+)
    (list "type" (%stored-object repo-ptr :core-user)
          +git-filemode-blob+))))

(defun %distributed-object-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless
        (and (= (length entries) 4)
             (equal
              (mapcar #'first entries)
              '("name" "numeric-id" "repository-mapper" "type"))
             (= (third (first entries)) +git-filemode-blob+)
             (= (third (second entries)) +git-filemode-blob+)
             (= (third (third entries)) +git-filemode-tree+)
             (= (third (fourth entries)) +git-filemode-blob+))
      (error "Git tree ~S is not a distributed object." sha))
    (let ((name (%loaded-object repo-ptr (second (first entries))))
          (numeric-id
            (%loaded-object repo-ptr (second (second entries))))
          (repository-mapper-sha (second (third entries)))
          (type (%loaded-object repo-ptr (second (fourth entries)))))
      (unless (eq type :core-user)
        (error "Unknown distributed object type ~S." type))
      (check-type name string)
      (check-type numeric-id (integer 1 *))
      (%mapper-data repo-ptr repository-mapper-sha)
      (make-distributed-object-data
       type repository-mapper-sha numeric-id name))))

(defun distributed-object-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (let ((data
            (%distributed-object-data
             (current-repository) sha)))
      (make-instance
       (ecase (distributed-object-data-type data)
         (:core-user 'core-user))
       :sha sha))))

(defun distributed-object? (object)
  (typep object 'distributed-object))

(defun distributed-object/repository-mapper (object)
  (check-type object distributed-object)
  (with-repository ()
    (mapper-from-sha
     (distributed-object-data-repository-mapper-sha
      (%distributed-object-data
       (current-repository)
       (distributed-object-sha object))))))

(defun distributed-object/numeric-id (object)
  (check-type object distributed-object)
  (with-repository ()
    (distributed-object-data-numeric-id
     (%distributed-object-data
      (current-repository)
      (distributed-object-sha object)))))

(defgeneric distributed-object-identifier (object))

(defmethod distributed-object-identifier
    ((object distributed-object))
  (let ((prefix
          (mapper-distributed-identifier
           (distributed-object/repository-mapper object))))
    (make-distributed-identifier
     :domain (did/domain prefix)
     :repository (did/repository prefix)
     :class (%distributed-object-class-key (type-of object))
     :numeric-id (distributed-object/numeric-id object))))

(defun core-user/name (user)
  (check-type user core-user)
  (with-repository ()
    (distributed-object-data-name
     (%distributed-object-data
      (current-repository)
      (distributed-object-sha user)))))

(defun %repository-root-with-local-mapper
    (repository updated-local)
  (let* ((root (repository/root-mapper repository))
         (domain-key (repository/domain repository))
         (repository-key (repository/name repository))
         (domain (mapper/resolve root domain-key))
         (updated-domain
           (unordered-mapper/set-entry
            domain repository-key updated-local))
         (updated-root
           (unordered-mapper/set-entry
            root domain-key updated-domain)))
    (repository/with-mappers
     repository updated-root updated-local)))

(defun make-core-user (repository name)
  "Returns an updated repository and its newly allocated CORE-USER."
  (check-type repository repository)
  (check-type name string)
  (let* ((local (repository/local-mapper repository))
         (class-key :core-user)
         (existing (mapper/resolve local class-key))
         (class-mapper
           (or existing
               (make-ordered-mapper
                :mapping-level "Class CORE-USER"
                :key class-key :parent local))))
    (unless (typep class-mapper 'ordered-mapper)
      (error "CORE-USER mapper is not ordered."))
    (multiple-value-bind (reserved numeric-id)
        (ordered-mapper/reserve-entry class-mapper)
      (let* ((user
               (with-repository ()
                 (make-instance
                  'core-user
                  :sha (%make-core-user-root
                        (current-repository)
                        (mapper-sha local) numeric-id name))))
             (populated
               (ordered-mapper/set-entry reserved numeric-id user))
             (updated-local
               (unordered-mapper/set-entry
                local class-key populated))
             (updated-repository
               (%repository-root-with-local-mapper
                repository updated-local)))
        (values updated-repository user)))))

(defun repository/resolve-core-user (repository did)
  (let ((object
          (repository/resolve-distributed-identifier repository did)))
    (check-type object core-user)
    object))
