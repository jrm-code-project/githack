;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass canonical-identifier ()
  ((sha :initarg :sha :reader canonical-identifier-sha :type string)))

(defclass distributed-identifier (canonical-identifier)
  ())

(defun %make-component-vector (repo-ptr components)
  (let ((contents (%persistent-node-null-sha repo-ptr)))
    (loop for component in components
          for index from 0
          do (setf contents
                   (%persistent-node-add
                    repo-ptr #'< contents index component)))
    (%persistent-vector-object
     repo-ptr contents (length components))))

(defun %make-canonical-identifier-root (repo-ptr components)
  (let ((component-vector
          (%make-component-vector repo-ptr components)))
    (create-tree
     repo-ptr
     (list
      (list "components"
            (persistent-vector-sha component-vector)
            +git-filemode-tree+)))))

(defun %canonical-identifier-components (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 1)
                 (string= (first (first entries)) "components")
                 (= (third (first entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a canonical identifier." sha))
    (let* ((vector-sha (second (first entries)))
           (data (%persistent-vector-data repo-ptr vector-sha))
           (result nil))
      (%persistent-node-fold
       repo-ptr
       (lambda (ignored node-sha node-data)
         (declare (ignore ignored node-sha))
         (push (persistent-node-data-value node-data) result)
         nil)
       nil
       (persistent-vector-data-contents-sha data))
      (nreverse result))))

(defun make-canonical-identifier (components)
  (unless (proper-list-p components)
    (error "Canonical identifier components must be a proper list."))
  (with-repository ()
    (make-instance
     'canonical-identifier
     :sha (%make-canonical-identifier-root
           (current-repository) components))))

(defun canonical-identifier-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%canonical-identifier-components (current-repository) sha)
    (make-instance 'canonical-identifier :sha sha)))

(defun canonical-identifier-equal-p (left right)
  (and (typep left 'canonical-identifier)
       (typep right 'canonical-identifier)
       (equal (canonical-identifier-components left)
              (canonical-identifier-components right))))

(defun %check-did-components (domain repository class numeric-id)
  (check-type domain (or null string))
  (check-type repository string)
  (when (zerop (length repository))
    (error "A distributed identifier requires a repository component."))
  (check-type class (or null keyword))
  (check-type numeric-id (integer 0 *))
  (when (and (null class) (plusp numeric-id))
    (error "A numeric ID requires a class component."))
  (values (or domain "") repository class numeric-id))

(defun %make-distributed-identifier-root
    (repo-ptr domain repository class numeric-id)
  (multiple-value-bind (domain repository class numeric-id)
      (%check-did-components domain repository class numeric-id)
    (create-tree
     repo-ptr
     (list
      (list "class" (%stored-object repo-ptr class)
            +git-filemode-blob+)
      (list "domain" (%stored-object repo-ptr domain)
            +git-filemode-blob+)
      (list "numeric-id" (%stored-object repo-ptr numeric-id)
            +git-filemode-blob+)
      (list "repository" (%stored-object repo-ptr repository)
            +git-filemode-blob+)))))

(defun %distributed-identifier-components (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 4)
                 (equal (mapcar #'first entries)
                        '("class" "domain" "numeric-id" "repository"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  entries))
      (error "Git tree ~S is not a distributed identifier." sha))
    (let ((class (%loaded-object repo-ptr (second (first entries))))
          (domain (%loaded-object repo-ptr (second (second entries))))
          (numeric-id
            (%loaded-object repo-ptr (second (third entries))))
          (repository
            (%loaded-object repo-ptr (second (fourth entries)))))
      (%check-did-components domain repository class numeric-id)
      (list domain repository class numeric-id))))

(defun make-distributed-identifier
    (&key domain repository class (numeric-id 0))
  (with-repository ()
    (make-instance
     'distributed-identifier
     :sha (%make-distributed-identifier-root
           (current-repository)
           domain repository class numeric-id))))

(defun distributed-identifier? (object)
  (typep object 'distributed-identifier))

(defun distributed-identifier-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%distributed-identifier-components (current-repository) sha)
    (make-instance 'distributed-identifier :sha sha)))

(defun canonical-identifier-components (identifier)
  (check-type identifier canonical-identifier)
  (with-repository ()
    (if (typep identifier 'distributed-identifier)
        (%distributed-identifier-components
         (current-repository)
         (canonical-identifier-sha identifier))
        (%canonical-identifier-components
         (current-repository)
         (canonical-identifier-sha identifier)))))

(defun did->list (did)
  (check-type did distributed-identifier)
  (canonical-identifier-components did))

(defun list->did (components)
  (unless (and (proper-list-p components)
               (= (length components) 4))
    (error "A canonical DID form must contain four components."))
  (make-distributed-identifier
   :domain (first components)
   :repository (second components)
   :class (third components)
   :numeric-id (fourth components)))

(defun did/domain (did)
  (first (did->list did)))

(defun did/repository (did)
  (second (did->list did)))

(defun did/class (did)
  (third (did->list did)))

(defun did/numeric-id (did)
  (fourth (did->list did)))

(defun did/locale (did)
  (make-distributed-identifier
   :domain (did/domain did)
   :repository (did/repository did)))

(defun %write-did-repository (repository stream)
  (if (find #\. repository)
      (prin1 repository stream)
      (write-string repository stream)))

(defun did->string (did &optional stream)
  (check-type did distributed-identifier)
  (let ((writer
          (lambda (output)
            (unless (string= (did/domain did) "")
              (%write-did-repository (did/domain did) output)
              (write-char #\. output))
            (%write-did-repository (did/repository did) output)
            (when (did/class did)
              (format output ".~(~A~).~D"
                      (did/class did) (did/numeric-id did))))))
    (if stream
        (progn (funcall writer stream) did)
        (with-output-to-string (output)
          (funcall writer output)))))

(defun %did-tokens (string start end)
  (let ((tokens nil)
        (token-start start)
        (index start)
        (quoted-p nil))
    (labels ((emit (limit)
               (let ((token (subseq string token-start limit)))
                 (push
                  (if (and (>= (length token) 2)
                           (char= (char token 0) #\")
                           (char= (char token (1- (length token))) #\"))
                      (read-from-string token)
                      token)
                  tokens))))
      (loop while (< index end)
            for character = (char string index)
            do (cond
                 ((char= character #\")
                  (setf quoted-p (not quoted-p)))
                 ((and (char= character #\.) (not quoted-p))
                  (emit index)
                  (setf token-start (1+ index))))
               (incf index))
      (when quoted-p
        (error "Missing closing quote in distributed identifier ~S."
               string))
      (emit end)
      (nreverse tokens))))

(defun parse-did
    (string &key (start 0) (end (length string))
                 junk-allowed brackets-allowed)
  (declare (ignore junk-allowed))
  (check-type string string)
  (when (and brackets-allowed
             (< start end)
             (char= (char string start) #\[)
             (char= (char string (1- end)) #\]))
    (incf start)
    (decf end))
  (let ((tokens (%did-tokens string start end)))
    (case (length tokens)
      (1
       (make-distributed-identifier :repository (first tokens)))
      (2
       (make-distributed-identifier
        :domain (first tokens) :repository (second tokens)))
      (3
       (make-distributed-identifier
        :repository (first tokens)
        :class (intern (string-upcase (second tokens)) "KEYWORD")
        :numeric-id (parse-integer (third tokens))))
      (4
       (make-distributed-identifier
        :domain (first tokens)
        :repository (second tokens)
        :class (intern (string-upcase (third tokens)) "KEYWORD")
        :numeric-id (parse-integer (fourth tokens))))
      (otherwise
       (error "Distributed identifier ~S has invalid component count."
              string)))))

(defmethod print-object ((did distributed-identifier) stream)
  (write-char #\[ stream)
  (did->string did stream)
  (write-char #\] stream))
