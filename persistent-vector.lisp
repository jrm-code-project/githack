;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass persistent-vector ()
  ((sha :initarg :sha :reader persistent-vector-sha :type string)))

(defstruct (persistent-vector-data
             (:constructor make-persistent-vector-data
                 (contents-sha length-tree-sha length)))
  contents-sha
  length-tree-sha
  length)

(defun %make-persistent-vector-length-tree (repo-ptr length)
  (check-type length (integer 0 *))
  (create-tree
   repo-ptr
   (list
    (list "length"
          (%stored-object repo-ptr length)
          +git-filemode-blob+))))

(defun %persistent-vector-length-from-tree (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 1)
                 (string= (first (first entries)) "length")
                 (= (third (first entries)) +git-filemode-blob+))
      (error "Git tree ~S is not a persistent vector length tree." sha))
    (let ((length (%loaded-object repo-ptr (second (first entries)))))
      (unless (typep length '(integer 0 *))
        (error "Persistent vector length tree ~S contains invalid length ~S."
               sha length))
      length)))

(defun %persistent-vector-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("contents" "length"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (second entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a persistent vector." sha))
    (let ((contents-sha (second (first entries)))
          (length-tree-sha (second (second entries))))
      (unless (%persistent-node-null-p repo-ptr contents-sha)
        (%persistent-node-data repo-ptr contents-sha))
      (let ((length
              (%persistent-vector-length-from-tree
               repo-ptr length-tree-sha)))
        (unless (= (%persistent-node-size repo-ptr contents-sha) length)
          (error "Persistent vector ~S has length ~D but ~D stored values."
                 sha length
                 (%persistent-node-size repo-ptr contents-sha)))
        (make-persistent-vector-data
         contents-sha length-tree-sha length)))))

(defun %make-persistent-vector-root (repo-ptr contents-sha length)
  (let ((length-tree-sha
          (%make-persistent-vector-length-tree repo-ptr length)))
    (create-tree
     repo-ptr
     (list (list "contents" contents-sha +git-filemode-tree+)
           (list "length" length-tree-sha +git-filemode-tree+)))))

(defun %persistent-vector-object (repo-ptr contents-sha length)
  (make-instance
   'persistent-vector
   :sha (%make-persistent-vector-root repo-ptr contents-sha length)))

(defun make-persistent-vector
    (length &key
              (initial-element nil initial-element-supplied-p)
              (initial-contents nil initial-contents-supplied-p))
  "Creates a persistent vector of LENGTH backed by an integer-keyed tree."
  (check-type length (integer 0 *))
  (when (and initial-element-supplied-p initial-contents-supplied-p)
    (error "Specify either :INITIAL-ELEMENT or :INITIAL-CONTENTS, not both."))
  (when (and initial-contents-supplied-p
             (/= (length initial-contents) length))
    (error "Initial contents length ~D does not match vector length ~D."
           (length initial-contents) length))
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (contents-sha (%persistent-node-null-sha repo-ptr)))
      (dotimes (index length)
        (let ((value
                (if initial-contents-supplied-p
                    (elt initial-contents index)
                    initial-element)))
          (setf contents-sha
                (%persistent-node-add
                 repo-ptr #'< contents-sha index value))))
      (%persistent-vector-object repo-ptr contents-sha length))))

(defun persistent-vector-from-sha (sha)
  "Validates SHA and returns a persistent vector wrapper for it."
  (check-type sha string)
  (with-repository ()
    (%persistent-vector-data (current-repository) sha)
    (make-instance 'persistent-vector :sha sha)))

(defun persistent-vector-length (vector)
  (check-type vector persistent-vector)
  (with-repository ()
    (persistent-vector-data-length
     (%persistent-vector-data
      (current-repository) (persistent-vector-sha vector)))))

(defun persistent-vector-length-tree-sha (vector)
  (check-type vector persistent-vector)
  (with-repository ()
    (persistent-vector-data-length-tree-sha
     (%persistent-vector-data
      (current-repository) (persistent-vector-sha vector)))))

(defun persistent-vector-contents-sha (vector)
  (check-type vector persistent-vector)
  (with-repository ()
    (persistent-vector-data-contents-sha
     (%persistent-vector-data
      (current-repository) (persistent-vector-sha vector)))))

(defun %check-persistent-vector-index (index length)
  (check-type index (integer 0 *))
  (when (>= index length)
    (error "Persistent vector index ~D is outside [0, ~D)."
           index length)))

(defun persistent-vector-ref (vector index)
  "Returns the element at INDEX."
  (check-type vector persistent-vector)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-vector-data
              repo-ptr (persistent-vector-sha vector)))
           (length (persistent-vector-data-length data)))
      (%check-persistent-vector-index index length)
      (let ((node-sha
              (%persistent-node-find
               repo-ptr #'<
               (persistent-vector-data-contents-sha data)
               index)))
        (unless node-sha
          (error "Persistent vector ~S has no value at valid index ~D."
                 (persistent-vector-sha vector) index))
        (persistent-node-data-value
         (%persistent-node-data repo-ptr node-sha))))))

(defun persistent-vector-aref (vector index)
  "Alias for PERSISTENT-VECTOR-REF."
  (persistent-vector-ref vector index))

(defun persistent-vector-update (vector index value)
  "Returns a new persistent vector with VALUE stored at INDEX."
  (check-type vector persistent-vector)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-vector-data
              repo-ptr (persistent-vector-sha vector)))
           (length (persistent-vector-data-length data)))
      (%check-persistent-vector-index index length)
      (%persistent-vector-object
       repo-ptr
       (%persistent-node-add
        repo-ptr #'<
        (persistent-vector-data-contents-sha data)
        index value)
       length))))

(defun persistent-vector-push-extend
    (new-element vector &optional extension)
  "Returns the grown vector and, as a second value, the appended index.
EXTENSION is accepted for VECTOR-PUSH-EXTEND compatibility; persistent vectors
have no separate capacity."
  (check-type vector persistent-vector)
  (when extension
    (check-type extension (integer 1 *)))
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-vector-data
              repo-ptr (persistent-vector-sha vector)))
           (index (persistent-vector-data-length data))
           (new-contents
             (%persistent-node-add
              repo-ptr #'<
              (persistent-vector-data-contents-sha data)
              index new-element)))
      (values
       (%persistent-vector-object repo-ptr new-contents (1+ index))
       index))))

(defun persistent-vector->vector (persistent-vector)
  "Resolves PERSISTENT-VECTOR into a simple Lisp vector."
  (check-type persistent-vector persistent-vector)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (data
             (%persistent-vector-data
              repo-ptr (persistent-vector-sha persistent-vector)))
           (result
             (make-array (persistent-vector-data-length data))))
      (%persistent-node-fold
       repo-ptr
       (lambda (result node-sha node-data)
         (declare (ignore node-sha))
         (setf
          (aref result (persistent-node-data-key node-data))
          (persistent-node-data-value node-data))
         result)
       result
       (persistent-vector-data-contents-sha data)))))
