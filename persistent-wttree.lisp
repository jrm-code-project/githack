;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass persistent-node ()
  ((sha :initarg :sha :reader persistent-node-sha :type string)))

(defclass persistent-wttree-table (table:table)
  ((test :initarg :test :initform 'equal :reader persistent-wttree-test)))

(defclass persistent-immutable-wttree
    (persistent-wttree-table table::immutable-table)
  ())

(defstruct (persistent-node-data
             (:constructor make-persistent-node-data
                 (key value left right weight)))
  key
  value
  left
  right
  weight)

(defun %stored-object (repo-ptr object)
  (store-octet-vector-as-blob repo-ptr (object->octet-vector object)))

(defun %loaded-object (repo-ptr sha)
  (octet-vector->object (retrieve-blob-as-octet-vector repo-ptr sha)))

(defun %persistent-node-null-sha (repo-ptr)
  (%persistent-null repo-ptr))

(defun %persistent-node-null-p (repo-ptr sha)
  (string-equal sha (%persistent-node-null-sha repo-ptr)))

(defun %persistent-node-reference (repo-ptr node)
  (etypecase node
    (null (%persistent-node-null-sha repo-ptr))
    (persistent-node (persistent-node-sha node))
    (string node)))

(defun %persistent-node-object (repo-ptr sha)
  (unless (%persistent-node-null-p repo-ptr sha)
    (make-instance 'persistent-node :sha sha)))

(defun %persistent-node-data (repo-ptr sha)
  (when (%persistent-node-null-p repo-ptr sha)
    (error "Persistent NULL is not a node."))
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 5)
                 (equal (mapcar #'first entries)
                        '("k" "l" "r" "v" "w"))
                 (= (third (first entries)) +git-filemode-blob+)
                 (= (third (second entries)) +git-filemode-tree+)
                 (= (third (third entries)) +git-filemode-tree+)
                 (= (third (fourth entries)) +git-filemode-blob+)
                 (= (third (fifth entries)) +git-filemode-blob+))
      (error "Git tree ~S is not a persistent node." sha))
    (make-persistent-node-data
     (%loaded-object repo-ptr (second (first entries)))
     (%loaded-object repo-ptr (second (fourth entries)))
     (second (second entries))
     (second (third entries))
     (%loaded-object repo-ptr (second (fifth entries))))))

(defun %persistent-node-size (repo-ptr sha)
  (if (%persistent-node-null-p repo-ptr sha)
      0
      (persistent-node-data-weight (%persistent-node-data repo-ptr sha))))

(defun %make-persistent-node (repo-ptr key value left right)
  (let ((weight (+ 1
                   (%persistent-node-size repo-ptr left)
                   (%persistent-node-size repo-ptr right))))
    (create-tree
     repo-ptr
     (list (list "k" (%stored-object repo-ptr key) +git-filemode-blob+)
           (list "l" left +git-filemode-tree+)
           (list "r" right +git-filemode-tree+)
           (list "v" (%stored-object repo-ptr value) +git-filemode-blob+)
           (list "w" (%stored-object repo-ptr weight) +git-filemode-blob+)))))

(defun %log2-less-p (left right)
  (and (< left right)
       (< (ash (logand left right) 1) right)))

(defun %weight-too-small-p (small large)
  (%log2-less-p small (ash large -1)))

(defun %single-rotation-p (inner outer)
  (not (%log2-less-p outer inner)))

(defun %persistent-node-join (repo-ptr key value left right)
  (let ((left-weight (%persistent-node-size repo-ptr left))
        (right-weight (%persistent-node-size repo-ptr right)))
    (cond
      ((%weight-too-small-p left-weight right-weight)
       (let* ((right-data (%persistent-node-data repo-ptr right))
              (inner (persistent-node-data-left right-data))
              (outer (persistent-node-data-right right-data)))
         (if (%single-rotation-p
              (%persistent-node-size repo-ptr inner)
              (%persistent-node-size repo-ptr outer))
             (%make-persistent-node
              repo-ptr
              (persistent-node-data-key right-data)
              (persistent-node-data-value right-data)
              (%make-persistent-node repo-ptr key value left inner)
              outer)
             (let ((inner-data (%persistent-node-data repo-ptr inner)))
               (%make-persistent-node
                repo-ptr
                (persistent-node-data-key inner-data)
                (persistent-node-data-value inner-data)
                (%make-persistent-node
                 repo-ptr key value left
                 (persistent-node-data-left inner-data))
                (%make-persistent-node
                 repo-ptr
                 (persistent-node-data-key right-data)
                 (persistent-node-data-value right-data)
                 (persistent-node-data-right inner-data)
                 outer))))))
      ((%weight-too-small-p right-weight left-weight)
       (let* ((left-data (%persistent-node-data repo-ptr left))
              (outer (persistent-node-data-left left-data))
              (inner (persistent-node-data-right left-data)))
         (if (%single-rotation-p
              (%persistent-node-size repo-ptr inner)
              (%persistent-node-size repo-ptr outer))
             (%make-persistent-node
              repo-ptr
              (persistent-node-data-key left-data)
              (persistent-node-data-value left-data)
              outer
              (%make-persistent-node repo-ptr key value inner right))
             (let ((inner-data (%persistent-node-data repo-ptr inner)))
               (%make-persistent-node
                repo-ptr
                (persistent-node-data-key inner-data)
                (persistent-node-data-value inner-data)
                (%make-persistent-node
                 repo-ptr
                 (persistent-node-data-key left-data)
                 (persistent-node-data-value left-data)
                 outer
                 (persistent-node-data-left inner-data))
                (%make-persistent-node
                 repo-ptr key value
                 (persistent-node-data-right inner-data) right))))))
      (t
       (%make-persistent-node repo-ptr key value left right)))))

(defun %persistent-node-add (repo-ptr key-less-p sha key value)
  (if (%persistent-node-null-p repo-ptr sha)
      (%make-persistent-node
       repo-ptr key value sha sha)
      (let* ((data (%persistent-node-data repo-ptr sha))
             (node-key (persistent-node-data-key data))
             (left (persistent-node-data-left data))
             (right (persistent-node-data-right data)))
        (cond
          ((funcall key-less-p key node-key)
           (%persistent-node-join
            repo-ptr node-key (persistent-node-data-value data)
            (%persistent-node-add repo-ptr key-less-p left key value)
            right))
          ((funcall key-less-p node-key key)
           (%persistent-node-join
            repo-ptr node-key (persistent-node-data-value data)
            left
            (%persistent-node-add repo-ptr key-less-p right key value)))
          (t
           (%make-persistent-node repo-ptr node-key value left right))))))

(defun %persistent-node-find (repo-ptr key-less-p sha key)
  (loop until (%persistent-node-null-p repo-ptr sha)
        for data = (%persistent-node-data repo-ptr sha)
        for node-key = (persistent-node-data-key data)
        do (cond
             ((funcall key-less-p key node-key)
              (setf sha (persistent-node-data-left data)))
             ((funcall key-less-p node-key key)
              (setf sha (persistent-node-data-right data)))
             (t (return sha)))))

(defun %persistent-node-child (data direction)
  (ecase direction
    (:left (persistent-node-data-left data))
    (:right (persistent-node-data-right data))))

(defun %persistent-node-extreme (repo-ptr sha direction)
  (when (%persistent-node-null-p repo-ptr sha)
    (error "Empty persistent node."))
  (loop for data = (%persistent-node-data repo-ptr sha)
        for next = (%persistent-node-child data direction)
        until (%persistent-node-null-p repo-ptr next)
        do (setf sha next)
        finally (return sha)))

(defun %persistent-node-remove-extreme (repo-ptr sha direction)
  (when (%persistent-node-null-p repo-ptr sha)
    (error "Empty persistent node."))
  (let* ((data (%persistent-node-data repo-ptr sha))
         (left (persistent-node-data-left data))
         (right (persistent-node-data-right data))
         (next (%persistent-node-child data direction)))
    (if (%persistent-node-null-p repo-ptr next)
        (ecase direction
          (:left right)
          (:right left))
        (%persistent-node-join
         repo-ptr
         (persistent-node-data-key data)
         (persistent-node-data-value data)
         (if (eq direction :left)
             (%persistent-node-remove-extreme repo-ptr left direction)
             left)
         (if (eq direction :right)
             (%persistent-node-remove-extreme repo-ptr right direction)
             right)))))

(defun %persistent-node-concat (repo-ptr left right)
  (cond
    ((%persistent-node-null-p repo-ptr left) right)
    ((%persistent-node-null-p repo-ptr right) left)
    (t
     (let* ((minimum (%persistent-node-extreme
                      repo-ptr right :left))
            (data (%persistent-node-data repo-ptr minimum)))
       (%persistent-node-join
        repo-ptr
        (persistent-node-data-key data)
        (persistent-node-data-value data)
        left
        (%persistent-node-remove-extreme
         repo-ptr right :left))))))

(defun %persistent-node-remove (repo-ptr key-less-p sha key)
  (if (%persistent-node-null-p repo-ptr sha)
      sha
      (let* ((data (%persistent-node-data repo-ptr sha))
             (node-key (persistent-node-data-key data))
             (left (persistent-node-data-left data))
             (right (persistent-node-data-right data)))
        (cond
          ((funcall key-less-p key node-key)
           (%persistent-node-join
            repo-ptr node-key (persistent-node-data-value data)
            (%persistent-node-remove repo-ptr key-less-p left key)
            right))
          ((funcall key-less-p node-key key)
           (%persistent-node-join
            repo-ptr node-key (persistent-node-data-value data)
            left
            (%persistent-node-remove repo-ptr key-less-p right key)))
          (t (%persistent-node-concat repo-ptr left right))))))

(defun %persistent-node-fold (repo-ptr procedure initial sha)
  (if (%persistent-node-null-p repo-ptr sha)
      initial
      (let ((data (%persistent-node-data repo-ptr sha)))
        (%persistent-node-fold
         repo-ptr procedure
         (funcall
          procedure
          (%persistent-node-fold
           repo-ptr procedure initial
           (persistent-node-data-left data))
          sha data)
         (persistent-node-data-right data)))))

(defun %persistent-node-split (repo-ptr key-less-p sha pivot keep-p)
  (%persistent-node-fold
   repo-ptr
   (lambda (result node-sha data)
     (declare (ignore node-sha))
     (if (funcall keep-p key-less-p
                  (persistent-node-data-key data) pivot)
         (%persistent-node-add
          repo-ptr key-less-p result
          (persistent-node-data-key data)
          (persistent-node-data-value data))
         result))
   (%persistent-node-null-sha repo-ptr)
   sha))

(defun make-persistent-node (key value left right)
  "Creates an immutable persistent node and returns its SHA wrapper."
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%make-persistent-node
        repo-ptr key value
        (%persistent-node-reference repo-ptr left)
        (%persistent-node-reference repo-ptr right))))))

(defun persistent-node-key (node)
  (with-repository ()
    (persistent-node-data-key
     (%persistent-node-data
      (current-repository) (persistent-node-sha node)))))

(defun persistent-node-value (node)
  (with-repository ()
    (persistent-node-data-value
     (%persistent-node-data
      (current-repository) (persistent-node-sha node)))))

(defun persistent-node-left (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (persistent-node-data-left
        (%persistent-node-data repo-ptr (persistent-node-sha node)))))))

(defun persistent-node-right (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (persistent-node-data-right
        (%persistent-node-data repo-ptr (persistent-node-sha node)))))))

(defun persistent-node-weight (node)
  (with-repository ()
    (%persistent-node-size
     (current-repository) (persistent-node-sha node))))

(defun persistent-node-size (node)
  (if (null node) 0 (persistent-node-weight node)))

(defun persistent-node-singleton (key value)
  (make-persistent-node key value nil nil))

(defun persistent-node-add (key-less-p node key value)
  (check-type key-less-p function)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-add
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr node)
        key value)))))

(defun persistent-node-find (key-less-p node key)
  (check-type key-less-p function)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (found (%persistent-node-find
                   repo-ptr key-less-p
                   (%persistent-node-reference repo-ptr node) key)))
      (and found (%persistent-node-object repo-ptr found)))))

(defun persistent-node-minimum (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-extreme
        repo-ptr (%persistent-node-reference repo-ptr node)
        :left)))))

(defun persistent-node-maximum (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-extreme
        repo-ptr (%persistent-node-reference repo-ptr node)
        :right)))))

(defun persistent-node-remove-minimum (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-remove-extreme
        repo-ptr (%persistent-node-reference repo-ptr node)
        :left)))))

(defun persistent-node-remove-maximum (node)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-remove-extreme
        repo-ptr (%persistent-node-reference repo-ptr node)
        :right)))))

(defun persistent-node-remove (key-less-p node key)
  (check-type key-less-p function)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-remove
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr node) key)))))

(defun persistent-node-inorder-fold (procedure initial node)
  (check-type procedure function)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-fold
       repo-ptr
       (lambda (result sha data)
         (declare (ignore data))
         (funcall procedure result
                  (make-instance 'persistent-node :sha sha)))
       initial
       (%persistent-node-reference repo-ptr node)))))

(defun persistent-node-keys (node)
  (nreverse
   (persistent-node-inorder-fold
    (lambda (keys item) (cons (persistent-node-key item) keys))
    nil node)))

(defun persistent-node-values (node)
  (nreverse
   (persistent-node-inorder-fold
    (lambda (values item) (cons (persistent-node-value item) values))
    nil node)))

(defun persistent-node-split-lt (key-less-p node pivot)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-split
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr node) pivot
        (lambda (less-p key boundary)
          (funcall less-p key boundary)))))))

(defun persistent-node-split-gt (key-less-p node pivot)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-split
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr node) pivot
        (lambda (less-p key boundary)
          (funcall less-p boundary key)))))))

(defun %persistent-node-union
    (repo-ptr key-less-p left right &optional merge)
  (%persistent-node-fold
   repo-ptr
   (lambda (result node-sha data)
     (declare (ignore node-sha))
     (let* ((key (persistent-node-data-key data))
            (right-value (persistent-node-data-value data))
            (left-node (%persistent-node-find
                        repo-ptr key-less-p result key))
            (value
              (if (and merge left-node)
                  (funcall
                   merge key right-value
                   (persistent-node-data-value
                    (%persistent-node-data repo-ptr left-node)))
                  right-value)))
       (%persistent-node-add
        repo-ptr key-less-p result key value)))
   left right))

(defun persistent-node-union (key-less-p left right)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-union
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr left)
        (%persistent-node-reference repo-ptr right))))))

(defun persistent-node-union-merge (key-less-p left right merge)
  (check-type merge function)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-object
       repo-ptr
       (%persistent-node-union
        repo-ptr key-less-p
        (%persistent-node-reference repo-ptr left)
        (%persistent-node-reference repo-ptr right)
        merge)))))

(defun persistent-node-subset-p
    (key-less-p left right &optional (value-test #'eql))
  (with-repository ()
    (let ((repo-ptr (current-repository))
          (answer t)
          (right-sha nil))
      (setf right-sha (%persistent-node-reference repo-ptr right))
      (%persistent-node-fold
       repo-ptr
       (lambda (ignored node-sha data)
         (declare (ignore ignored node-sha))
         (let ((match
                 (%persistent-node-find
                  repo-ptr key-less-p right-sha
                  (persistent-node-data-key data))))
           (unless
               (and match
                    (funcall
                     value-test
                     (persistent-node-data-value data)
                     (persistent-node-data-value
                      (%persistent-node-data repo-ptr match))))
             (setf answer nil)))
         nil)
       nil
       (%persistent-node-reference repo-ptr left))
      answer)))

(defun %persistent-wttree-key-less-p (table)
  (ecase (table:table/test table)
    (equal #'table::less)
    (equalp #'table::lessp)))

(defun %persistent-wttree-root-sha (repo-ptr table)
  (%persistent-node-reference repo-ptr (table:representation table)))

(defun %persistent-wttree-with-root (table repo-ptr root-sha)
  (make-instance
   (class-of table)
   :metadata (copy-list (table:metadata table))
   :representation (%persistent-node-object repo-ptr root-sha)
   :test (table:table/test table)))

(defmethod shared-initialize :after
    ((instance persistent-wttree-table) slot-names
     &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((representation-supplied-p (member :representation initargs))
        (initial-contents-supplied-p (member :initial-contents initargs))
        (representation (getf initargs :representation))
        (initial-contents (getf initargs :initial-contents)))
    (when (and representation-supplied-p initial-contents-supplied-p)
      (error "Specify either :REPRESENTATION or :INITIAL-CONTENTS, not both."))
    (when representation-supplied-p
      (with-repository ()
        (let* ((repo-ptr (current-repository))
               (sha (%persistent-node-reference repo-ptr representation)))
          (unless (%persistent-node-null-p repo-ptr sha)
            (%persistent-node-data repo-ptr sha))
          (setf (slot-value instance 'table::representation)
                (%persistent-node-object repo-ptr sha)))))
    (when initial-contents-supplied-p
      (let ((entries
              (etypecase initial-contents
                (list
                 (mapcar
                  (lambda (entry)
                    (unless (consp entry)
                      (error "Expected an alist entry, got ~S." entry))
                    (cons (car entry) (cdr entry)))
                  initial-contents))
                (table:table
                 (nreverse
                  (table:fold-table
                   (lambda (entries key value)
                     (acons key value entries))
                   nil initial-contents))))))
        (with-repository ()
          (let* ((repo-ptr (current-repository))
                 (key-less-p (%persistent-wttree-key-less-p instance))
                 (root (%persistent-node-null-sha repo-ptr)))
            (dolist (entry entries)
              (setf root
                    (%persistent-node-add
                     repo-ptr key-less-p root
                     (car entry) (cdr entry))))
            (setf (slot-value instance 'table::representation)
                  (%persistent-node-object repo-ptr root))))))))

(defmethod table:table/test ((table persistent-wttree-table))
  (persistent-wttree-test table))

(defun persistent-wttree-sha (table)
  "Returns the root node SHA, or the persistent NULL SHA for an empty tree."
  (check-type table persistent-wttree-table)
  (with-repository ()
    (%persistent-wttree-root-sha (current-repository) table)))

(defun make-persistent-wttree-table (&rest initargs)
  (apply #'make-instance 'persistent-wttree-table initargs))

(defun make-persistent-immutable-wttree (&rest initargs)
  (apply #'make-instance 'persistent-immutable-wttree initargs))

(defmethod table:fold-table
    (procedure initial (table persistent-wttree-table))
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-fold
       repo-ptr
       (lambda (result sha data)
         (declare (ignore sha))
         (funcall procedure result
                  (persistent-node-data-key data)
                  (persistent-node-data-value data)))
       initial
       (%persistent-wttree-root-sha repo-ptr table)))))

(defmethod table:table/clear ((table persistent-wttree-table))
  (make-instance
   (class-of table)
   :metadata (copy-list (table:metadata table))
   :test (table:table/test table)))

(defmethod table:table/clear! ((table persistent-wttree-table))
  (setf (table:representation table) nil)
  table)

(defmethod table:table/copy ((table persistent-wttree-table))
  (make-instance
   (class-of table)
   :metadata (copy-list (table:metadata table))
   :representation (table:representation table)
   :test (table:table/test table)))

(defmethod table:table/insert
    ((table persistent-wttree-table) key value)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (root
             (%persistent-node-add
              repo-ptr (%persistent-wttree-key-less-p table)
              (%persistent-wttree-root-sha repo-ptr table)
              key value)))
      (%persistent-wttree-with-root table repo-ptr root))))

(defmethod table:table/insert!
    ((table persistent-wttree-table) key value)
  (setf (table:representation table)
        (table:representation (table:table/insert table key value)))
  table)

(defmethod table:table/remove
    ((table persistent-wttree-table) &rest keys)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (key-less-p (%persistent-wttree-key-less-p table))
           (root (%persistent-wttree-root-sha repo-ptr table)))
      (dolist (key keys)
        (setf root
              (%persistent-node-remove
               repo-ptr key-less-p root key)))
      (%persistent-wttree-with-root table repo-ptr root))))

(defmethod table:table/remove!
    ((table persistent-wttree-table) &rest keys)
  (setf (table:representation table)
        (table:representation
         (apply #'table:table/remove table keys)))
  table)

(defmethod table:table/delete
    ((table persistent-wttree-table) &rest keys)
  (apply #'table:table/remove! table keys))

(defmethod table::table/delete-keys
    ((table persistent-wttree-table) keys)
  (apply #'table:table/remove! table keys))

(defmethod table:table/lookup
    ((table persistent-wttree-table) key &optional default)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (found
             (%persistent-node-find
              repo-ptr (%persistent-wttree-key-less-p table)
              (%persistent-wttree-root-sha repo-ptr table)
              key)))
      (if found
          (persistent-node-data-value
           (%persistent-node-data repo-ptr found))
          default))))

(defmethod table:table/size ((table persistent-wttree-table))
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-size
       repo-ptr (%persistent-wttree-root-sha repo-ptr table)))))

(defmethod table:table/keys ((table persistent-wttree-table))
  (let ((result nil))
    (table:fold-table
     (lambda (ignored key value)
       (declare (ignore ignored value))
       (push key result))
     nil table)
    (nreverse result)))

(defmethod table:table/values ((table persistent-wttree-table))
  (let ((result nil))
    (table:fold-table
     (lambda (ignored key value)
       (declare (ignore ignored key))
       (push value result))
     nil table)
    (nreverse result)))

(defmethod table:table->alist ((table persistent-wttree-table))
  (let ((result nil))
    (table:fold-table
     (lambda (ignored key value)
       (declare (ignore ignored))
       (push (cons key value) result))
     nil table)
    (nreverse result)))

(defmethod table:table->plist ((table persistent-wttree-table))
  (let ((result nil))
    (table:fold-table
     (lambda (ignored key value)
       (declare (ignore ignored))
       (setf result (list* value key result)))
     nil table)
    (nreverse result)))

(defmethod table:table->hash-table
    ((table persistent-wttree-table) &rest initargs)
  (let ((result (apply #'make-hash-table initargs)))
    (table:fold-table
     (lambda (ignored key value)
       (declare (ignore ignored))
       (setf (gethash key result) value))
     nil table)
    result))

(defun %persistent-wttree-extreme (table direction)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (sha
             (%persistent-node-extreme
              repo-ptr (%persistent-wttree-root-sha repo-ptr table)
              direction))
           (data (%persistent-node-data repo-ptr sha)))
      (values (persistent-node-data-key data)
              (persistent-node-data-value data)))))

(defmethod table:table/minimum ((table persistent-wttree-table))
  (%persistent-wttree-extreme table :left))

(defmethod table:table/maximum ((table persistent-wttree-table))
  (%persistent-wttree-extreme table :right))

(defun %persistent-wttree-pop (table direction)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (root (%persistent-wttree-root-sha repo-ptr table))
           (extreme (%persistent-node-extreme repo-ptr root direction))
           (data (%persistent-node-data repo-ptr extreme))
           (new-root
             (%persistent-node-remove-extreme repo-ptr root direction)))
      (values
       (persistent-node-data-key data)
       (persistent-node-data-value data)
       (%persistent-wttree-with-root table repo-ptr new-root)))))

(defmethod table:table/pop-minimum ((table persistent-wttree-table))
  (%persistent-wttree-pop table :left))

(defmethod table:table/pop-maximum ((table persistent-wttree-table))
  (%persistent-wttree-pop table :right))

(defmethod table:table/pop-minimum! ((table persistent-wttree-table))
  (multiple-value-bind (key value result)
      (table:table/pop-minimum table)
    (setf (table:representation table) (table:representation result))
    (values key value table)))

(defmethod table:table/pop-maximum! ((table persistent-wttree-table))
  (multiple-value-bind (key value result)
      (table:table/pop-maximum table)
    (setf (table:representation table) (table:representation result))
    (values key value table)))

(defun %persistent-wttree-split (table pivot keep-p)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (root
             (%persistent-node-split
              repo-ptr (%persistent-wttree-key-less-p table)
              (%persistent-wttree-root-sha repo-ptr table)
              pivot keep-p)))
      (%persistent-wttree-with-root table repo-ptr root))))

(defmethod table:table/split-lt
    ((table persistent-wttree-table) pivot)
  (%persistent-wttree-split
   table pivot
   (lambda (less-p key boundary)
     (funcall less-p key boundary))))

(defmethod table:table/split-gt
    ((table persistent-wttree-table) pivot)
  (%persistent-wttree-split
   table pivot
   (lambda (less-p key boundary)
     (funcall less-p boundary key))))

(defmethod table:table/subset?
    ((sub persistent-wttree-table)
     (super persistent-wttree-table)
     &optional (value-test #'eql))
  (and (eq (table:table/test sub) (table:table/test super))
       (persistent-node-subset-p
        (%persistent-wttree-key-less-p sub)
        (table:representation sub)
        (table:representation super)
        value-test)))

(defun %persistent-wttree-union (left right merge)
  (unless (eq (table:table/test left) (table:table/test right))
    (error "Persistent WTTREE tests differ: ~S and ~S."
           (table:table/test left) (table:table/test right)))
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (root
             (%persistent-node-union
              repo-ptr (%persistent-wttree-key-less-p left)
              (%persistent-wttree-root-sha repo-ptr left)
              (%persistent-wttree-root-sha repo-ptr right)
              merge)))
      (%persistent-wttree-with-root left repo-ptr root))))

(defmethod table:table/union
    ((left persistent-wttree-table) (right persistent-wttree-table))
  (%persistent-wttree-union left right nil))

(defmethod table:table/union!
    ((left persistent-wttree-table) (right persistent-wttree-table))
  (setf (table:representation left)
        (table:representation (table:table/union left right)))
  left)

(defmethod table:table/union-merge
    ((left persistent-wttree-table)
     (right persistent-wttree-table)
     merge)
  (%persistent-wttree-union left right merge))

(defmethod table:table/union-merge!
    ((left persistent-wttree-table)
     (right persistent-wttree-table)
     merge)
  (setf (table:representation left)
        (table:representation
         (table:table/union-merge left right merge)))
  left)
