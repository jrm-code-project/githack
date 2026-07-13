;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass cid-set ()
  ((sha :initarg :sha :reader cid-set-sha :type string)))

(defclass dense-cid-set (cid-set) ())

(deftype cid () '(integer 0 *))

(defstruct (cid-set-data
             (:constructor make-cid-set-data (repository members-sha)))
  repository
  members-sha)

(defun %check-cid (cid)
  (check-type cid (integer 0 *))
  cid)

(defun %make-cid-set-root (repo-ptr repository members-sha)
  (create-tree
   repo-ptr
   (list
    (list "members" members-sha +git-filemode-tree+)
    (list "repository"
          (%stored-object repo-ptr repository)
          +git-filemode-blob+))))

(defun %cid-set-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries)
                        '("members" "repository"))
                 (= (third (first entries)) +git-filemode-tree+)
                 (= (third (second entries)) +git-filemode-blob+))
      (error "Git tree ~S is not a CID set." sha))
    (let ((members-sha (second (first entries))))
      (unless (%persistent-node-null-p repo-ptr members-sha)
        (%persistent-node-data repo-ptr members-sha))
      (make-cid-set-data
       (%loaded-object repo-ptr (second (second entries)))
       members-sha))))

(defun %cid-set-object (repo-ptr repository members-sha)
  (make-instance
   'dense-cid-set
   :sha (%make-cid-set-root repo-ptr repository members-sha)))

(defun cid-set-p (object)
  (typep object 'cid-set))

(defun cid-set? (object)
  (cid-set-p object))

(defun cid-set-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-set-data (current-repository) sha)
    (make-instance 'dense-cid-set :sha sha)))

(defun cid-set/empty (repository)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%cid-set-object
       repo-ptr repository (%persistent-node-null-sha repo-ptr)))))

(defun cid-set/repository (set)
  (check-type set cid-set)
  (with-repository ()
    (cid-set-data-repository
     (%cid-set-data (current-repository) (cid-set-sha set)))))

(defun %cid-set-members-sha (repo-ptr set)
  (cid-set-data-members-sha
   (%cid-set-data repo-ptr (cid-set-sha set))))

(defun %check-cid-set-repositories (left right)
  (unless (equal (cid-set/repository left)
                 (cid-set/repository right))
    (error "CID sets belong to different repositories: ~S and ~S."
           (cid-set/repository left)
           (cid-set/repository right))))

(defun cid-set/adjoin (original cid)
  (check-type original cid-set)
  (%check-cid cid)
  (if (zerop cid)
      original
      (with-repository ()
        (let* ((repo-ptr (current-repository))
               (data (%cid-set-data
                      repo-ptr (cid-set-sha original)))
               (members (cid-set-data-members-sha data)))
          (if (%persistent-node-find repo-ptr #'< members cid)
              original
              (%cid-set-object
               repo-ptr
               (cid-set-data-repository data)
               (%persistent-node-add
                repo-ptr #'< members cid t)))))))

(defun cid-set/remove (original cid)
  (check-type original cid-set)
  (%check-cid cid)
  (if (zerop cid)
      original
      (with-repository ()
        (let* ((repo-ptr (current-repository))
               (data (%cid-set-data
                      repo-ptr (cid-set-sha original)))
               (members (cid-set-data-members-sha data)))
          (if (%persistent-node-find repo-ptr #'< members cid)
              (%cid-set-object
               repo-ptr
               (cid-set-data-repository data)
               (%persistent-node-remove repo-ptr #'< members cid))
              original)))))

(defun cid-set/member (set cid)
  (check-type set cid-set)
  (%check-cid cid)
  (or (zerop cid)
      (with-repository ()
        (let ((repo-ptr (current-repository)))
          (not
           (null
            (%persistent-node-find
             repo-ptr #'<
             (%cid-set-members-sha repo-ptr set)
             cid)))))))

(defun cid-set-count (set)
  (check-type set cid-set)
  (with-repository ()
    (let ((repo-ptr (current-repository)))
      (%persistent-node-size
       repo-ptr (%cid-set-members-sha repo-ptr set)))))

(defun cid-set/empty? (set)
  (zerop (cid-set-count set)))

(defun cid-set->list (set)
  (check-type set cid-set)
  (with-repository ()
    (let ((repo-ptr (current-repository))
          (result nil))
      (%persistent-node-fold
       repo-ptr
       (lambda (ignored node-sha data)
         (declare (ignore ignored node-sha))
         (push (persistent-node-data-key data) result)
         nil)
       nil
       (%cid-set-members-sha repo-ptr set))
      (nreverse result))))

(defun cid-set/equal? (left right)
  (check-type left cid-set)
  (check-type right cid-set)
  (%check-cid-set-repositories left right)
  (if (string= (cid-set-sha left) (cid-set-sha right))
      (values t nil)
      (values (equal (cid-set->list left)
                     (cid-set->list right))
              t)))

(defun %cid-set-binary-operation (left right operation)
  (%check-cid-set-repositories left right)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (left-data (%cid-set-data repo-ptr (cid-set-sha left)))
           (right-data (%cid-set-data repo-ptr (cid-set-sha right)))
           (left-members (cid-set-data-members-sha left-data))
           (right-members (cid-set-data-members-sha right-data))
           (result (funcall operation
                            repo-ptr left-members right-members)))
      (%cid-set-object
       repo-ptr (cid-set-data-repository left-data) result))))

(defun cid-set/union (left right)
  (%cid-set-binary-operation
   left right
   (lambda (repo-ptr left-members right-members)
     (%persistent-node-union
      repo-ptr #'< left-members right-members))))

(defun cid-set/intersection (left right)
  (%cid-set-binary-operation
   left right
   (lambda (repo-ptr left-members right-members)
     (%persistent-node-fold
      repo-ptr
      (lambda (result node-sha data)
        (declare (ignore node-sha))
        (let ((cid (persistent-node-data-key data)))
          (if (%persistent-node-find
               repo-ptr #'< right-members cid)
              (%persistent-node-add
               repo-ptr #'< result cid t)
              result)))
      (%persistent-node-null-sha repo-ptr)
      left-members))))

(defun cid-set/intersection? (left right)
  (not (cid-set/empty? (cid-set/intersection left right))))

(defun cid-set/exclusive-or (left right)
  (%cid-set-binary-operation
   left right
   (lambda (repo-ptr left-members right-members)
     (let ((result (%persistent-node-null-sha repo-ptr)))
       (flet ((add-unique (source other)
                (%persistent-node-fold
                 repo-ptr
                 (lambda (ignored node-sha data)
                   (declare (ignore ignored node-sha))
                   (let ((cid (persistent-node-data-key data)))
                     (unless (%persistent-node-find
                              repo-ptr #'< other cid)
                       (setf result
                             (%persistent-node-add
                              repo-ptr #'< result cid t))))
                   nil)
                 nil source)))
         (add-unique left-members right-members)
         (add-unique right-members left-members))
       result))))

(defun cid-set/highest-active-cid (set)
  (check-type set cid-set)
  (with-repository ()
    (let* ((repo-ptr (current-repository))
           (members (%cid-set-members-sha repo-ptr set)))
      (unless (%persistent-node-null-p repo-ptr members)
        (persistent-node-data-key
         (%persistent-node-data
          repo-ptr
          (%persistent-node-extreme repo-ptr members :right)))))))

(defun cid-set/last-cid (set)
  (or (cid-set/highest-active-cid set) 0))

(defun list->cid-set (repository list)
  (let ((set (cid-set/empty repository)))
    (dolist (cid list set)
      (setf set (cid-set/adjoin set cid)))))

(defun range->cid-set (repository start end)
  (%check-cid start)
  (%check-cid end)
  (when (> start end)
    (error "CID range start ~D exceeds end ~D." start end))
  (let ((set (cid-set/empty repository)))
    (loop for cid from start below end
          do (setf set (cid-set/adjoin set cid)))
    set))
