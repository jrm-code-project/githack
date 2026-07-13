;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass cid-object (canonical-object)
  ((sha :initarg :sha :reader cid-object-sha :type string)))

(defstruct (cid-object-data
             (:constructor make-cid-object-data (did cid)))
  did
  cid)

(defun %cid-object-did-reference (did)
  (if (typep did 'distributed-identifier)
      (list :distributed-identifier (did->list did))
      (list :value did)))

(defun %cid-object-reference-did (reference)
  (unless (and (consp reference)
               (consp (cdr reference))
               (null (cddr reference)))
    (error "Invalid CID object DID reference ~S." reference))
  (ecase (first reference)
    (:distributed-identifier (list->did (second reference)))
    (:value (second reference))))

(defun %check-cid-number (cid)
  (unless (or (null cid)
              (typep cid '(integer 1 *)))
    (error "A resolved CID must be NIL or a positive integer, not ~S." cid))
  cid)

(defun %make-cid-object-root (repo-ptr did cid)
  (%check-cid-number cid)
  (create-tree
   repo-ptr
   (list
    (list "cid" (%stored-object repo-ptr cid) +git-filemode-blob+)
    (list "did"
          (%stored-object repo-ptr (%cid-object-did-reference did))
          +git-filemode-blob+))))

(defun %cid-object-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries) '("cid" "did"))
                 (every
                  (lambda (entry)
                    (= (third entry) +git-filemode-blob+))
                  entries))
      (error "Git tree ~S is not a CID object." sha))
    (let ((cid (%loaded-object repo-ptr (second (first entries))))
          (did
            (%cid-object-reference-did
             (%loaded-object repo-ptr (second (second entries))))))
      (%check-cid-number cid)
      (make-cid-object-data did cid))))

(defun make-cid-object (distributed-identifier &key cid)
  "Creates a canonical persistent CID object."
  (with-repository ()
    (make-instance
     'cid-object
     :sha (%make-cid-object-root
           (current-repository) distributed-identifier cid))))

(defun cid-object-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%cid-object-data (current-repository) sha)
    (make-instance 'cid-object :sha sha)))

(defun cid-object/did (object)
  (check-type object cid-object)
  (with-repository ()
    (cid-object-data-did
     (%cid-object-data
      (current-repository) (cid-object-sha object)))))

(defun cid-object/cid (object)
  (check-type object cid-object)
  (with-repository ()
    (cid-object-data-cid
     (%cid-object-data
      (current-repository) (cid-object-sha object)))))

(defun cid-object-with-cid (object cid)
  "Returns a new CID object with the same DID and resolved local CID."
  (check-type object cid-object)
  (make-cid-object (cid-object/did object) :cid cid))

(defun cid-object-equal-p (left right)
  (and (typep left 'cid-object)
       (typep right 'cid-object)
       (let ((left-did (cid-object/did left))
             (right-did (cid-object/did right)))
         (if (and (typep left-did 'distributed-identifier)
                  (typep right-did 'distributed-identifier))
             (canonical-identifier-equal-p left-did right-did)
             (equal left-did right-did)))))

(defmethod canonical-object-equal-p
    ((left cid-object) (right cid-object))
  (cid-object-equal-p left right))

(defmethod print-object ((object cid-object) stream)
  (print-unreadable-object (object stream :type t)
    (princ (cid-object/did object) stream)))
