;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(defclass integer-mapper ()
  ((sha :initarg :sha :reader integer-mapper-sha :type string)))

(defclass integer-range-mapper (integer-mapper) ())

(defclass integer-range-mapper-entry ()
  ((sha
    :initarg :sha
    :reader integer-range-mapper-entry-sha
    :type string)))

(defstruct (integer-range-entry-data
             (:constructor make-integer-range-entry-data
                 (local-start local-limit remote-start
                  repository-mapper-sha)))
  local-start local-limit remote-start repository-mapper-sha)

(defstruct (integer-range-mapper-data
             (:constructor make-integer-range-mapper-data
                 (mapping-level pseudo-class next-integer
                  current-entry-sha entries-sha)))
  mapping-level pseudo-class next-integer current-entry-sha entries-sha)

(defun %make-integer-range-entry-root
    (repo-ptr local-start local-limit remote-start
     repository-mapper-sha)
  (check-type local-start (integer 1 *))
  (check-type local-limit (integer 1 *))
  (check-type remote-start (integer 1 *))
  (unless (<= local-start local-limit)
    (error "Invalid local integer range [~D, ~D)."
           local-start local-limit))
  (%mapper-data repo-ptr repository-mapper-sha)
  (create-tree
   repo-ptr
   (list
    (list "local-limit" (%stored-object repo-ptr local-limit)
          +git-filemode-blob+)
    (list "local-start" (%stored-object repo-ptr local-start)
          +git-filemode-blob+)
    (list "remote-start" (%stored-object repo-ptr remote-start)
          +git-filemode-blob+)
    (list "repository-mapper" repository-mapper-sha
          +git-filemode-tree+))))

(defun %integer-range-entry-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless
        (and (= (length entries) 4)
             (equal (mapcar #'first entries)
                    '("local-limit" "local-start"
                      "remote-start" "repository-mapper"))
             (every
              (lambda (entry)
                (= (third entry) +git-filemode-blob+))
              (subseq entries 0 3))
             (= (third (fourth entries)) +git-filemode-tree+))
      (error "Git tree ~S is not an integer range entry." sha))
    (let ((local-limit
            (%loaded-object repo-ptr (second (first entries))))
          (local-start
            (%loaded-object repo-ptr (second (second entries))))
          (remote-start
            (%loaded-object repo-ptr (second (third entries))))
          (repository-mapper-sha (second (fourth entries))))
      (check-type local-start (integer 1 *))
      (check-type local-limit (integer 1 *))
      (check-type remote-start (integer 1 *))
      (unless (<= local-start local-limit)
        (error "Integer range entry ~S is reversed." sha))
      (%mapper-data repo-ptr repository-mapper-sha)
      (make-integer-range-entry-data
       local-start local-limit remote-start
       repository-mapper-sha))))

(defun make-integer-range-mapper-entry
    (&key local-start local-limit remote-start repository-mapper)
  (check-type repository-mapper mapper)
  (let ((limit (or local-limit local-start)))
    (with-repository ()
      (make-instance
       'integer-range-mapper-entry
       :sha (%make-integer-range-entry-root
             (current-repository) local-start limit remote-start
             (mapper-sha repository-mapper))))))

(defun integer-range-mapper-entry-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%integer-range-entry-data (current-repository) sha)
    (make-instance 'integer-range-mapper-entry :sha sha)))

(defmacro define-integer-range-entry-reader (name accessor)
  `(defun ,name (entry)
     (check-type entry integer-range-mapper-entry)
     (with-repository ()
       (,accessor
        (%integer-range-entry-data
         (current-repository)
         (integer-range-mapper-entry-sha entry))))))

(define-integer-range-entry-reader
 integer-range-mapper-entry/local-start
 integer-range-entry-data-local-start)
(define-integer-range-entry-reader
 integer-range-mapper-entry/local-limit
 integer-range-entry-data-local-limit)
(define-integer-range-entry-reader
 integer-range-mapper-entry/remote-start
 integer-range-entry-data-remote-start)

(defun integer-range-mapper-entry/repository-mapper (entry)
  (with-repository ()
    (mapper-from-sha
     (integer-range-entry-data-repository-mapper-sha
      (%integer-range-entry-data
       (current-repository)
       (integer-range-mapper-entry-sha entry))))))

(defun integer-range-mapper-entry/remote-limit (entry)
  (+ (integer-range-mapper-entry/remote-start entry)
     (- (integer-range-mapper-entry/local-limit entry)
        (integer-range-mapper-entry/local-start entry))))

(defun integer-range-mapper-entry/contains-local-integer
    (entry integer)
  (and (<= (integer-range-mapper-entry/local-start entry) integer)
       (< integer (integer-range-mapper-entry/local-limit entry))
       entry))

(defun integer-range-mapper-entry/translate-local-integer
    (entry integer)
  (unless
      (integer-range-mapper-entry/contains-local-integer entry integer)
    (error "Local integer ~D is outside the mapper entry." integer))
  (+ (integer-range-mapper-entry/remote-start entry)
     (- integer
        (integer-range-mapper-entry/local-start entry))))

(defun %same-mapper-p (left right)
  (string= (mapper-sha left) (mapper-sha right)))

(defun integer-range-mapper-entry/contains-remote-integer
    (entry integer repository-mapper)
  (and
   (%same-mapper-p
    repository-mapper
    (integer-range-mapper-entry/repository-mapper entry))
   (<= (integer-range-mapper-entry/remote-start entry) integer)
   (< integer (integer-range-mapper-entry/remote-limit entry))
   entry))

(defun integer-range-mapper-entry/translate-remote-integer
    (entry integer)
  (unless
      (and
       (<= (integer-range-mapper-entry/remote-start entry) integer)
       (< integer (integer-range-mapper-entry/remote-limit entry)))
    (error "Remote integer ~D is outside the mapper entry." integer))
  (+ (integer-range-mapper-entry/local-start entry)
     (- integer
        (integer-range-mapper-entry/remote-start entry))))

(defun %validate-integer-range-entry-vector (repo-ptr sha)
  (let* ((data (%persistent-vector-data repo-ptr sha))
         (vector (make-instance 'persistent-vector :sha sha)))
    (dotimes (index (persistent-vector-data-length data))
      (%integer-range-entry-data
       repo-ptr (persistent-vector-ref vector index)))
    data))

(defun %make-integer-range-mapper-root
    (repo-ptr mapping-level pseudo-class next-integer
     current-entry-sha entries-sha)
  (check-type mapping-level string)
  (check-type pseudo-class symbol)
  (check-type next-integer (integer 1 *))
  (let ((current
          (%integer-range-entry-data repo-ptr current-entry-sha)))
    (unless (= (integer-range-entry-data-local-limit current)
               next-integer)
      (error "Current integer range does not end at ~D."
             next-integer)))
  (%validate-integer-range-entry-vector repo-ptr entries-sha)
  (create-tree
   repo-ptr
   (list
    (list "current" current-entry-sha +git-filemode-tree+)
    (list "entries" entries-sha +git-filemode-tree+)
    (list "level" (%stored-object repo-ptr mapping-level)
          +git-filemode-blob+)
    (list "next" (%stored-object repo-ptr next-integer)
          +git-filemode-blob+)
    (list "pseudo-class" (%stored-object repo-ptr pseudo-class)
          +git-filemode-blob+)
    (list "type" (%stored-object repo-ptr :integer-range)
          +git-filemode-blob+))))

(defun %integer-range-mapper-data (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless
        (and (= (length entries) 6)
             (equal
              (mapcar #'first entries)
              '("current" "entries" "level" "next"
                "pseudo-class" "type"))
             (= (third (first entries)) +git-filemode-tree+)
             (= (third (second entries)) +git-filemode-tree+)
             (every
              (lambda (entry)
                (= (third entry) +git-filemode-blob+))
              (subseq entries 2)))
      (error "Git tree ~S is not an integer range mapper." sha))
    (let ((current-sha (second (first entries)))
          (entries-sha (second (second entries)))
          (level (%loaded-object repo-ptr (second (third entries))))
          (next (%loaded-object repo-ptr (second (fourth entries))))
          (pseudo-class
            (%loaded-object repo-ptr (second (fifth entries))))
          (type (%loaded-object repo-ptr (second (sixth entries)))))
      (unless (eq type :integer-range)
        (error "Invalid integer mapper type ~S." type))
      (check-type level string)
      (check-type next (integer 1 *))
      (check-type pseudo-class symbol)
      (let ((current
              (%integer-range-entry-data repo-ptr current-sha)))
        (unless (= (integer-range-entry-data-local-limit current)
                   next)
          (error "Integer mapper ~S has inconsistent next value." sha)))
      (%validate-integer-range-entry-vector repo-ptr entries-sha)
      (make-integer-range-mapper-data
       level pseudo-class next current-sha entries-sha))))

(defun make-integer-range-mapper
    (&key (mapping-level "Unknown") pseudo-class repository-mapper)
  (check-type repository-mapper mapper)
  (let* ((current
           (make-integer-range-mapper-entry
            :local-start 1 :local-limit 1 :remote-start 1
            :repository-mapper repository-mapper))
         (entries (make-persistent-vector 0)))
    (with-repository ()
      (make-instance
       'integer-range-mapper
       :sha (%make-integer-range-mapper-root
             (current-repository) mapping-level pseudo-class 1
             (integer-range-mapper-entry-sha current)
             (persistent-vector-sha entries))))))

(defun integer-range-mapper-from-sha (sha)
  (check-type sha string)
  (with-repository ()
    (%integer-range-mapper-data (current-repository) sha)
    (make-instance 'integer-range-mapper :sha sha)))

(defmacro define-integer-range-mapper-reader (name accessor)
  `(defun ,name (mapper)
     (check-type mapper integer-range-mapper)
     (with-repository ()
       (,accessor
        (%integer-range-mapper-data
         (current-repository) (integer-mapper-sha mapper))))))

(define-integer-range-mapper-reader
 integer-mapper/mapping-level
 integer-range-mapper-data-mapping-level)
(define-integer-range-mapper-reader
 integer-mapper/pseudo-class
 integer-range-mapper-data-pseudo-class)
(define-integer-range-mapper-reader
 integer-range-mapper/next-available-integer
 integer-range-mapper-data-next-integer)

(defun integer-range-mapper/last-allocated-integer (mapper)
  (1- (integer-range-mapper/next-available-integer mapper)))

(defun integer-range-mapper/current-entry (mapper)
  (with-repository ()
    (integer-range-mapper-entry-from-sha
     (integer-range-mapper-data-current-entry-sha
      (%integer-range-mapper-data
       (current-repository) (integer-mapper-sha mapper))))))

(defun integer-range-mapper/entries (mapper)
  (with-repository ()
    (let* ((data
             (%integer-range-mapper-data
              (current-repository) (integer-mapper-sha mapper)))
           (vector
             (make-instance
              'persistent-vector
              :sha (integer-range-mapper-data-entries-sha data))))
      (loop for index below (persistent-vector-length vector)
            collect
            (integer-range-mapper-entry-from-sha
             (persistent-vector-ref vector index))))))

(defun %integer-range-mapper-object
    (mapping-level pseudo-class next current entries)
  (let ((entry-vector
          (make-persistent-vector
           (length entries)
           :initial-contents
           (mapcar #'integer-range-mapper-entry-sha entries))))
    (with-repository ()
      (make-instance
       'integer-range-mapper
       :sha (%make-integer-range-mapper-root
             (current-repository) mapping-level pseudo-class next
             (integer-range-mapper-entry-sha current)
             (persistent-vector-sha entry-vector))))))

(defgeneric integer-mapper/allocate-integers
    (mapper count repository-map &optional remote-integer))

(defmethod integer-mapper/allocate-integers
    ((mapper integer-range-mapper) count repository-map
     &optional remote-integer)
  (check-type count (integer 1 *))
  (check-type repository-map mapper)
  (let* ((next
           (integer-range-mapper/next-available-integer mapper))
         (current (integer-range-mapper/current-entry mapper))
         (same-repository-p
           (%same-mapper-p
            repository-map
            (integer-range-mapper-entry/repository-mapper current)))
         (expected-remote
           (integer-range-mapper-entry/remote-limit current))
         (extend-p
           (and same-repository-p
                (or (null remote-integer)
                    (= remote-integer expected-remote))))
         (new-current
           (if extend-p
               (make-integer-range-mapper-entry
                :local-start
                (integer-range-mapper-entry/local-start current)
                :local-limit (+ next count)
                :remote-start
                (integer-range-mapper-entry/remote-start current)
                :repository-mapper repository-map)
               (make-integer-range-mapper-entry
                :local-start next
                :local-limit (+ next count)
                :remote-start (or remote-integer next)
                :repository-mapper repository-map)))
         (old-entries
           (integer-range-mapper/entries mapper))
         (entries
           (if extend-p old-entries
               (append old-entries (list current)))))
    (values
     (%integer-range-mapper-object
      (integer-mapper/mapping-level mapper)
      (integer-mapper/pseudo-class mapper)
      (+ next count) new-current entries)
     next)))

(defun integer-mapper/allocate-integer
    (mapper repository-map &optional remote-integer)
  (integer-mapper/allocate-integers
   mapper 1 repository-map remote-integer))

(defun %integer-range-all-entries (mapper)
  (append (integer-range-mapper/entries mapper)
          (list (integer-range-mapper/current-entry mapper))))

(defun integer-range-mapper/resolve-remote-reference
    (mapper remote-repository-reference remote-key
     &key return-entry)
  (let ((entry
          (find-if
           (lambda (candidate)
             (integer-range-mapper-entry/contains-remote-integer
              candidate remote-key remote-repository-reference))
           (%integer-range-all-entries mapper))))
    (if return-entry entry
        (and entry
             (integer-range-mapper-entry/translate-remote-integer
              entry remote-key)))))

(defgeneric integer-mapper/resolve-distributed-identifier
    (mapper did repository-map))

(defmethod integer-mapper/resolve-distributed-identifier
    ((mapper integer-range-mapper) did repository-map)
  (integer-range-mapper/resolve-remote-reference
   mapper repository-map (did/numeric-id did)))

(defgeneric integer-mapper/distributed-identifier
    (mapper integer))

(defmethod integer-mapper/distributed-identifier
    ((mapper integer-range-mapper) integer)
  (let ((entry
          (find-if
           (lambda (candidate)
             (integer-range-mapper-entry/contains-local-integer
              candidate integer))
           (%integer-range-all-entries mapper))))
    (unless entry
      (error "Integer ~D is not mapped." integer))
    (let ((prefix
            (mapper-distributed-identifier
             (integer-range-mapper-entry/repository-mapper entry))))
      (make-distributed-identifier
       :domain (did/domain prefix)
       :repository (did/repository prefix)
       :class (integer-mapper/pseudo-class mapper)
       :numeric-id
       (integer-range-mapper-entry/translate-local-integer
        entry integer)))))
