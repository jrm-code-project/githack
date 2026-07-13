;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

(define-foreign-library libgit2
  (:windows (:or "git2.dll" "d:\\lib\\git2.dll"))
  (t (:default "libgit2")))

(use-foreign-library libgit2)

(defcfun ("git_libgit2_init" git-libgit2-init) :int)
(defcfun ("git_libgit2_shutdown" git-libgit2-shutdown) :int)

;; Automatically initialize libgit2 on load
(git-libgit2-init)

(defcstruct git-oid
  (id :unsigned-char :count 20))

(defcfun ("git_blob_create_from_buffer" git-blob-create-from-buffer) :int
  (id :pointer)
  (repo :pointer)
  (buffer :pointer)
  (len :size))

(defcfun ("git_oid_tostr" git-oid-tostr) :pointer
  (out :pointer)
  (n :size)
  (id :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *repository-pathname* #p"D:\\GitHack\\"))

(defcfun ("git_repository_open" git-repository-open) :int
  (out :pointer)
  (path :string))

(defcfun ("git_repository_init" git-repository-init) :int
  (out :pointer)
  (path :string)
  (is-bare :unsigned-int))

(defcfun ("git_repository_free" git-repository-free) :void
  (repo :pointer))

(defun open-or-init-repository (pathname &key (bare t))
  "Opens a Git repository at PATHNAME. If one doesn't exist, it initializes it.
   Defaults to a BARE repository because this is an object database, not a workspace.
   Returns a CFFI pointer to the git_repository."
  ;; Convert the Lisp pathname to a native string (important for Windows/libgit2)
  (let ((path-str (uiop:native-namestring pathname)))
    (with-foreign-object (repo-ptr-ptr :pointer)
      ;; First, try to just open it
      (let ((err (git-repository-open repo-ptr-ptr path-str)))
        (if (= err 0)
            (mem-ref repo-ptr-ptr :pointer)
            ;; If open fails (usually returns GIT_ENOTFOUND), initialize it
            (let ((init-err (git-repository-init repo-ptr-ptr path-str (if bare 1 0))))
              (if (= init-err 0)
                  (mem-ref repo-ptr-ptr :pointer)
                  (error "libgit2 error opening/initing repo at ~A: code ~D" path-str init-err))))))))

(defvar *repository*)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (documentation '*repository* 'variable)
        "Holds the CFFI pointer to the currently open git_repository.  Use WITH-REPOSITORY to safely bind this variable."))

(defmacro current-repository ()
  "Returns the CFFI pointer to the currently open git_repository.
   Throws an error if *repository* is not bound."
  `(if (null-pointer-p *repository*)
       (error "No repository is currently open. Use WITH-REPOSITORY to open one.")
       *repository*))

(defmacro with-repository ((&key (pathname '*repository-pathname* supplied-p) (bare t)) &body body)
  "Safely opens/inits a git repository, binds the C pointer to REPO-VAR,
   and ensures the pointer is freed when execution leaves the block."
  (let ((path-form (if supplied-p pathname '*repository-pathname*)))
    `(let ((*repository* (open-or-init-repository ,path-form :bare ,bare)))
       (unwind-protect
            (progn ,@body)
         (unless (null-pointer-p *repository*)
           (git-repository-free *repository*))))))

(defun store-octet-vector-as-blob (repo-ptr octet-vector)
  "Takes a CFFI pointer to an open git_repository and a Lisp (simple-array (unsigned-byte 8) (*)).
   Writes the bytes to the Git object database and returns the 40-character SHA string."
  ;; Make sure it's a simple array so CFFI can get a direct pointer to the memory
  (check-type octet-vector (simple-array (unsigned-byte 8) (*)))
  (let ((len (length octet-vector)))
    (with-foreign-object (oid '(:struct git-oid))
      ;; Pin the Lisp vector and get a raw C pointer to its data
      (with-pointer-to-vector-data (buf-ptr octet-vector)
        (let ((err-code (git-blob-create-from-buffer oid repo-ptr buf-ptr len)))
          (when (< err-code 0)
            (error "libgit2 error writing blob: code ~D" err-code))

          ;; Now convert the 20-byte OID struct to a readable 40-character hex string
          (with-foreign-object (hex-buf :char 41) ; 40 chars + null terminator
            (git-oid-tostr hex-buf 41 oid)
            (foreign-string-to-lisp hex-buf)))))))

(defcfun ("git_oid_fromstr" git-oid-fromstr) :int
  (out :pointer)
  (str :string))

;; int git_blob_lookup(git_blob **blob, git_repository *repo, const git_oid *id);
(defcfun ("git_blob_lookup" git-blob-lookup) :int
  (blob :pointer)
  (repo :pointer)
  (id :pointer))

;; const void * git_blob_rawcontent(const git_blob *blob);
(defcfun ("git_blob_rawcontent" git-blob-rawcontent) :pointer
  (blob :pointer))

;; git_off_t git_blob_rawsize(const git_blob *blob);
;; Note: git_off_t is typically a 64-bit integer (int64)
(defcfun ("git_blob_rawsize" git-blob-rawsize) :int64
  (blob :pointer))

;; void git_blob_free(git_blob *blob);
(defcfun ("git_blob_free" git-blob-free) :void
  (blob :pointer))

(defun retrieve-blob-as-octet-vector (repo-ptr sha-string)
  "Looks up a blob by its 40-character SHA-1 string in the given repository.
   Returns a newly allocated Lisp (simple-array (unsigned-byte 8) (*)) containing the data."
  (unless (= (length sha-string) 40)
    (error "Invalid SHA string: must be exactly 40 characters."))

  (with-foreign-object (oid '(:struct git-oid))
    ;; 1. Convert hex string to raw 20-byte OID
    (let ((err (git-oid-fromstr oid sha-string)))
      (when (< err 0)
        (error "libgit2 error parsing SHA ~S: code ~D" sha-string err)))

    (with-foreign-object (blob-ptr-ptr :pointer)
      ;; 2. Look up the blob in the database
      (let ((err (git-blob-lookup blob-ptr-ptr repo-ptr oid)))
        (when (< err 0)
          (error "libgit2 error finding blob ~S: code ~D" sha-string err)))

      ;; 3. Grab the actual blob pointer
      (let ((blob-ptr (mem-ref blob-ptr-ptr :pointer)))
        (unwind-protect
             ;; 4. Get the size and the raw memory pointer
             (let* ((size (git-blob-rawsize blob-ptr))
                    (raw-data-ptr (git-blob-rawcontent blob-ptr))
                    ;; Allocate our Lisp vector
                    (lisp-vector (make-array size :element-type '(unsigned-byte 8))))

               ;; 5. Blit the memory from C-land into Lisp-land
               ;; CFFI provides a handy loop for this, but for speed on massive blobs,
               ;; you could theoretically map it, but copying is safer for now.
               (loop for i from 0 below size
                     do (setf (aref lisp-vector i) (mem-aref raw-data-ptr :unsigned-char i)))

               ;; Return the Lisp vector
               lisp-vector)

          ;; 6. Free the libgit2 blob structure (crucial to prevent C memory leaks)
          (unless (null-pointer-p blob-ptr)
            (git-blob-free blob-ptr)))))))

(defun octect-vector->oid (octect-vector)
  (with-repository ()
    (store-octet-vector-as-blob (current-repository) octect-vector)))

(defun oid->octect-vector (oid)
  (with-repository ()
    (retrieve-blob-as-octet-vector (current-repository) oid)))

(defcfun ("git_treebuilder_new" git-treebuilder-new) :int
  (out :pointer)
  (repo :pointer)
  (source :pointer))

(defcfun ("git_treebuilder_insert" git-treebuilder-insert) :int
  (out :pointer)
  (builder :pointer)
  (filename :string)
  (id :pointer)
  (filemode :unsigned-int))

(defcfun ("git_treebuilder_write" git-treebuilder-write) :int
  (id :pointer)
  (builder :pointer))

(defcfun ("git_treebuilder_free" git-treebuilder-free) :void
  (builder :pointer))

(defcfun ("git_tree_lookup" git-tree-lookup) :int
  (out :pointer)
  (repo :pointer)
  (id :pointer))

(defcfun ("git_tree_entrycount" git-tree-entrycount) :size
  (tree :pointer))

(defcfun ("git_tree_entry_byindex" git-tree-entry-byindex) :pointer
  (tree :pointer)
  (index :size))

(defcfun ("git_tree_entry_name" git-tree-entry-name) :string
  (entry :pointer))

(defcfun ("git_tree_entry_id" git-tree-entry-id) :pointer
  (entry :pointer))

(defcfun ("git_tree_entry_filemode" git-tree-entry-filemode) :unsigned-int
  (entry :pointer))

(defcfun ("git_tree_free" git-tree-free) :void
  (tree :pointer))

(defconstant +git-filemode-blob+ #o100644)
(defconstant +git-filemode-tree+ #o040000)

(defun oid-pointer->string (oid)
  (with-foreign-object (hex-buffer :char 41)
    (git-oid-tostr hex-buffer 41 oid)
    (foreign-string-to-lisp hex-buffer)))

(defun create-tree (repo-ptr entries)
  "Writes ENTRIES of the form (NAME SHA FILEMODE) and returns the tree SHA."
  (with-foreign-object (builder-out :pointer)
    (let ((error-code
            (git-treebuilder-new builder-out repo-ptr (null-pointer))))
      (when (< error-code 0)
        (error "libgit2 error creating tree builder: code ~D" error-code)))
    (let ((builder (mem-ref builder-out :pointer)))
      (unwind-protect
           (progn
             (dolist (entry entries)
               (destructuring-bind (name sha filemode) entry
                 (with-foreign-object (oid '(:struct git-oid))
                   (let ((error-code (git-oid-fromstr oid sha)))
                     (when (< error-code 0)
                       (error "libgit2 error parsing SHA ~S: code ~D"
                              sha error-code)))
                   (let ((error-code
                           (git-treebuilder-insert
                            (null-pointer) builder name oid filemode)))
                     (when (< error-code 0)
                       (error "libgit2 error inserting ~S into tree: code ~D"
                              name error-code))))))
             (with-foreign-object (tree-oid '(:struct git-oid))
               (let ((error-code (git-treebuilder-write tree-oid builder)))
                 (when (< error-code 0)
                   (error "libgit2 error writing tree: code ~D" error-code)))
               (oid-pointer->string tree-oid)))
        (unless (null-pointer-p builder)
          (git-treebuilder-free builder))))))

(defun read-tree (repo-ptr sha)
  "Reads SHA and returns entries of the form (NAME SHA FILEMODE)."
  (with-foreign-object (oid '(:struct git-oid))
    (let ((error-code (git-oid-fromstr oid sha)))
      (when (< error-code 0)
        (error "libgit2 error parsing SHA ~S: code ~D" sha error-code)))
    (with-foreign-object (tree-out :pointer)
      (let ((error-code (git-tree-lookup tree-out repo-ptr oid)))
        (when (< error-code 0)
          (error "Git object ~S is not a tree: libgit2 code ~D"
                 sha error-code)))
      (let ((tree (mem-ref tree-out :pointer)))
        (unwind-protect
             (loop for index below (git-tree-entrycount tree)
                   for entry = (git-tree-entry-byindex tree index)
                   collect (list (git-tree-entry-name entry)
                                 (oid-pointer->string
                                  (git-tree-entry-id entry))
                                 (git-tree-entry-filemode entry)))
          (unless (null-pointer-p tree)
            (git-tree-free tree)))))))

(defun %persistent-null (repo-ptr)
  (create-tree repo-ptr nil))

(defun persistent-null ()
  "Ensures the persistent NULL tree exists and returns its SHA."
  (with-repository ()
    (%persistent-null (current-repository))))

(defun persistent-cons-entries (repo-ptr sha)
  (let ((entries (read-tree repo-ptr sha)))
    (unless (and (= (length entries) 2)
                 (equal (mapcar #'first entries) '("car" "cdr"))
                 (= (third (first entries)) +git-filemode-blob+)
                 (= (third (second entries)) +git-filemode-tree+))
      (error "Git tree ~S is not a persistent CONS cell." sha))
    entries))

(defun %persistent-cons (repo-ptr car cdr)
  (let ((car-sha
          (store-octet-vector-as-blob repo-ptr (object->octet-vector car))))
    ;; Reading CDR first both validates it as a tree and gives malformed links
    ;; an immediate, useful error.
    (read-tree repo-ptr cdr)
    (create-tree repo-ptr
                 (list (list "car" car-sha +git-filemode-blob+)
                       (list "cdr" cdr +git-filemode-tree+)))))

(defun persistent-cons (car cdr)
  "Returns the SHA of a persistent CONS whose CDR is a persistent tree SHA."
  (with-repository ()
    (%persistent-cons (current-repository) car cdr)))

(defun persistent-car (persistent-cons-sha)
  "Returns the Lisp CAR stored in PERSISTENT-CONS-SHA."
  (with-repository ()
    (let* ((entries
             (persistent-cons-entries
              (current-repository) persistent-cons-sha))
           (car-sha (second (first entries))))
      (octet-vector->object
       (retrieve-blob-as-octet-vector (current-repository) car-sha)))))

(defun persistent-cdr (persistent-cons-sha)
  "Returns the persistent CDR tree SHA stored in PERSISTENT-CONS-SHA."
  (with-repository ()
    (second
     (second
      (persistent-cons-entries
       (current-repository) persistent-cons-sha)))))

(defun list->persistent-cons (list)
  "Persists a proper Lisp LIST and returns its first CONS SHA, or NULL SHA."
  (unless (proper-list-p list)
    (error "Only proper Lisp lists can be persisted, not ~S." list))
  (with-repository ()
    (let ((tail (%persistent-null (current-repository))))
      (dolist (item (reverse list) tail)
        (setf tail (%persistent-cons (current-repository) item tail))))))

(defun persistent-cons->list (sha)
  "Resolves a persistent CONS chain (or persistent NULL) into a Lisp list."
  (with-repository ()
    (let ((null-sha (%persistent-null (current-repository)))
          (result nil))
      (loop until (string-equal sha null-sha)
            do (let* ((entries
                        (persistent-cons-entries
                         (current-repository) sha))
                      (car-sha (second (first entries))))
                 (push
                  (octet-vector->object
                   (retrieve-blob-as-octet-vector
                    (current-repository) car-sha))
                  result)
                 (setf sha (second (second entries)))))
      (nreverse result))))

(defparameter *my-data*
  (make-array 5 :element-type '(unsigned-byte 8)
                :initial-contents '(104 101 108 108 111)))

;;;; =========================================================================
;;;; Transaction System
;;;; =========================================================================

(defun object->octet-vector (obj)
  "Serializes a Lisp object to a UTF-8 octet vector."
  (sb-ext:string-to-octets (prin1-to-string obj) :external-format :utf-8))

(defun octet-vector->object (vec)
  "Deserializes a UTF-8 octet vector back to a Lisp object."
  (read-from-string (sb-ext:octets-to-string vec :external-format :utf-8)))

(defun get-master-branch-sha ()
  "Reads the current commit SHA of the master branch from refs/heads/master."
  (let ((path (merge-pathnames "refs/heads/master" *repository-pathname*)))
    (if (probe-file path)
        (string-right-trim '(#\Space #\Newline #\Return)
                           (alexandria:read-file-into-string path))
        nil)))

(defun get-branch-sha (branch-name)
  "Reads the current commit SHA of the given branch from refs/heads/<branch-name>."
  (let ((path (merge-pathnames (format nil "refs/heads/~A" branch-name) *repository-pathname*)))
    (if (probe-file path)
        (string-right-trim '(#\Space #\Newline #\Return)
                           (alexandria:read-file-into-string path))
        nil)))

(defun deserialize-branch-state (branch-name)
  "Retrieves and deserializes the root tree state object from the commit at the given branch's tip."
  (let ((commit-sha (get-branch-sha branch-name)))
    (when commit-sha
      (get-root-state-from-commit commit-sha))))

(defun set-master-branch-sha (sha)
  "Updates refs/heads/master to point to the new commit SHA."
  (let ((path (merge-pathnames "refs/heads/master" *repository-pathname*)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string sha stream)
      (terpri stream))
    sha))

(defun get-root-state-from-commit (commit-sha)
  "Retrieves the root database state object associated with the given commit SHA."
  (let* ((commit-octet (oid->octect-vector commit-sha))
         (commit-obj (octet-vector->object commit-octet))
         (root-sha (getf commit-obj :root))
         (root-octet (oid->octect-vector root-sha)))
    (octet-vector->object root-octet)))

(defstruct transaction
  (status :active) ; :active, :committed, :aborted
  user
  metainformation
  state)

(defun transaction-get (tx key)
  "Retrieves the value for KEY from the transaction's current state (an alist)."
  (cdr (assoc key (transaction-state tx))))

(defun transaction-put (tx key value)
  "Stores KEY and VALUE in the transaction's current state (an alist)."
  (let ((entry (assoc key (transaction-state tx))))
    (if entry
        (setf (cdr entry) value)
        (push (cons key value) (transaction-state tx))))
  value)

(defun commit-transaction-logic (tx)
  "Serializes the root state and commit objects, writes them to git, and updates master."
  (let* ((new-root (transaction-state tx))
         (new-root-octet (object->octet-vector new-root))
         (new-root-sha (octect-vector->oid new-root-octet))
         (parent-sha (get-master-branch-sha))
         (commit-obj (list :parent parent-sha
                           :root new-root-sha
                           :user (transaction-user tx)
                           :meta (transaction-metainformation tx)
                           :timestamp (get-universal-time)))
         (commit-octet (object->octet-vector commit-obj))
         (new-commit-sha (octect-vector->oid commit-octet)))
    (set-master-branch-sha new-commit-sha)
    new-commit-sha))

(defun transaction/abort (tx)
  "Aborts the transaction immediately by marking it as aborted and throwing to call-with-transaction."
  (setf (transaction-status tx) :aborted)
  (throw 'transaction-exit tx))

(defun transaction/commit (tx)
  "Commits the transaction immediately by writing changes to git and throwing to call-with-transaction."
  (unless (eq (transaction-status tx) :active)
    (error "Transaction is not active (status is ~S)." (transaction-status tx)))
  (commit-transaction-logic tx)
  (setf (transaction-status tx) :committed)
  (throw 'transaction-exit tx))

(defun call-with-transaction (user metainformation receiver)
  "Executes RECEIVER inside a transaction context.
   If RECEIVER exits normally, the transaction commits.
   If RECEIVER does a throw or signals an error, the transaction aborts.
   RECEIVER can also call transaction/commit or transaction/abort."
  (let* ((parent-commit (get-master-branch-sha))
         (initial-state (if parent-commit
                            (get-root-state-from-commit parent-commit)
                            nil))
         (tx (make-transaction :status :active
                               :user user
                               :metainformation metainformation
                               :state (copy-tree initial-state)))
         (finished nil))
    (catch 'transaction-exit
      (unwind-protect
           (progn
             (funcall receiver tx)
             ;; If we reach here, the receiver exited normally and finished.
             ;; Check if status is still active (just in case they didn't throw on commit/abort)
             (when (eq (transaction-status tx) :active)
               (commit-transaction-logic tx)
               (setf (transaction-status tx) :committed))
             (setf finished t)
             tx)
        ;; Cleanup form
        (unless finished
          (unless (eq (transaction-status tx) :committed)
            (setf (transaction-status tx) :aborted)))))))
