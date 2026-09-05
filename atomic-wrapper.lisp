;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Git requires every commit to point at a root *tree*, but a
;;; PERSISTENT-* application object at the root of a commit may
;;; resolve to a single atomic GIT-BLOB rather than any kind of
;;; GIT-TREE (a CLOS object, PERSISTENT-CONS, or persistent vector
;;; all serialize to GIT-TREEs already and need no help; a bare atom
;;; does not). ATOMIC-WRAPPER-TREE bridges that gap with a small,
;;; transparent wrapper tree of exactly three entries --
;;;
;;;   .meta      a blob holding the plist (:TAG :ATOMIC-WRAPPER)
;;;   README.md  a blob explaining the wrapper to a human Git user
;;;   value      a pointer to the actual atomic GIT-BLOB being committed
;;;
;;; -- so that GIT-TRANSACTION can always hand GIT-COMMIT a proper
;;; GIT-TREE, while RESOLVE-COMMIT-ROOT transparently unwraps it back
;;; out again on the read path, making the wrapper invisible to
;;; higher-level Lisp application code.

(defparameter +atomic-wrapper-readme+
  "Structural wrapper required by Git to attach an atomic blob to a commit root."
  "The fixed README.md content WRAP-ATOMIC-COMMIT-ROOT writes,
verbatim and unencoded, into every atomic-wrapper tree.")

(defun wrap-atomic-commit-root (repository git-object)
  "Build, persist, and return the ATOMIC-WRAPPER GIT-TREE for
GIT-OBJECT (an already-persisted GIT-BLOB, or any other GIT-OBJECT
that is not itself a GIT-TREE): a tree with exactly three entries,
\".meta\" (the plist (:TAG :ATOMIC-WRAPPER)), \"README.md\" (fixed
explanatory text), and \"value\" (GIT-OBJECT itself) -- transparently
reversed by RESOLVE-COMMIT-ROOT. Signals an error if GIT-OBJECT has
no SHA (is not yet persisted)."
  (unless (sha git-object)
    (error 'unpersisted-object-error
           :format-control "Cannot wrap unpersisted GIT-OBJECT ~S in an atomic-wrapper tree."
           :format-arguments (list git-object)))
  (let* ((meta-blob (make-instance 'git-blob :repository repository
                                              :sha (git-hash-object
                                                    repository "blob"
                                                    (%serialize-plist (list :tag :atomic-wrapper)))))
         (readme-blob (make-instance 'git-blob :repository repository
                                                :sha (git-hash-object
                                                      repository "blob"
                                                      (sb-ext:string-to-octets
                                                       +atomic-wrapper-readme+
                                                       :external-format :utf-8))))
         (tree (make-instance 'git-tree :repository repository
                                         :entries (list (cons ".meta" meta-blob)
                                                        (cons "README.md" readme-blob)
                                                        (cons "value" git-object)))))
    (setf (sha tree) (git-hash-object repository "tree" (serialize-tree tree)))
    (setf (get-loaded? tree) t)
    tree))

(defun %ensure-tree-entries-loaded (repository tree)
  "Ensure TREE's ENTRIES are populated, fetching and parsing its raw
Git tree bytes via GIT-CAT-FILE and DESERIALIZE-TREE if TREE is not
already loaded. Returns TREE."
  (unless (get-loaded? tree)
    (setf (get-entries tree) (deserialize-tree repository (git-cat-file repository (sha tree))))
    (setf (get-loaded? tree) t))
  tree)

(defun %ensure-blob-loaded (blob)
  "Ensure BLOB's PAYLOAD slot is populated, fetching and decoding its
raw Git blob bytes via GIT-CAT-FILE and DESERIALIZE-ATOM if BLOB is
not already loaded. Returns BLOB."
  (unless (get-loaded? blob)
    (setf (get-payload blob)
          (deserialize-atom (git-cat-file (get-repository blob) (sha blob))))
    (setf (get-loaded? blob) t))
  blob)

(defun %atomic-wrapper-tree-p (repository tree)
  "Return true if TREE (with ENTRIES already loaded) is an
ATOMIC-WRAPPER-TREE: one whose \".meta\" entry, fetched via
GIT-CAT-FILE and parsed as a plist via %DESERIALIZE-PLIST, has a
:TAG of :ATOMIC-WRAPPER. Returns NIL (rather than signaling) for any
ordinary tree with no \".meta\" entry at all."
  (let ((meta-entry (assoc ".meta" (get-entries tree) :test #'string=)))
    (and meta-entry
         (eq (getf (%deserialize-plist (git-cat-file repository (sha (cdr meta-entry)))) :tag)
             :atomic-wrapper))))

(defun %ensure-commit-loaded (commit)
  "Ensure COMMIT's TREE/PARENTS/AUTHOR/COMMITTER/TIMESTAMP/MESSAGE
slots are populated, fetching and parsing its raw Git commit text
via GIT-CAT-FILE and DESERIALIZE-COMMIT if COMMIT is not already
loaded (as is the case for a freshly INFLATE-GIT-PROXY'd commit,
e.g. a GIT-BRANCH's TARGET). Returns COMMIT."
  (unless (get-loaded? commit)
    (deserialize-commit commit
                         (sb-ext:octets-to-string
                          (git-cat-file (get-repository commit) (sha commit))
                          :external-format :utf-8)))
  commit)

(defun resolve-commit-root (commit)
  "Return the logical root GIT-OBJECT proxy for COMMIT: ordinarily
COMMIT's own GET-TREE, loaded if necessary, but transparently
unwrapped to the single underlying GIT-OBJECT held in its \"value\"
entry if that tree turns out to be an ATOMIC-WRAPPER-TREE (as
created by WRAP-ATOMIC-COMMIT-ROOT). The wrapper is thus entirely
invisible to callers: they receive back whatever kind of GIT-OBJECT
was originally committed as the root, tree or atom alike. COMMIT
itself is loaded first (via %ENSURE-COMMIT-LOADED) if necessary, so
this works equally well on a freshly INFLATE-GIT-PROXY'd commit."
  (%ensure-commit-loaded commit)
  (let* ((repository (get-repository commit))
         (tree (%ensure-tree-entries-loaded repository (get-tree commit))))
    (if (%atomic-wrapper-tree-p repository tree)
        (cdr (assoc "value" (get-entries tree) :test #'string=))
        tree)))
