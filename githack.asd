(defsystem githack
  :description "Run Git from within Lisp."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (alexandria cffi fold named-let table)
  :components ((:file "package")
               (:file "githack" :depends-on ("package"))
               (:file "persistent-wttree" :depends-on ("githack" "package"))
               (:file "persistent-vector"
                :depends-on ("githack" "package" "persistent-wttree"))
               (:file "persistent-hash-table"
                :depends-on
                ("githack" "package" "persistent-vector"
                 "persistent-wttree"))
               (:file "identifier"
                :depends-on
                ("githack" "package" "persistent-vector"))
               (:file "canonical"
                :depends-on
                ("githack" "package" "persistent-hash-table"
                "persistent-vector"))
               (:file "cid-object"
                :depends-on
                ("canonical" "githack" "identifier" "package"
                "persistent-wttree"))
               (:file "cid-set"
                :depends-on ("githack" "package" "persistent-wttree"))
               (:file "cid-detail-table"
                :depends-on
                ("githack" "package" "persistent-hash-table"
                 "persistent-vector"))
               (:file "mapper"
                :depends-on
                ("cid-object" "cid-set" "githack" "identifier"
                 "package" "persistent-hash-table"
                 "persistent-vector"))
               (:file "integer-mapper"
                :depends-on
                ("githack" "identifier" "mapper" "package"
                 "persistent-vector"))
               (:file "versioned-value"
                :depends-on
                ("cid-set" "githack" "package"
                 "persistent-vector"))
               (:file "cvi"
                :depends-on
                ("cid-set" "githack" "package"
                "persistent-vector" "versioned-value"))
               (:file "cvfile"
                :depends-on
                ("cid-set" "cvi" "githack" "package"
                "persistent-vector" "versioned-value"))
               (:file "cid-master-table"
                :depends-on
                ("cid-detail-table" "cid-object" "cid-set"
                 "githack" "mapper" "package"
                 "persistent-vector" "versioned-value"))
               (:file "repository"
                :depends-on
                ("canonical" "cid-master-table" "cid-object" "cid-set"
                 "githack" "identifier" "mapper" "package"
                 "persistent-hash-table" "persistent-vector"
                 "versioned-value"))
               (:file "distributed-object"
                :depends-on
                ("githack" "identifier" "mapper" "package"
                 "repository"))
               (:file "txn"
                :depends-on
                ("cid-master-table" "cid-object" "cid-set" "cvi"
                 "distributed-object" "githack" "package" "repository"
                 "versioned-value"))
               (:file "versioned-object"
                :depends-on
                ("cid-set" "cvi" "cvfile" "githack" "package"
                 "repository" "txn" "versioned-value"))))
