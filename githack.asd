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
                 "persistent-wttree"))))
