(defsystem "lem-utils"
  :serial t
  :depends-on ("trivia"
               "closer-mop")
  :components ((:file "package")
               (:file "class")
               (:file "general")))
