(asdf:defsystem "lem-template"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cl-template)
  :components ((:file "utils")
               (:file "prompt")
               (:file "template")))
