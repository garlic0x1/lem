(defsystem "lem-termbox2"
  :depends-on ("cffi"
               "termbox2"
               "cl-setlocale"
               "lem"
               "lem/extensions")
  :serial t
  :components ((:file "config")
               (:file "term")
               (:file "clipboard")
               (:file "style")
               (:file "key")
               (:file "attribute")
               (:file "drawing-object")
               (:file "view")
               (:file "render")
               (:file "input")
               (:file "mainloop")
               (:file "ncurses")))
