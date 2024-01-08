(asdf:defsystem "lem-term"
  :author "garlic0x1"
  :description "A terminal emulator for Lem."
  :license "MIT"
  :depends-on (:lem :alexandria :bordeaux-threads :trivia)
  :components ((:file "term")))