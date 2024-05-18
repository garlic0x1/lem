(ql:quickload :lem-termbox2)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:main
                          :executable t)
