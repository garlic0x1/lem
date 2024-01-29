(defpackage :lem-c-mode/lsp-config
  (:use :cl)
  (:import-from #:lem-lsp-mode #:define-language-spec #:spec-initialization-options)
  (:import-from #:lem-lsp-base/type #:make-lsp-map))
(in-package :lem-c-mode/lsp-config)

(define-language-spec (c-spec lem-c-mode:c-mode)
  :language-id "c"
  :root-uri-patterns '("compile_commands.json")
  :command `("bash" "-c" "clangd 2> /dev/null")
  :install-command ""
  :readme-url "https://github.com/clangd/clangd"
  :connection-mode :stdio)

(defmethod spec-initialization-options ((spec c-spec))
  (make-lsp-map "matcher" "fuzzy"))
