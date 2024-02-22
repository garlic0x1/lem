(defpackage :lem-markdown-mode/interactive
  (:use :cl :lem)
  (:import-from #:alexandria :if-let :when-let :curry)
  (:export :register-block-evaluator))
(in-package :lem-markdown-mode/interactive)

(define-keys lem-markdown-mode::*markdown-mode-keymap*
  ("C-c C-e" 'markdown-eval-block)
  ("C-c C-r" 'markdown-eval-block-nop)
  ("C-c C-c" 'markdown-eval-block-and-insert)
  ("C-c C-d" 'markdown-kill-block-result))

(defvar *block-evaluators* (make-hash-table :test #'equal)
  "Dispatch table for block evaluators per language.")

(defvar *result-processors* (make-hash-table :test #'equal)
  "Dispatch table for result preprocessing.")

(defmacro register-block-evaluator (language (string callback) &body body)
  "Convenience macro to register block evaluators, wraps setf."
  `(setf (gethash ,language *block-evaluators*)
         (lambda (,string ,callback)
           ,@body)))

(defmacro register-result-processor (name (result) &body body)
  "Convenience macro to register result processors, wraps setf."
  `(setf (gethash ,name *result-processors*)
         (lambda (,result)
           ,@body)))

(defmacro with-constant-position ((point) &body body)
  "This allows you to move around the point without worry."
  `(let ((tmp (copy-point ,point)))
     (prog1 (progn ,@body)
       (move-point ,point tmp)
       (delete-point tmp))))

(defmacro when-markdown-mode (&body body)
  "Ensure the major mode is markdown-mode and alert the user if not."
  `(if (eq 'lem-markdown-mode:markdown-mode
           (buffer-major-mode (current-buffer)))
       (progn ,@body)
       (message "Not in markdown mode.")))

(defun pop-up-buffer (name text)
  "Create a pop-up with name containing text."
  (let ((buffer (make-buffer name)))
    (erase-buffer buffer)
    (with-buffer-read-only buffer nil
      (insert-string (buffer-point buffer) text)
      (with-pop-up-typeout-window (s buffer)
        (declare (ignore s))))))

(defun block-fence-lang (fence)
  "Get language from a block fence string."
  (let ((str (coerce (cdddr (coerce fence 'list)) 'string)))
    (unless (str:emptyp str)
      str)))

(defun block-fence-words (fence)
  "Get words trailing a block fence."
  (str:words (coerce (cdddr (coerce fence 'list)) 'string)))

(defun apply-processors (processors result)
  "Apply a list of single-argument functions to result."
  (reduce (lambda (ag proc) (funcall (gethash proc *result-processors*) ag))
          processors
          :initial-value result))

(defun block-at-point (point)
  "Get the language of a code block and its contents."
  (search-backward-regexp point "```")
  (when-let ((lang (block-fence-lang (str:trim (line-string point)))))
    (search-forward point (format nil "~%"))
    (let ((start (copy-point point)))
      (search-forward-regexp point "```")
      (let ((procs (block-fence-words (line-string point))))
        (search-backward point (format nil "~%"))
        (let ((string (points-to-string start point)))
          (delete-point start)
          (values lang string procs))))))

(define-command markdown-kill-block-result (&optional (point (current-point))) ()
  "Searches for a result block below the current code block, and kills it."
  (when-markdown-mode
    (with-constant-position (point)
      (when (block-at-point point)
        (search-forward-regexp point "```")
        (line-offset point 2)
        (when (equal "result" (block-fence-lang (line-string point)))
          (loop :while (not (equal "```" (line-string point)))
                :do (kill-whole-line)
                :do (line-offset point 1))
          (kill-whole-line)
          (kill-whole-line))))))

(defun pop-up-eval-result (point result)
  "Display results of evaluation in a pop-up buffer."
  (declare (ignore point))
  (pop-up-buffer "*result*" (format nil "~a" result)))

(defun insert-eval-result (point result)
  "Insert results of evaluation in a code block."
  (block-at-point point)
  (search-forward-regexp point "```")
  (line-end point)
  (insert-string point (format nil "~%~%```result~%~a~%```" result))
  (message "Block evaluated."))

(defun nop-eval-result (point result)
  "Clean up and do nothing with result."
  (declare (ignore point result))
  (message "Block evaluated."))

(defun wrap-handler (handler point preprocessors)
  "Wrap handlers to capture and delete the point when they are done.
Also applies preprocessors from the code block."
  (lambda (result)
    (funcall handler point (apply-processors preprocessors result))
    (delete-point point)))

(defun eval-block-internal (point handler)
  "Evaluate code block and apply handler to result."
  (when-markdown-mode
    (multiple-value-bind (lang block procs) (block-at-point point)
      (when lang
        (if-let ((evaluator (gethash lang *block-evaluators*)))
          (funcall evaluator block (wrap-handler handler point procs))
          (message "No evaluator registered for ~a." lang))))))

(define-command markdown-eval-block () ()
  "Evaluate current markdown code block and display results in pop-up."
  (eval-block-internal (copy-point (current-point)) #'pop-up-eval-result))

(define-command markdown-eval-block-nop () ()
  "Evaluate current markdown code block and do nothing with result."
  (eval-block-internal (copy-point (current-point)) #'nop-eval-result))

(define-command markdown-eval-block-and-insert () ()
  "Evaluate current markdown code block and display results in pop-up."
  (markdown-kill-block-result)
  (eval-block-internal (copy-point (current-point)) #'insert-eval-result))

;;
;; Default evaluators:
;;

(register-block-evaluator "bash" (string callback)
  "Register evaluator for Bash blocks."
  (bt:make-thread
   (lambda ()
     (funcall callback (uiop:run-program string :output :string)))))

(register-block-evaluator "lisp" (string callback)
  "Register evaluator for Lisp blocks."
  (lem-lisp-mode:check-connection)
  (lem-lisp-mode:lisp-eval-async
   `(eval (read-from-string ,(format nil "(progn ~a)" string)))
   callback))

;;
;; Default preprocessors:
;;

(register-result-processor "literal" (result)
  (format nil "~s" result))

(register-result-processor "none" (result)
  (declare (ignore result))
  "")
