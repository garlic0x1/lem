(defpackage #:lem-term 
  (:use :cl :lem :alexandria-2)
  (:import-from #:lem/listener-mode 
                #:listener-set-prompt-function
                #:listener-check-input-function
                #:listener-execute-function
                #:listener-prompt-attribute
                #:start-listener-mode))
(in-package :lem-term)

(defvar *cmd* "/usr/bin/bash")
(defvar *proc* nil)
(defvar *handler* nil)

(defmacro reset-listener-vars (&body pairs)
  `(progn
     ,@(mapcar 
        (lambda (it)
          `(setf (variable-value ,(car it) :buffer (current-buffer)) ,(cdr it)))
        pairs)))

(define-major-mode run-bash-mode nil
    (:name "run-bash-mode"
     :keymap *run-bash-mode-keymap*)
  (reset-listener-vars 
    ('listener-set-prompt-function  . #'identity)
    ('listener-check-input-function . (constantly t))
    ('listener-execute-function     . 'execute-input)
    ('listener-prompt-attribute     . nil))
  (start-listener-mode))

(defun execute-input (_point string)
  (declare (ignore _point))
  (let ((in (uiop:process-info-input *proc*)))
    (write-line string in)
    (force-output in)))

(defun buf-end-stream (buf)
  (make-buffer-output-stream (buffer-end-point buf)))

(defun handle-proc ()
  (with-open-stream (buf (buf-end-stream (get-bash-buffer)))
    (let ((out (uiop:process-info-output *proc*)))
      (loop :for c := (read-byte out)
            :do (write-byte c buf)
            :do (force-output buf)
            :do (redraw-display)))))

(define-command stop-bash () ()
  (when *handler*
    (ignore-errors (bt2:destroy-thread *handler*)))
  (when *proc*
    (uiop:close-streams *proc*))
  (setf *handler* nil 
        *proc* nil))

(defun start-bash ()
  (unless (or *proc* *handler*)
    (setf *proc* (uiop:launch-program *cmd* :input :stream  :output :stream)
          *handler* (bt2:make-thread 'handle-proc :name "*bash-handler*"))))

(defun get-bash-buffer ()
  (let ((buffer (make-buffer "*bash*")))
    (unless (eq (buffer-major-mode buffer) 'run-bash-mode)
      (change-buffer-mode buffer 'run-bash-mode))
    buffer))

(define-command run-bash () () 
  (start-bash)
  (pop-to-buffer (get-bash-buffer)))

(define-command restart-bash () ()
  (stop-bash)
  (start-bash)
  (pop-to-buffer (get-bash-buffer)))





