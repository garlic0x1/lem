(defpackage :lem-termbox2/view
  (:use :cl :termbox2)
  (:export :make-view
           :delete-view
           :clear-view
           :set-view-size
           :set-view-pos
           :redraw-view
           :update-display))
(in-package :lem-termbox2/view)

(defclass view ()
  ((x
    :initarg :x
    :accessor view-x)
   (y
    :initarg :y
    :accessor view-y)
   (width
    :initarg :width
    :accessor view-width)
   (height
    :initarg :height
    :accessor view-height)))

(defun make-view (window x y width height use-modeline)
  (declare (ignore window use-modeline))
  (make-instance 'view :x x :y y :width width :height height))

(defun delete-view (view)
  :TODO)

(defun clear-view (view)
  :TODO)

(defun set-view-size (view width height)
  (setf (view-width view) width
        (view-height view) height))

(defun set-view-pos (view x y)
  (setf (view-x view) x
        (view-y view) y))

(defun redraw-view (view)
  (tb-print (view-x view) (view-y view)
            0 1 "lol"))

;
