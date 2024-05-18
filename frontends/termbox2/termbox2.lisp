(defpackage :lem-termbox2
  (:use :cl)
  )
(in-package :lem-termbox2)

(pushnew :lem-termbox2 *features*)

(defclass termbox2 (lem:implementation)
  ()
  (:default-initargs
   :name :termbox2
   :redraw-after-modifying-floating-window t))

(defmethod lem-if:invoke ((impl termbox2) function)
  :TODO)

(defmethod lem-if:get-background-color ((impl termbox2))
  :TODO)

(defmethod lem-if:update-background ((impl termbox2) color-name)
  :TODO)

(defmethod lem-if:update-foreground ((impl termbox2) color-name)
  :TODO)

(defmethod lem-if:update-cursor-shape ((impl termbox2) cursor-type)
  :TODO)

(defmethod lem-if:display-width ((impl termbox2))
  (termbox2:tb-width))

(defmethod lem-if:display-height ((impl termbox2))
  (termbox2:tb-height))

(defmethod lem-if:make-view ((impl termbox2) window x y width height use-modeline)
  :TODO)

(defmethod lem-if:delete-view ((impl termbox2) view)
  :TODO)

(defmethod lem-if:clear ((impl termbox2) view)
  :TODO)

(defmethod lem-if:set-view-size ((impl termbox2) view width height)
  :TODO)

(defmethod lem-if:set-view-pos ((impl termbox2) view x y)
  :TODO)

(defmethod lem-if:redraw-view-after ((impl termbox2) view)
  :TODO)

(defmethod lem-if:update-display ((impl termbox2))
  :TODO)

(defmethod lem-if:clipboard-paste ((impl termbox2))
  :TODO)

(defmethod lem-if:clipboard-copy ((impl termbox2) text)
  :TODO)

(defmethod lem-if:view-width ((impl termbox2) view)
  :TODO)

(defmethod lem-if:view-height ((impl termbox2) view)
  :TODO)

(defmethod lem-if:render-line ((impl termbox2) view x y objects height)
  :TODO)

(defmethod lem-if:render-line-on-modeline
    ((impl termbox2) view left-objects right-objects default-attribute height)
  :TODO)

(defmethod lem-if:object-width ((impl termbox2) drawing-object)
  :TODO)

(defmethod lem-if:object-height ((impl termbox2) drawing-object)
  :TODO)

(defmethod lem-if:clear-to-end-of-window ((impl termbox2) view y)
  :TODO)

(defmethod lem-if:get-char-width ((impl termbox2))
  (tb-get-char-width))
