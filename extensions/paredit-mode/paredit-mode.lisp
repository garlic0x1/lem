#|
link : http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html
|#

(defpackage :lem-paredit-mode
  (:use :cl
        :lem)
  (:export :paredit-mode
           :paredit-forward
           :paredit-backward
           :paredit-insert-paren
           :paredit-insert-doublequote
           :paredit-insert-vertical-line
           :paredit-backward-delete
           :paredit-forward-delete
           :paredit-close-parenthesis
           :paredit-kill
           :paredit-slurp
           :paredit-barf
           :paredit-splice
           :paredit-splice-backward
           :paredit-splice-forward
           :paredit-raise
           :paredit-wrap-round
           :paredit-meta-doublequote
           :paredit-vertical-line-wrap
           :*paredit-mode-keymap*))
(in-package :lem-paredit-mode)

(define-minor-mode paredit-mode
    (:name "paredit"
     :description "Helps to handle parentheses balanced in your Lisp code."
     :keymap *paredit-mode-keymap*))

(defun move-to-word-end (q)
  (loop while (not (syntax-space-char-p (character-at q)))
        do (character-offset q 1)))

(defun backward-open-paren-char-p (p)
  (with-point ((q p))
    (skip-whitespace-backward q)
    (syntax-open-paren-char-p (character-at q))))

(defun %skip-closed-parens-and-whitespaces-forward (point skip-last-whitespaces)
  (loop while (and (null (end-buffer-p point))
                   (or (syntax-closed-paren-char-p (character-at point))
                       (syntax-space-char-p (character-at point))))
        do (character-offset point 1))
  (unless skip-last-whitespaces
    (skip-whitespace-backward point)))

(defun move-to-string-start (point)
  (loop while (in-string-p point)
        do (character-offset point -1)))

(defun move-to-string-end (point)
  (loop while (in-string-p point)
        do (character-offset point 1)))

(define-command paredit-forward (&optional (n 1)) ("p")
  (handler-case
      (forward-sexp n)
    (error ()
      (unless (end-buffer-p (current-point))
        (lem:forward-up-list (current-point))))))

(define-command paredit-backward (&optional (n 1)) ("p")
  (handler-case
      (backward-sexp n)
    (error ()
      (unless (start-buffer-p (current-point))
        (lem:backward-up-list (current-point))))))

(defun bolp (point)
  (zerop (point-charpos point)))

(defun eolp (point)
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
             (1- len)))))

(defun integer-char-p (char)
  (< (char-code #\0) (char-code char) (char-code #\9)))

(defun sharp-literal-p (char point)
  (with-point ((p point))
    (character-offset p -1)
    (and (character-at p)
         (char-equal (character-at p) char)
         (eql (character-at p -1) #\#))))

(defun sharp-n-literal-p (char point)
  (with-point ((p point))
    (character-offset p -1)
    (when (char-equal char (character-at p))
      (character-offset p -1)
      (skip-chars-backward p #'integer-char-p)
      (and (integer-char-p (character-at p))
           (eql (character-at p -1) #\#)))))

(defparameter *non-space-following-chars*
  '(#\Space #\( #\' #\` #\,))

(defparameter *non-space-preceding-chars*
  '(#\Space #\)))

(defun non-space-following-context-p (&optional (p (current-point)))
  (or (bolp p)
      (find (character-at p -1)
            *non-space-following-chars*)
      (eql (character-at p -1) #\#)
      (and (eql (character-at p -1) #\@)
           (eql (character-at p -2) #\,))
      (sharp-literal-p #\' p)
      (sharp-literal-p #\. p)
      (sharp-literal-p #\S p)
      (sharp-literal-p #\C p)
      (sharp-literal-p #\+ p)
      (sharp-literal-p #\- p)
      (sharp-n-literal-p #\A p)
      (sharp-n-literal-p #\= p)))

(define-command paredit-insert-paren () ()
  (let ((p (current-point)))
    (when (in-string-or-comment-p p)
      (insert-character p #\()
      (return-from paredit-insert-paren))
    (when (syntax-escape-point-p p 0)
      (insert-character p #\()
      (return-from paredit-insert-paren))
    (unless (non-space-following-context-p p)
      (insert-character p #\Space))
    (dolist (c '(#\( #\)))
      (insert-character p c))
    (unless (or (eolp p)
                (eql (character-at p) #\Space)
                (eql (character-at p) #\)))
      (insert-character p #\Space)
      (character-offset p -1))
    (character-offset p -1)))

(define-command paredit-insert-doublequote () ()
  (let ((p (current-point)))
    (cond
      ((syntax-escape-point-p p 0)
       (insert-character p #\"))
      ((in-string-p p)
       (if (eql (character-at p) #\")
           (forward-char)
           (insert-string p "\\\"" #\\)))
      (t (unless (or (bolp p)
                     (find (character-at p -1)
                           *non-space-following-chars*)
                     (sharp-literal-p #\P p))
           (insert-character p #\Space))
         (insert-character p #\" 2)
         (unless (or (eolp p)
                     (find (character-at p)
                           *non-space-preceding-chars*))
           (insert-character p #\Space)
           (character-offset p -1))
         (character-offset p -1)))))

(define-command paredit-insert-vertical-line () ()
  (let ((p (current-point)))
    (when (or (in-string-or-comment-p p)
              (syntax-escape-point-p p 0))
      (insert-character p #\|)
      (return-from paredit-insert-vertical-line))
    (insert-character p #\| 2)
    (character-offset p -1)))

(define-command paredit-backward-delete (&optional (n 1)) ("p")
  (when (< 0 n)
    (let ((p (current-point)))
      (cond
        ((lem-paredit-mode::bolp p)
         (delete-previous-char))
        ;; The previous char is escaped
        ((syntax-escape-point-p p -1)
         (delete-previous-char 2))
        ;; The previous char is an escaping #\\
        ((syntax-escape-point-p p 0)
         (delete-next-char)
         (delete-previous-char))
        ;; The point is in a string and the previous char is a #\"
        ((and (in-string-p p) (eql (character-at p -1) #\"))
         (if (and (eql (character-at p 0) #\"))
             (progn (delete-next-char)
                    (delete-previous-char))
             (backward-char)))
        ;; The point is in a string and the previous char is not a #\"
        ;; or it is in a comment
        ((in-string-or-comment-p p)
         (delete-previous-char))
        ;; The previous char is #\(
        ((eql (character-at p -1) #\()
         (if (eql (character-at p) #\))
             (progn (delete-next-char)
                    (delete-previous-char))
             (backward-char)))
        ;; The previous char is #\|
        ((eql (character-at p -1) #\|)
         (if (eql (character-at p) #\|)
             (progn (delete-next-char)
                    (delete-previous-char))
             (backward-char)))
        ;; Should not delete #\) nor #\" nor #\|
        ((or (eql (character-at p -1) #\))
             (eql (character-at p -1) #\")
             (eql (character-at p -1) #\|))
         (backward-char))
        (t
         (delete-previous-char))))
    (paredit-backward-delete (1- n))))

(define-command paredit-forward-delete (&optional (n 1)) ("p")
  (when (< 0 n)
    (let ((p (current-point)))
      (cond
        ((lem-paredit-mode::eolp p)
         (delete-next-char))
        ;; The next char is escaped
        ((syntax-escape-point-p p 0)
         (delete-next-char)
         (delete-previous-char))
        ;; The next char is an escaping #\\
        ((syntax-escape-point-p p 1)
         (delete-next-char 2))
        ;; The point is in a string and the next char is a #\"
        ((and (in-string-p p) (eql (character-at p) #\"))
         (if (and (eql (character-at p -1) #\")
                  (not (syntax-escape-point-p p -1)))
             (progn (delete-next-char)
                    (delete-previous-char))
             (forward-char)))
        ;; The point is in a string and the next char is not a #\"
        ;; or it is in a comment
        ((in-string-or-comment-p p)
         (delete-next-char))
        ;; The next char is #\)
        ((eql (character-at p) #\))
         (if (and (eql (character-at p -1) #\()
                  (not (syntax-escape-point-p p -1)))
             (progn (delete-next-char)
                    (delete-previous-char))
             (forward-char)))
        ;; The next char is #\|
        ((eql (character-at p) #\|)
         (if (and (eql (character-at p -1) #\|)
                  (not (syntax-escape-point-p p -1)))
             (progn (delete-next-char)
                    (delete-previous-char))
             (forward-char)))
        ;; Should not delete #\( nor #\" nor #\|
        ((or (eql (character-at p) #\()
             (eql (character-at p) #\")
             (eql (character-at p) #\|))
         (forward-char))
        (t
         (delete-next-char))))
    (paredit-forward-delete (1- n))))

(define-command paredit-close-parenthesis () ()
  (with-point ((p (current-point)))
    (when (in-string-or-comment-p p)
      (insert-character p #\))
      (return-from paredit-close-parenthesis))
    (case (character-at p)
      (#\)
       (if (syntax-escape-point-p p 0)
           (insert-character p #\))
           (forward-char)))
      (otherwise
       (handler-case (scan-lists p 1 1)
         (error ()
           (insert-character p #\))
           (return-from paredit-close-parenthesis)))
       (with-point ((new-p p))
         (character-offset new-p -1)
         (move-point (current-point) new-p)
         (with-point ((p new-p))
           (skip-whitespace-backward p)
           (delete-between-points p new-p)))))))

(define-command paredit-kill () ()
  (with-point ((origin (current-point))
               (line-end (current-point))
               (kill-end (current-point)))
    (line-end line-end)
    (skip-whitespace-forward kill-end t)
    (cond
      ;; Only whitespaces and maybe a line comment after point
      ((or (point<= line-end kill-end)
           (eql (character-at kill-end) #\;))
       (kill-line 1))
      ;; Inside a string or comment - kill up to end of line
      ;; or end of string or comment
      ((in-string-or-comment-p origin)
       (loop while (and (point>= line-end kill-end)
                        (in-string-or-comment-p kill-end))
             do (character-offset kill-end 1))
       (unless (in-string-or-comment-p kill-end)
         (case (character-at kill-end -1)
           ((#\")
            (character-offset kill-end -1))
           ((#\#) ; doublechecking that the char before is #\|
            (if (eql (character-at kill-end -2) #\|)
                (character-offset kill-end -2)))))
       (kill-region origin kill-end))
      (t
       (loop while (and (point> line-end kill-end)
                        (not (eql #\) (character-at kill-end)))
                        (form-offset kill-end 1))
             do (skip-whitespace-forward kill-end t)
                (cond
                  ((eql (character-at kill-end) #\;)
                   (line-end kill-end)
                   (character-offset kill-end 1)
                   (return))
                  ((eql (character-at kill-end) #\Newline)
                   (character-offset kill-end 1)
                   (return))))
       (kill-region origin kill-end)))))

(define-command paredit-slurp () ()
  (with-point ((origin (current-point))
               (kill-point (current-point)))
    (cond
      ((in-string-p origin)
       (move-to-string-end kill-point)
       (character-offset kill-point -1)
       (with-point ((yank-point kill-point))
         (let ((c (character-at kill-point)))
           (character-offset yank-point 1)
           (form-offset yank-point 1)
           (unless (end-buffer-p yank-point)
             (insert-character yank-point c)
             (delete-character kill-point)))
         (move-point (current-point) origin)
         (indent-points origin yank-point)))
      (t
       (scan-lists kill-point 1 1)
       (character-offset kill-point -1)
       (%skip-closed-parens-and-whitespaces-forward kill-point nil)
       (character-offset kill-point -1)
       (with-point ((yank-point kill-point :left-inserting))
         (%skip-closed-parens-and-whitespaces-forward yank-point t)
         (unless (end-buffer-p yank-point)
           (let ((c (character-at kill-point)))
             (form-offset yank-point 1)
             (insert-character yank-point c)
             (delete-character kill-point))
           (move-point (current-point) origin)
           (indent-points origin yank-point)))))))

(define-command paredit-barf () ()
  (with-point ((origin (current-point) :right-inserting)
               (kill-point (current-point)))
    (cond
      ((in-string-p origin)
       (move-to-string-end kill-point)
       (character-offset kill-point -1)
       (with-point ((yank-point kill-point))
         (skip-whitespace-backward yank-point)
         (unless (syntax-string-quote-char-p (character-at yank-point -1))
           (form-offset yank-point -1)
           (syntax-skip-expr-prefix-backward yank-point)
           (skip-whitespace-backward yank-point)
           (let ((c (character-at kill-point)))
             (delete-character kill-point)
             (insert-character yank-point c))
           (move-point (current-point) origin)
           (indent-points origin yank-point))))
      (t
       (scan-lists kill-point -1 1)
       (when (syntax-open-paren-char-p (character-at kill-point))
         (scan-lists kill-point 1 0)
         (character-offset kill-point -1)
         (with-point ((yank-point kill-point))
           (let ((c (character-at kill-point)))
             (delete-character kill-point)
             (form-offset yank-point -1)
             (syntax-skip-expr-prefix-backward yank-point)
             (skip-space-and-comment-backward yank-point)
             (insert-character yank-point c))
           (move-point (current-point) origin)
           (indent-points origin kill-point)))))))

(define-command paredit-splice () ()
  (with-point ((origin (current-point) :right-inserting)
               (start (current-point)))
    (cond
      ((in-string-p origin)
       (move-to-string-start start)
       (with-point ((end (current-point)))
         (move-to-string-end end)
         (character-offset end -1)
         (when (and (syntax-string-quote-char-p (character-at start))
                    (syntax-string-quote-char-p (character-at end)))
           (delete-character end)
           (delete-character start))))
      (t
       (scan-lists start -1 1)
       (when (syntax-open-paren-char-p (character-at start))
         (with-point ((end start))
           (scan-lists end 1 0)
           (character-offset end -1)
           (delete-character end)
           (delete-character start)
           (indent-points start end)))))))

(define-command paredit-splice-backward () ()
  (with-point ((origin (current-point) :right-inserting)
               (start (current-point)))
    (cond
      ((in-string-p origin)
       (move-to-string-start start)
       (with-point ((end (current-point)))
         (move-to-string-end end)
         (character-offset end -1)
         (when (and (syntax-string-quote-char-p (character-at start))
                    (syntax-string-quote-char-p (character-at end)))
           (delete-character end)
           (delete-between-points start origin))))
      (t
       (scan-lists start -1 1)
       (when (syntax-open-paren-char-p (character-at start))
         (with-point ((end start))
           (scan-lists end 1 0)
           (character-offset end -1)
           (delete-character end)
           (delete-between-points start origin)
           (indent-points start end)))))))

(define-command paredit-splice-forward () ()
  (with-point ((origin (current-point) :right-inserting)
               (start (current-point)))
    (cond
      ((in-string-p origin)
       (move-to-string-start start)
       (with-point ((end origin))
         (move-to-string-end end)
         (character-offset end -1)
         (when (and (syntax-string-quote-char-p (character-at start))
                    (syntax-string-quote-char-p (character-at end)))
           (delete-character end)
           (delete-between-points origin end)
           (delete-character start))))
      (t
       (scan-lists start -1 1)
       (when (syntax-open-paren-char-p (character-at start))
         (with-point ((end start))
           (scan-lists end 1 0)
           (delete-between-points origin end)
           (delete-character start)
           (indent-points start end)))))))

(define-command paredit-raise () ()
  (with-point ((start (current-point)))
    (cond
      ((in-string-p start)
       (move-to-string-start start)
       (with-point ((end (current-point))
                    (word-start (current-point)))
         (move-to-string-end end)
         (character-offset end -1)
         (character-offset word-start 1)
         (form-offset word-start -1)
         (when (and (syntax-string-quote-char-p (character-at start))
                    (syntax-string-quote-char-p (character-at end)))
           (with-point ((word-end word-start))
             (form-offset word-end 1)
             (delete-character end)
             (delete-between-points word-end end)
             (delete-between-points start word-start)))))
      (t
       (scan-lists start -1 1)
       (when (syntax-open-paren-char-p (character-at start))
         (with-point ((end start)
                      (word-start (current-point)))
           (scan-lists end 1 0)
           (unless (syntax-open-paren-char-p (character-at word-start))
             (character-offset word-start 1)
             (form-offset word-start -1))
           (with-point ((word-end word-start))
             (form-offset word-end 1)
             (delete-between-points word-end end)
             (delete-between-points start word-start))))))))


(defun %paredit-wrap (begin-char end-char)
  (if (buffer-mark-p (current-buffer))
      (with-point ((begin (region-beginning (current-buffer)))
                   (end (region-end (current-buffer))))
        (unless (or (in-string-or-comment-p begin)
                    (in-string-or-comment-p end))
          (cond ((point> begin end)
                 (insert-character begin end-char)
                 (insert-character end begin-char))
                ((point< begin end)
                 (insert-character end end-char)
                 (insert-character begin begin-char)
                 (move-point (current-point) (character-offset begin 1)))
                (t
                 (insert-character begin begin-char)
                 (insert-character (current-point) end-char)
                 (insert-character (current-point) #\Space)
                 (move-point (current-point) (character-offset begin 1))))))
      (with-point ((origin (current-point)))
        (unless (in-string-or-comment-p origin)
          (forward-sexp)
          (insert-character origin begin-char)
          (unless (non-space-following-context-p origin)
            (insert-character origin #\Space))
          (insert-character (current-point) end-char)
          (move-point (current-point) (character-offset origin 1))))))

(define-command paredit-wrap-round () ()
  (%paredit-wrap #\( #\)))

(define-command paredit-meta-doublequote () ()
  (%paredit-wrap #\" #\"))

(define-command paredit-vertical-line-wrap () ()
  (lem-paredit-mode::%paredit-wrap #\| #\|))

(loop for (k . f) in '((forward-sexp . paredit-forward)
                       (backward-sexp . paredit-backward)
                       ("(" . paredit-insert-paren)
                       (")" . paredit-close-parenthesis)
                       ("\"" . paredit-insert-doublequote)
                       ("|" . paredit-insert-vertical-line)
                       (delete-previous-char . paredit-backward-delete)
                       (delete-next-char . paredit-forward-delete)
                       ("C-k" . paredit-kill)
                       ("C-Right" . paredit-slurp)
                       ("C-Left" . paredit-barf)
                       ("M-s" . paredit-splice)
                       ("M-Up" . paredit-splice-backward)
                       ("M-r" . paredit-raise)
                       ("M-(" . paredit-wrap-round)
                       ("M-|" . paredit-vertical-line-wrap)
                       ("M-\"" . paredit-meta-doublequote))
      do (define-key *paredit-mode-keymap* k f))
