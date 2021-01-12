(defpackage :day-18
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split :regex-replace-all :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-18)

(defparameter *ops* (list (cons "+" #'+) (cons "*" #'*)))
(defparameter *part-1-precedence* (list (cons "+" 1) (cons "*" 1)))
(defparameter *part-2-precedence* (list (cons "+" 2) (cons "*" 1)))
(defparameter *normal-precedence* (list (cons "+" 1) (cons "*" 2)))
(defparameter *precedence* *part-1-precedence*)
(defparameter *ops-strings* (mapcar #'car *ops*))

(defun op-precedence (op)
  (if (not (stringp op))
      nil
      (cdr (find op *precedence* :test #'string= :key #'car))))

(defun op-p (tok)
  (and (stringp tok)
       (member tok *ops-strings* :test #'string=)))

(defun op-func (tok)
  (if (stringp tok)
      (cdr (find tok *ops* :test #'string= :key #'car))
      nil))

(defun tokenize (str-expr)
  (let* ((right-parens (regex-replace-all "\\)" str-expr " )"))
         (left-parens (regex-replace-all "\\(" right-parens "( "))
         (pre-tokens (split "\\s" left-parens)))

    (mapcar #'(lambda (token)
              (let ((ret token))
                (cl-ppcre:do-register-groups ((#'parse-integer num)) ("([0-9])" token)
                  (setf ret num))
                ret)) pre-tokens)))

(defun reverse-tokens (tokens)
  (mapcar #'(lambda (tok)
              (cond ((numberp tok) tok)
                    ((string= ")" tok) "(")
                    ((string= "(" tok) ")")
                    (t tok))) (reverse tokens)))

(defun shunting-yard-prefix (tokens)
  (loop with op-stack = nil
        with expr-list = nil
        for token in tokens
        do (cond ((numberp token) (setf expr-list (append expr-list (list token))))
                 
                 ((op-p token)
                  (let ((precedence (op-precedence token))
                        (top-stack (first op-stack)))
                    (if (and (op-p top-stack)
                             (<= precedence (op-precedence top-stack)))
                        (progn
                          (setf expr-list (append expr-list (list top-stack)))
                          (setf op-stack (rest op-stack))))
                    (push token op-stack)))
                 
                 
                 ((string= "(" token) (push "(" op-stack))
                  
                 ((string= ")" token) (loop while (not (string= "(" (first op-stack)))
                                            do (progn
                                                 (setf expr-list (append expr-list (list (first op-stack))))
                                                 (setf op-stack (rest op-stack)))
                                            finally (setf op-stack (rest op-stack)))))
        finally (return (append expr-list op-stack))))

(defun execute-prefix (tokens)
  (loop with stack = nil
        for token in tokens
        do (cond ((numberp token) (push token stack))
                 
                 ((op-p token)
                  (let ((func (op-func token))
                        (op1 (pop stack))
                        (op2 (pop stack)))
                    (push (funcall func op1 op2) stack))))
        finally (return (pop stack))))

(defun evaluate (str-expression)
  (execute-prefix (shunting-yard-prefix (reverse-tokens (tokenize str-expression)))))

(defun part-1 ()
  (let ((*precedence* *part-1-precedence*))
    (reduce #'+ (mapcar #'evaluate (read-day-file "18")))))

(defun normal ()
  (let ((*precedence* *normal-precedence*))
    (reduce #'+ (mapcar #'evaluate (read-day-file "18")))))

(defun part-2 ()
  (let ((*precedence* *part-2-precedence*))
    (reduce #'+ (mapcar #'evaluate (read-day-file "18")))))
