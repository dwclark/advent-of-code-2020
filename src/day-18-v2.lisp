(defpackage :day-18-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :infix-math :$)
  (:import-from :infix-math/data :precedence)
  (:export #:part-1 #:part-2))

(in-package :day-18-v2)

(defun str->symbols (s)
  (with-input-from-string (in s)
    (loop for x = (read in nil nil) while x collect x)))

(defun sum-expressions ()
  (reduce #'+ (mapcar #'(lambda (s) (eval (append (list '$) (str->symbols s))))
                      (read-day-file "18"))))

(defun with-infix-mods (symbols changes-f)
  (let ((precedences (mapcar #'precedence symbols)))
    (unwind-protect
         (progn
           (funcall changes-f)
           (sum-expressions))
      (loop for sym in symbols
            for pre in precedences
            do (setf (precedence sym) pre)))))
                 
(defun part-1 ()
  (with-infix-mods '(+ *) #'(lambda () (setf (precedence '+) (precedence '*)))))

(defun part-2 ()
  (with-infix-mods '(+ *) #'(lambda () (rotatef (precedence '+) (precedence '*)))))
