(defpackage :day-25-v3
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-25-v3)

(defparameter *public-keys* (list 14222596 4057428))
(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

(defun do-transform (key)
  (let ((val 1)
	(loop-size -1))
    (lambda ()
      (setf val (rem (* val key) *divisor*))
      (values (incf loop-size) val))))

(defun part-1 ()
  (let* ((loop-size (loop with tran = (do-transform *subject-number*)
			  with i = 0 and val = 0
			  while (progn
				  (multiple-value-setq (i val) (funcall tran))
				  ;(format t "i: ~A, val: ~A~%" i val)
				  (not (= val (first *public-keys*))))
			  finally (return i))))
    (loop with ignore = 0 and key = 0
	  with tran = (do-transform (second *public-keys*))
	  for i from 0 to loop-size
	  do (multiple-value-setq (ignore key) (funcall tran))
	  finally (return key))))
				       
					     
	
				   
