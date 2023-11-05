(defpackage :day-25-v4
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-25-v4)

(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

(defun find-loop-size (start key)
  (loop with val = 1
	while (not (= val key))
	for loop-size from 0
	do (setf val (rem (* val start) *divisor*))
	finally (return loop-size)))

(defun find-key (loop-size key)
  (loop with val = 1
	for i from 0 to loop-size
	do (setf val (rem (* val key) *divisor*))
	finally (return val)))

(defun part-1 ()
  (find-key (find-loop-size *subject-number* 14222596) 4057428))
    
					     
	
				   
