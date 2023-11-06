(defpackage :day-25-v4 (:use :cl) (:export #:part-1 #:part-2))
(in-package :day-25-v4)

(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

(defun find-loop-size (start key)
  (loop for val = 1 then (rem (* val start) *divisor*)
	until (= key val) counting val))

(defun find-key (loop-size key)
  (loop for val = 1 then (rem (* val key) *divisor*)
	repeat loop-size finally (return val)))

(defun part-1 ()
  (find-key (find-loop-size *subject-number* 14222596) 4057428))
