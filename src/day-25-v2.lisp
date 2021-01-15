(defpackage :day-25-v2
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-25-v2)

(defparameter *public-keys* (list 14222596 4057428))
(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

(defun compute (val key) (rem (* val key) *divisor*))

(defun loop-size (public-key &optional (day 0) (value 1))
  (if (= public-key value) day
      (loop-size public-key (1+ day) (compute value *subject-number*))))

(defun make-encryption-key (public-key loop-size &optional (val 1))
  (if (zerop loop-size) val
      (make-encryption-key public-key (1- loop-size) (compute val public-key))))

(defun part-1 ()
  (make-encryption-key (first *public-keys*) (loop-size (second *public-keys*))))
