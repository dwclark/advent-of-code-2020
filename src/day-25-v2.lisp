(defpackage :day-25-v2
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-25-v2)

(defparameter *public-keys* (list 14222596 4057428))
(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

(defun loop-size (public-key)
  (loop with value = 1
        for day from 1 to most-positive-fixnum
        do (if (= public-key value)
               (return (1- day))
               (setf value (rem (* value *subject-number*) *divisor*)))))

(defun make-encryption-key (public-key loop-size)
  (let ((val 1))
    (dotimes (n loop-size)
      (setf val (rem (* val public-key) *divisor*)))
    val))

(defun part-1 ()
  (make-encryption-key (first *public-keys*) (loop-size (second *public-keys*))))
