(defpackage :day-15-v2
  (:use :cl)
  (:export #:part-1 #:part-2))

(in-package :day-15-v2)

(defparameter *plays* (list 2 20 0 4 1 17))
(defparameter *board* nil)

(defun speak (num turn)
  (multiple-value-bind (prev is-there) (gethash num *board*)
    (setf (gethash num *board*) turn)
    (if is-there (- turn prev) 0)))

(defun reset-board ()
  (setf *board* (make-hash-table :test 'eq))
  (loop for play in *plays* and i from 0
        do (speak play i)))

(defun play (max-turns)
  (reset-board)
  (loop with prev = 0
        for turn from (length *plays*) below (1- max-turns)
        do (setf prev (speak prev turn))
        finally (return prev)))

(defun part-1 () (play 2020))

(defun part-2 () (play 30000000))
