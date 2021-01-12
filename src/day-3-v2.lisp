(defpackage :day-3-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-3-v2)

(defun trees (grid right down)
  (loop with height = (length grid)
        with width = (length (nth 0 grid))
        for row from down below height by down
        for step from 1 below height
        count (char= #\# (char (nth row grid) (mod (* right step) width))) into total
        finally (return total)))

(defun part-1 ()
  (trees (read-day-file "3") 3 1))
         
(defun part-2 ()
  (let ((g (read-day-file "3")))
    (flet ((t (right down) (trees g right down)))
      (* (t 1 1) (t 3 1) (t 5 1) (t 7 1) (t 1 2)))))
