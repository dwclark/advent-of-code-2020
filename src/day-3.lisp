(defpackage :day-3
  (:use :cl)
  (:import-from :alexandria :curry)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-3)

(defun repeat-string (n string)
  (format nil "~V@{~a~:*~}" n string))

(defun count-trees (lines down-by right-by)
  (loop with pos = 0
        for i from down-by below (length lines) by down-by
        count (char= #\# (char (elt lines i) (incf pos right-by))) into trees
        finally (return trees)))

(defun make-grid ()
  (let ((lines (read-day-file "3")))
    (map 'vector (curry #'repeat-string (length lines)) lines)))

(defun part-1 ()
  (count-trees (make-grid) 1 3))
         
(defun part-2 ()
  (let ((grid (make-grid)))
    (* (count-trees grid 1 1)
       (count-trees grid 1 3)
       (count-trees grid 1 5)
       (count-trees grid 1 7)
       (count-trees grid 2 1))))
