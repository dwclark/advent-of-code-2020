(defpackage :day-5-v2
  (:use :cl)
  (:import-from :utils :read-day-file :bit-vector->integer)
  (:import-from :alexandria :map-iota)
  (:export #:part-1 #:part-2))

(in-package :day-5-v2)

(defun partition->seat-id (str)
  (flet ((c->n (c) (if (or (char= #\B c) (char= #\R c)) 1 0)))
    (bit-vector->integer 
     (make-array 10 :element-type 'bit :initial-contents (map 'list #'c->n str)))))

(defun part-1 ()
  (reduce #'max (mapcar #'partition->seat-id (read-day-file "5"))))

(defun part-2 ()
  (let ((all (mapcar #'partition->seat-id (read-day-file "5"))))
    (flet ((in-set (n) (if (not (member n all)) (return-from part-2 n))))
      (map-iota #'in-set (length all) :start (reduce #'min all)))))
