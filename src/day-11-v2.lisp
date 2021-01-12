(defpackage :day-11-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :curry :with-unique-names :define-constant)
  (:import-from :aops :flatten :each-index! :each-index)
  (:export #:part-1 #:part-2))

(in-package :day-11-v2)

(defparameter *grid* nil)

(define-constant +occupied+ #\# :test #'char=)
(define-constant +empty+ #\L :test #'char=)
(define-constant +floor+ #\. :test #'char=)
(define-constant +none+ #\n :test #'char=)
(define-constant +deltas+ '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) :test #'equal)

(defun make-grid ()
  (let* ((lines (read-day-file "11"))
         (grid (make-array (list (length lines) (length (first lines))))))
    (each-index! grid (row col)
        (char (nth row lines) col))))

(defun walk-direction (prev-row prev-col row-diff col-diff remaining)
  (let ((row (+ row-diff prev-row))
        (col (+ col-diff prev-col)))
    (if (and (not (zerop remaining)) (<= 0 row) (<= 0 col)
             (< row (array-dimension *grid* 0))
             (< col (array-dimension *grid* 1)))
        (let ((at (aref *grid* row col)))
          (if (char= +floor+ at)
              (walk-direction row col row-diff col-diff (1- remaining))
              at))
        +none+)))

(defun discover-neighbors (row col remaining)
  (flet ((walk (delta) (walk-direction row col (first delta) (second delta) remaining)))
    (mapcar #'walk +deltas+)))

(defun execute-round (should-empty should-occupy)
  (each-index (row col)
    (let ((current (aref *grid* row col)))
      (cond ((char= +floor+ current) +floor+)
            ((funcall should-empty row col) +empty+)
            ((funcall should-occupy row col) +occupied+)
            (t current)))))

(defun execute-all-rounds (should-empty should-occupy)
  (loop
    do (let ((new (execute-round should-empty should-occupy)))
         (if (equalp new *grid*)
             (return (count-if (curry #'char= +occupied+) (flatten *grid*)))
             (setf *grid* new)))))

(defun should-occupy (remaining)
  #'(lambda (row col)
      (and (char= +empty+ (aref *grid* row col))
           (zerop (count-if (curry #'char= +occupied+) (discover-neighbors row col remaining))))))

(defun should-empty (crowded remaining)
  #'(lambda (row col)
      (and (char= +occupied+ (aref *grid* row col))
           (<= crowded (count-if (curry #'char= +occupied+) (discover-neighbors row col remaining))))))
  
(defun part-1 ()
  (let ((*grid* (make-grid)))
    (execute-all-rounds (should-empty 4 1) (should-occupy 1))))

(defun part-2 ()
  (let ((*grid* (make-grid)))
    (execute-all-rounds (should-empty 5 most-positive-fixnum) (should-occupy most-positive-fixnum))))
