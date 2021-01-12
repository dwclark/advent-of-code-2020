(defpackage :day-11
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :curry :with-unique-names)
  (:export #:part-1 #:part-2))

(in-package :day-11)

(defparameter *grid* nil)
(defparameter *floor* #\.)
(defparameter *occupied* #\#)
(defparameter *empty* #\L)
(defparameter *none* #\n)

(defmacro with-grid ((grid) &body body)
  `(let ((*grid* grid))
     ,@body))

(defmacro with-file-grid (&body body)
  `(let ((*grid* (make-grid)))
     ,@body))

(defun make-grid ()
  (let* ((lines (read-day-file "11"))
         (rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols)))
         (row-idx 0))
    (loop for line in lines
          do (loop for col-idx from 0 below (length line)
                   do (setf (aref grid row-idx col-idx) (char line col-idx))
                   finally (incf row-idx)))
    grid))

(defmacro loop-grid ((row col rows cols) &body body)
  `(let ((,rows (array-dimension *grid* 0))
         (,cols (array-dimension *grid* 1)))
     (loop for ,row from 0 below ,rows
           do (loop for ,col from 0 below ,cols
                    do (progn ,@body)))))

(defmacro loop-rows ((row rows cols) &body body)
  `(let ((,rows (array-dimension *grid* 0))
         (,cols (array-dimension *grid* 1)))
     (loop for ,row from 0 below ,rows
           do (progn ,@body))))

(defun make-action-grid ()
  (make-array (list (array-dimension *grid* 0) (array-dimension *grid* 1))
              :initial-element #'do-nothing))

(defun floor-p (row col)
  (char= *floor* (aref *grid* row col)))

(defun chair-p (row col)
  (not (floor-p *grid* row col)))

(defun occupied-p (row col)
  (char= *occupied* (aref *grid* row col)))

(defun empty-p (row col)
  (char= *empty* (aref *grid* row col)))

(defun do-nothing (row col))

(defun empty-seat (row col)
  (setf (aref *grid* row col) *empty*))

(defun occupy-seat (row col)
  (setf (aref *grid* row col) *occupied*))

(defun legal-p (row col)
  (and (<= 0 row) (< row (array-dimension *grid* 0))
       (<= 0 col) (< col (array-dimension *grid* 1))))

(defun walk-direction (prev-row prev-col row-f col-f remaining)
  (let ((row (funcall row-f prev-row))
        (col (funcall col-f prev-col)))
    (if (or (= 0 remaining)
            (not (legal-p row col)))
        *none*
        (cond ((occupied-p row col) *occupied*)
              ((empty-p row col) *empty*)
              (t (walk-direction row col row-f col-f (1- remaining)))))))

(defun discover-neighbors (row col remaining)
  (list (walk-direction row col #'1- #'1- remaining)
        (walk-direction row col #'1- #'identity remaining)
        (walk-direction row col #'1- #'1+ remaining)
        (walk-direction row col #'identity #'1- remaining)
        (walk-direction row col #'identity #'1+ remaining)
        (walk-direction row col #'1+ #'1- remaining)
        (walk-direction row col #'1+ #'identity remaining)
        (walk-direction row col #'1+ #'1+ remaining)))

(defun apply-actions (actions)
  (let ((became-empty 0)
        (became-occupied 0))

    (loop-grid (row col rows cols)
      (let ((action (aref actions row col)))
        (funcall action row col)
        (if (eq #'empty-seat action)
            (incf became-empty))
        (if (eq #'occupy-seat action)
            (incf became-occupied))))

    (multiple-value-bind (occupied empty) (grid-counts)
      (values (to-string) occupied became-occupied empty became-empty))))

(defun should-occupy-1 (row col)
  (let ((neighbors (discover-neighbors row col 1)))
    (and (empty-p row col)
         (= 0 (count-if #'(lambda (e) (char= *occupied* e)) neighbors)))))

(defun should-occupy-2 (row col)
  (let ((neighbors (discover-neighbors row col most-positive-fixnum)))
    (and (empty-p row col)
         (= 0 (count-if #'(lambda (e) (char= *occupied* e)) neighbors)))))

(defun should-empty-1 (row col)
  (let ((neighbors (discover-neighbors row col 1)))
    (and (occupied-p row col)
         (<= 4 (count-if #'(lambda (e) (char= *occupied* e)) neighbors)))))

(defun should-empty-2 (row col)
  (let ((neighbors (discover-neighbors row col most-positive-fixnum)))
    (and (occupied-p row col)
         (<= 5 (count-if #'(lambda (e) (char= *occupied* e)) neighbors)))))

(defun discover-actions (should-empty should-occupy)
  (let ((actions (make-action-grid)))
    
    (loop-grid (row col rows cols)
      
      (if (funcall should-empty row col)
          (setf (aref actions row col) #'empty-seat))
      
      (if (funcall should-occupy row col)
          (setf (aref actions row col) #'occupy-seat)))
  
    actions))

(defun execute-round (should-empty should-occupy)
  (let ((actions (discover-actions should-empty should-occupy)))
    (apply-actions actions)))

(defun grid-counts ()
  (let ((occupied 0)
        (empty 0))

    (loop-grid (row col rows cols)
      (if (occupied-p row col)
          (incf occupied))
      (if (empty-p row col)
          (incf empty)))
    
    (values occupied empty)))

(defun to-string ()
  (with-output-to-string (out)
    (loop-rows (row rows cols)
      (format out "~&~A" (concatenate 'string (make-array cols :displaced-to *grid*
                                                               :displaced-index-offset (* cols row)))))))

(defun execute-all-rounds (should-empty should-occupy)
  (loop
    do (multiple-value-bind (grid-str occupied became-occupied empty became-empty) (execute-round should-empty should-occupy)
         (if (and (= 0 became-occupied) (= 0 became-empty))
             (return occupied)))))

(defun part-1 ()
  (with-file-grid
    (execute-all-rounds #'should-empty-1 #'should-occupy-1)))

(defun part-2 ()
  (with-file-grid
    (execute-all-rounds #'should-empty-2 #'should-occupy-2)))
