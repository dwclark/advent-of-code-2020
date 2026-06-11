(defpackage :day-11-v3
  (:use :cl)
  (:import-from :alexandria :define-constant)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(defun seats ()
  (coerce (read-day-file "11") 'vector))

(defparameter *grid* nil)
(defparameter *max-rows* 0)
(defparameter *max-cols* 0)

(define-constant +empty+ #\L :test #'char=)
(define-constant +floor+ #\.  :test #'char=)
(define-constant +occupied+ #\#  :test #'char=)
(define-constant +moves+ '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1)) :test #'equal)

(defun g[] (row col)
  (aref (aref *grid* row) col))

(defun adjacent (row col)
  (let ((ret 0))
    (dolist (move +moves+)
      (let ((test-row (+ row (car move)))
            (test-col (+ col (cdr move))))
        (if (and (< -1 test-row *max-rows*) (< -1 test-col *max-cols*)
                 (char= (g[] test-row test-col) +occupied+))
            (incf ret))))
    ret))

(defun line-of-sight (row col)
  (let ((ret 0))
    (dolist (move +moves+)
      (let ((multiplier 1))
        (loop do (let ((test-row (+ row (* multiplier (car move))))
                       (test-col (+ col (* multiplier (cdr move)))))
                   (if (and (< -1 test-row *max-rows*) (< -1 test-col *max-cols*))
                       (case (g[] test-row test-col)
                         (#.+occupied+ (incf ret) (loop-finish))
                         (#.+empty+ (loop-finish))
                         (t (incf multiplier)))
                       (loop-finish))))))
    ret))

(defun apply-changes (changes)
  (loop for change across changes
        do (multiple-value-bind (row col e) (values (aref change 0) (aref change 1) (aref change 2))
             (setf (aref (aref *grid* row) col) e))))

(defun count-grid (e)
  (loop for line across *grid*
        summing (count e line :test #'char=)))

(defun simulate (func limit)
  (loop do (let ((changes (make-array 0 :adjustable t :fill-pointer 0)))
             (loop for row from 0 below *max-rows*
                   do (loop for col from 0 below *max-cols*
                            do (let ((contents (g[] row col)))
                                 (if (not (char= contents +floor+))
                                     (let ((num-occupied (funcall func row col)))
                                       (cond
                                         ((and (char= contents +empty+) (zerop num-occupied))
                                          (vector-push-extend (vector row col +occupied+) changes))
                                         ((and (char= contents +occupied+) (<= limit num-occupied))
                                          (vector-push-extend (vector row col +empty+) changes))))))))
             (if (not (zerop (length changes)))
                 (apply-changes changes)
                 (loop-finish)))))

(defmacro with-grid (&body body)
  `(let* ((*grid* (seats))
          (*max-rows* (length *grid*))
          (*max-cols* (length (aref *grid* 0))))
     ,@body))

(defun part-1 ()
  (with-grid 
    (simulate #'adjacent 4)
    (count-grid +occupied+)))

(defun part-2 ()
  (with-grid
    (simulate #'line-of-sight 5)
    (count-grid +occupied+)))
