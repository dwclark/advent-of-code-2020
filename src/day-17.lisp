(defpackage :day-17
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-17)

(defparameter *active* #\#)

(defun make-space ()
  (make-hash-table :test 'equal))

(defparameter *dimensions* 3)
(defparameter *space* (make-space))

(defun is-active (table cube)
  (multiple-value-bind (val present) (gethash cube table)
    present))

(defun put-cube (table cube)
  (setf (gethash cube table) :active))
    
(defun backfill (&rest elements)
  (append elements (make-list (- *dimensions* (length elements)) :initial-element 0)))

(defun load-space ()
  (loop with ret = (make-space)
        with lines = (read-day-file "17")
        for line in lines
        for y-val from 0 below (length lines)
        do (loop for c across line
                 for x-val from 0 below (length line)
                 do (if (char= *active* c)
                        (put-cube ret (backfill x-val y-val))))
        finally (return ret)))

(defun visit-all-neighbors (func state cube diffs)
  (if (= *dimensions* (length diffs))
      (if (notevery #'zerop diffs)
          (funcall func state (mapcar #'+ cube diffs)))
      (loop for diff from -1 to 1
            do (visit-all-neighbors func state cube (append diffs (list diff)))))
  state)

(defun visit-space (func state current-dimensions cube)
  (if (null current-dimensions)
      (funcall func state cube)
      (loop with dimension = (first current-dimensions)
            for x from (1- (car dimension)) to (1+ (cdr dimension))
            do (visit-space func state (rest current-dimensions) (append cube (list x)))))
  state)

(defun starting-dimension ()
  (loop for key being the hash-keys in *space*
        do (return (mapcar #'(lambda (x) (cons x x)) key))))

(defun current-dimensions ()
  (loop with dimensions = (starting-dimension)
        for cube being the hash-keys in *space*
        do (mapcar #'(lambda (cell val)
                       (setf (car cell) (min val (car cell)))
                       (setf (cdr cell) (max val (cdr cell)))) dimensions cube)
        finally (return dimensions)))

(defun accumulate-neighbor-counts (hash-table cube)
  (if (is-active *space* cube)
      (put-cube hash-table cube)))

(defun accumulate-actives (next-round-active cube)
  (let ((active-neighbors (visit-all-neighbors #'accumulate-neighbor-counts (make-space) cube nil)))
    
    (if (and (is-active *space* cube)
             (<= 2 (hash-table-count active-neighbors) 3))
        (put-cube next-round-active cube))

    (if (and (not (is-active *space* cube))
             (= 3 (hash-table-count active-neighbors)))
        (put-cube next-round-active cube))
    next-round-active))
  
(defun run-cycle ()
  (setf *space* (visit-space #'accumulate-actives (make-space) (current-dimensions) nil)))

(defun run-game (times)
  (let ((*space* (load-space)))
    (dotimes (n times)
      (run-cycle))
    (hash-table-count *space*)))

(defun part-1 ()
  (let ((*dimensions* 3))
    (run-game 6)))

(defun part-2 ()
  (let ((*dimensions* 4))
    (run-game 6)))
