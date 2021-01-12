(defpackage :day-10
  (:use :cl)
  (:import-from :utils :load-numbers :power-set)
  (:export #:part-1 #:part-2))

(in-package :day-10)

(defparameter *jolts* #(1 4 5 6 7 10 11 12 15 16 19))

(defun jolt-diffs (sorted)
  (loop with table = (make-hash-table)
        for i from 0 below (1- (length sorted))
        do (let ((diff (- (aref sorted (1+ i)) (aref sorted i))))
             (setf (gethash diff table) (1+ (gethash diff table 0))))
        finally (progn
                  (setf (gethash 3 table) (1+ (gethash 3 table 0))
                        (gethash 1 table) (1+ (gethash 1 table 0)))
                  (return table))))

(defun next-dense-run (sorted start-index)
  (loop with index = start-index
        with size = 1
        for i from start-index below (1- (length sorted))
        do (if (< (- (aref sorted (1+ i)) (aref sorted i)) 3)
               (incf size)
               (if (<= 3 size)
                   (return (cons index size))
                   (setf index (1+ i)
                         size 1)))
        finally (return (if (<= 3 size)
                            (cons index size)
                            (cons 0 0)))))

(defun dense-runs (sorted)
  (loop with ret = nil
        with next = (next-dense-run sorted 0)
        while (/= 0 (cdr next))
        do (progn
             (push (make-array (cdr next) :displaced-to sorted
                                          :displaced-index-offset (car next)) ret)
             (setf next (next-dense-run sorted (+ (car next) (cdr next)))))
        finally (return (nreverse ret))))

(defun is-legal-run (lst begin end)
  (and (<= 2 (length lst))
       (= (first lst) begin)
       (= (car (last lst)) end)
       (= 0 (count-if #'(lambda (n) (< 3 n)) (mapcar #'- (rest lst) lst)))))

(defun combinations (sorted)
  (let* ((runs (mapcar #'(lambda (v)
                           (concatenate 'list v)) (dense-runs sorted)))
         (counts (mapcar #'(lambda (run)
                             (let ((begin (first run))
                                   (end (car (last run))))
                               (count-if #'(lambda (sub-run)
                                             (is-legal-run sub-run begin end)) (power-set run)))) runs)))
    (reduce #'* counts :initial-value 1)))

(defun part-1 ()
  (let ((diffs (jolt-diffs (sort (load-numbers "10") #'<))))
    (* (gethash 1 diffs) (gethash 3 diffs))))

(defun part-2 ()
  (let ((diffs (sort (concatenate 'vector (cons 0 (concatenate 'list (load-numbers "10")))) #'<)))
    (combinations diffs)))
