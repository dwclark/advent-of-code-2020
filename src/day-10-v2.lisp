(defpackage :day-10-v2
  (:use :cl)
  (:import-from :utils :load-numbers :power-set)
  (:export #:part-1 #:part-2))

(in-package :day-10-v2)

(defun jolt-diff-counts (jolts)
  ;bottom is 1, top is always 3 jolt rated, so always start counts at 1
  (loop with sorted = (sort jolts #'<)
        with table = (make-array 4 :element-type 'fixnum :initial-element 1)
        for i from 0 below (1- (length jolts))
        do (incf (aref table (- (aref sorted (1+ i)) (aref sorted i))))
        finally (return table)))

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
  (let ((counts (jolt-diff-counts (load-numbers "10"))))
    (* (aref counts 1) (aref counts 3))))

(defun part-2 ()
  (let ((diffs (sort (concatenate 'vector (cons 0 (concatenate 'list (load-numbers "10")))) #'<)))
    (combinations diffs)))
