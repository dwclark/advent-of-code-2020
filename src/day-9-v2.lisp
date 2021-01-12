(defpackage :day-9-v2
  (:use :cl)
  (:import-from :utils :load-numbers)
  (:import-from :alexandria :map-combinations)
  (:export #:part-1 #:part-2))

(in-package :day-9-v2)

(defun preamble-match-p (ary num)
  (flet ((sums-to (vec) (if (= num (reduce #'+ vec)) (return-from preamble-match-p t))))
    (map-combinations #'sums-to ary :length 2 :copy nil)
    nil))

(defun find-invalid (pre-length nums)
  (loop for i from pre-length below (length nums)
        do (let ((ary (make-array pre-length :displaced-to nums :displaced-index-offset (- i pre-length)))
                 (num (aref nums i)))
             (if (not (preamble-match-p ary num))
                 (return num)))))
  
(defun find-sum-range (target vec)
  (block outer
    (loop for i from 0 below (length vec)
          do (let ((total 0))
               (loop for j from i below (length vec)
                     do (progn
                          (setf total (+ total (aref vec j)))
                          (cond ((= target total) (return-from outer (values i (1+ j))))
                                ((< target total) (return)))))))))

(defun part-1 ()
  (find-invalid 25 (load-numbers "9")))

(defun part-2 ()
  (let* ((numbers (load-numbers "9"))
         (target (find-invalid 25 numbers)))
    (multiple-value-bind (low high) (find-sum-range target numbers)
      (let ((sub (concatenate 'list (subseq numbers low high))))
        (+ (apply #'min sub) (apply #'max sub))))))
