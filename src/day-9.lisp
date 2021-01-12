(defpackage :day-9
  (:use :cl)
  (:import-from :utils :load-numbers)
  (:export #:part-1 #:part-2))

(in-package :day-9)

(defun build-preamble-search (low high vec)
  (loop for i from low below high
        with hash-table = (make-hash-table)
        do (setf (gethash (aref vec i) hash-table) (aref vec i))
        finally (return hash-table)))

(defun validate-run (at preamble-length vec)
  (loop with low-index = (- at preamble-length)
        with search-table = (build-preamble-search low-index at vec)
        with look-for = (aref vec at)
        for key being the hash-keys in search-table
        do (let ((need (- look-for key)))
             (if (gethash need search-table)
                 (return :valid)))
        finally (return :invalid)))

(defun find-invalid (preamble-length vec)
  (loop for i from preamble-length below (length vec)
        do (if (eq :invalid (validate-run i preamble-length vec))        
               (return (aref vec i)))
        finally (return :valid)))

(defun find-sum-range (target vec)
  (block outer
    (loop for i from 0 below (length vec)
          do (let ((total 0))
               (loop for j from i below (length vec)
                     do (progn
                          (setf total (+ total (aref vec j)))
                          (cond ((= target total) (return-from outer (list i j)))
                                ((< target total) (return)))))))))

(defun slice (ary low high)
  (loop with ret = (make-array (- high low))
        with idx = -1
        for i from low below high
        do (setf (aref ret (incf idx)) (aref ary i))
        finally (return ret)))

(defun part-1 ()
  (find-invalid 25 (load-numbers "9")))

(defun part-2 ()
  (let* ((numbers (load-numbers "9"))
         (target (find-invalid 25 numbers))
         (found (find-sum-range target numbers))
         (sub (slice numbers (first found) (1+ (second found))))
         (sorted (sort sub #'<)))
    (+ (aref sorted 0)
       (aref sorted (1- (length sorted))))))
