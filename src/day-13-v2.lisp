(defpackage :day-13-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :curry)
  (:import-from :cl-ppcre :split)
  (:export #:part-1 #:part-2))

(in-package :day-13-v2)

(defun load-terminal ()
  (destructuring-bind (str str-ids) (read-day-file "13")
    (values (parse-integer str)
            (loop for id in (split "," str-ids) and index from 0
                  collecting (if (string= id "x") nil (cons (parse-integer id) index)) into results
                  finally (return (remove-if #'null results))))))

(defun soonest (earliest ids)
  (labels ((arrival (id) (- (* (ceiling (/ earliest id)) id) earliest))
           (arrival-cell (id) (cons (arrival id) id))
           (least-cell (c1 c2) (if (< (car c1) (car c2)) c1 c2)))
    (let ((arrival-cells (mapcar #'arrival-cell ids)))
      (reduce #'least-cell arrival-cells :initial-value (first arrival-cells)))))

(defun part-1 ()
  (multiple-value-bind (earliest ids-indexes) (load-terminal)
    (destructuring-bind (minutes . id) (soonest earliest (mapcar #'car ids-indexes))
      (* minutes id))))

(defun sieve-pairs (ids-indexes)
  (loop for (val . i) in ids-indexes
        collecting (cons val (mod (- val i) val))))

(defun sieve-legal-p (pairs)
  (= 1 (apply #'gcd (mapcar 'car pairs))))

(defun crt-sieve (pairs)
  (loop with result = 0
        with increment = 1
        for (id . index) in pairs
        do (loop while (/= (mod result id) index)
                 do (incf result increment)
                 finally (setf increment (* increment id)))
        finally (return result)))

(defun part-2 ()
  (multiple-value-bind (earliest ids-indexes) (load-terminal)
    (let ((pairs (sieve-pairs ids-indexes)))
      (if (sieve-legal-p pairs)
          (crt-sieve pairs)
          "sieve is not guaranteed to give the smallest result because divisors are not coprime"))))
