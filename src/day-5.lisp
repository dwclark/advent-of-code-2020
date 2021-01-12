(defpackage :day-5
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-5)

(defun partition->number (str low high)
  (if (= low high)
      low
      (let ((c (char str 0)))
        (if (or (char= c #\R) (char= c #\B))
            (partition->number (subseq str 1) (ceiling (/ (+ low high) 2)) high)
            (partition->number (subseq str 1) low (floor (/ (+ low high) 2)))))))

(defun find-column (str)
  (partition->number (subseq str 7) 0 7))

(defun find-row (str)
  (partition->number (subseq str 0 7) 0 127))

(defun seat-id (str)
  (let ((row (find-row str))
        (column (find-column str)))
    (+ (* 8 row) column)))

(defun find-missing (sorted)
  (loop for f in sorted
        for s in (rest sorted)
        do (if (= 2 (- s f))
               (return (1+ f)))))

(defun part-1 ()
  (reduce #'max (mapcar #'seat-id (read-day-file "5"))))

(defun part-2 ()
  (find-missing (sort (mapcar #'seat-id (read-day-file "5")) #'<)))
