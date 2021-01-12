(defpackage :day-25
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-25)

(defparameter *public-keys* (list 14222596 4057428))
(defparameter *divisor* 20201227)
(defparameter *subject-number* 7)

;; loop size for 4057428 is 2918888
;; loop size for 14222596 is 3616052

(defun execute-loops ()
  (let ((value 1)
        (key-1 (first *public-keys*))
        (key-2 (second *public-keys*))
        (have-key-1 nil)
        (have-key-2 nil))
                      
    (loop with day = 1
          while (or (not have-key-1) (not have-key-2))
          
          do (progn
               (setf value (* value *subject-number*))
               (setf value (rem value *divisor*))
               
               (if (= key-1 value)
                   (progn
                     (format t "~&loop size for ~A is ~A" key-1 day)
                     (setf have-key-1 t)))

               (if (= key-2 value)
                   (progn
                     (format t "~&loop size for ~A is ~A" key-2 day)
                     (setf have-key-2 t)))

               (incf day)))))

(defun make-encryption-key (public-key loop-size)
  (let ((val 1))
    (dotimes (n loop-size)
      (setf val (* val public-key))
      (setf val (rem val *divisor)))
    val))

(defun part-1 ()
  (make-encryption-key 4057428 3616052))
