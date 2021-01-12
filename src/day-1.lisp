(defpackage :day-1
  (:use :cl)
  (:import-from :utils :load-numbers)
  (:export #:part-1 #:part-2))

(in-package :day-1)

(defconstant +target+ 2020)

(defun part-1 ()
  (loop with numbers = (load-numbers "1")
        for num across numbers
        until (find (- +target+ num) numbers)
        finally (return (* num (- +target+ num)))))

(defun part-2 ()
  (loop with numbers = (load-numbers "1")
        for i from 0 below (length numbers)
        do (loop for j from (1+ i) below (length numbers)
                 do (loop for k from (1+ j) below (length numbers)
                          do (let ((i-val (aref numbers i))
                                   (j-val (aref numbers j))
                                   (k-val (aref numbers k)))
                               (if (= +target+ (+ i-val j-val k-val))
                                   (return-from part-2 (* i-val j-val k-val))))))))
                                   
