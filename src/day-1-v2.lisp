(defpackage :day-1-v2
  (:use :cl)
  (:import-from :utils :load-numbers)
  (:import-from :alexandria :map-combinations)
  (:export #:part-1 #:part-2))

(in-package :day-1-v2)

(defun solve-it (numbers len)
  (map-combinations #'(lambda (vec)
                        (if (= 2020 (reduce #'+ vec))
                            (return-from solve-it (reduce #'* vec))))
                    numbers :length len :copy nil))

(defun part-1 ()
  (solve-it (load-numbers "1") 2))

(defun part-2 ()
  (solve-it (load-numbers "1") 3))
                                   
