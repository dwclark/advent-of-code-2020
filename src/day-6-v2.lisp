(defpackage :day-6-v2
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:import-from :alexandria :curry :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-6-v2)

(defun find-sum (fn)
  (flet ((group-count (group)
           (length (reduce (rcurry fn :test #'char=)
                           (mapcar (curry #'concatenate 'list) group)))))
    (reduce #'+ (mapcar #'group-count (split-blank-lines (read-day-file "6"))))))

(defun part-1 ()
  (find-sum #'union))

(defun part-2 ()
  (find-sum #'intersection))
