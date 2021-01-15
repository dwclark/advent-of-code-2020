(defpackage :day-7-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :curry :hash-table-keys)
  (:import-from :fare-memoization :memoize)
  (:import-from :cl-ppcre :do-register-groups :split)
  (:export #:part-1 #:part-2 #:parse-bag #:load-bag-rules))

(in-package :day-7-v2)

(defun parse-bag-color (str)
  (do-register-groups ((#'intern color)) ("(\\w{1,} \\w{1,}) bags" str)
    (return color)))

(defun parse-bag-contents (str)
  (let (accum)
    (do-register-groups ((#'parse-integer num) (#'intern color)) ("([0-9]{1,}) (\\w{1,} \\w{1,}) bag" str)
      (push (list color num) accum))
    (nreverse accum)))

(defun parse-bag (line table)
  (let ((split (split "contain" line)))
    (setf (gethash (parse-bag-color (first split)) table)
          (parse-bag-contents line))))

(defun bag-color (lst) (first lst))
(defun bag-count (lst) (second lst))

(defun load-bag-rules ()
  (let ((table (make-hash-table :test 'eq)))
    (dolist (line (read-day-file "7")) (parse-bag line table))
    table))

(defparameter *rules* nil)

(defun has (search-color)
  (let ((interned (intern search-color)))
    (labels ((search-func (color) (eq interned color))
             (contains (color)
               (let ((contents (gethash color *rules*)))
                 (or (find-if (curry #'eq (intern search-color)) contents :key #'bag-color)
                     (some #'contains (mapcar #'bag-color contents))))))
      (memoize 'contains)
      #'contains)))

(defun total (color)
  (flet ((sum-contents (bag) (+ (bag-count bag) (* (bag-count bag) (total (bag-color bag))))))
    (reduce #'+ (mapcar #'sum-contents (gethash color *rules*)))))
                     
(defun part-1 ()
  (let ((*rules* (load-bag-rules)))
    (count-if (has "shiny gold") (hash-table-keys *rules*))))

(defun part-2 ()
  (let ((*rules* (load-bag-rules)))
    (total (intern "shiny gold"))))
