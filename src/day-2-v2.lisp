(defpackage :day-2-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :xor)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-2-v2)

(defun parse-spec (line)
  (do-register-groups ((#'parse-integer s-min s-max) look-for password) ("^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$" line)
    (return (values s-min s-max (elt look-for 0) password))))

(defun test-1 (line)
  (multiple-value-bind (min-val max-val c password) (parse-spec line)
    (<= min-val (count c password) max-val)))

(defun test-2 (line)
  (multiple-value-bind (idx-1 idx-2 c password) (parse-spec line)
    (xor (char= c (char password (1- idx-1)))
         (char= c (char password (1- idx-2))))))

(defun part-1 ()
  (count-if #'test-1 (read-day-file "2")))

(defun part-2 ()
  (count-if #'test-2 (read-day-file "2")))
