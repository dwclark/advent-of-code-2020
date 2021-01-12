(defpackage :day-8-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :register-groups-bind)
  (:export #:part-1 #:part-2))

(in-package :day-8-v2)

(defun load-instructions ()
  (labels ((to-symbol (s) (intern (string-upcase s) :day-8-v2)))
    (map 'vector #'(lambda (line)
                     (register-groups-bind ((#'to-symbol op) (#'parse-integer num)) ("([a-z]{3}) ([+-]\\d+)" line)
                       (list op num)))
         (read-day-file "8"))))

(defun machine (code)
  (loop with code-length = (length code)
        with accum = 0
        with ip = 0
        with visited = (make-hash-table)
        do (progn
             
             (when (= ip code-length)
               (return (values accum :normal-termination)))
             
             (when (< code-length ip)
               (return (values accum :illegal-operation)))
             
             (destructuring-bind (op val) (aref code ip)
               (when (gethash ip visited)
                 (return (values accum :infinite-loop)))
               
               (setf (gethash ip visited) t)
               
               (case op
                 (nop (incf ip))
                 (jmp (incf ip val))
                 (acc (progn
                         (incf accum val)
                         (incf ip))))))))

(defun part-1 ()
  (multiple-value-bind (result flag) (machine (load-instructions))
    result))

(defun part-2 ()
  (loop with code = (load-instructions)
        for i from 0 below (length code)
        do (destructuring-bind (op val) (aref code i)
             (case op
               (nop (setf (aref code i) (list 'jmp val)))
               (jmp (setf (aref code i) (list 'nop val))))
             
             (multiple-value-bind (result flag) (machine code)
               (if (eq :normal-termination flag)
                   (return result))
               (setf (aref code i) (list op val))))))
