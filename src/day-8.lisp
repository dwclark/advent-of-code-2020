(defpackage :day-8
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-8)

(defun load-instructions ()
  (map 'vector #'(lambda (line)
                   (let ((lst (cl-ppcre:split " " line)))
                     (list (intern (string-upcase (first lst)) :day-8) (parse-integer (second lst)))))
       (read-day-file "8")))

(defun machine (code)
  (loop with code-length = (length code)
        with accum = 0
        with ip = 0
        with visited = (make-hash-table)
        do (progn
             
             (when (= ip code-length)
               (return (list accum :normal-termination)))
             
             (when (< code-length ip)
               (return (list accum :illegal-operation)))
             
             (let ((instruction (aref code ip)))
               (when (gethash ip visited)
                 (return (list accum :infinite-loop)))
               
               (setf (gethash ip visited) ip)
               
               (case (first instruction)
                 ('nop (incf ip))
                 ('jmp (incf ip (second instruction)))
                 ('acc (progn
                         (incf accum (second instruction))
                         (incf ip))))))))

(defun part-1 ()
  (machine (load-instructions)))

(defun part-2 ()
  (loop with code = (load-instructions)
        for i from 0 below (length code)
        do (let ((original (aref code i))
                 (result nil))
             (case (first original)
               (nop (setf (aref code i) (list 'jmp (second original))))
               (jmp (setf (aref code i) (list 'nop (second original)))))
             
             (setf result (machine code))
             (if (eq :normal-termination (second result))
                 (return (first result)))
             (setf (aref code i) original))))
