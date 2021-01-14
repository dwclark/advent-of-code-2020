(defpackage :day-14-v2
  (:use :cl)
  (:import-from :utils :read-day-file :bit-vector->integer :integer->bit-vector :power-set)
  (:import-from :alexandria :copy-array :hash-table-values :curry)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-14-v2)

(defparameter *width* 36)

(defun str->mask-ops (str)
  (loop for i from 0 below (length str)
        collect (cons i (digit-char-p (char str i)))))

(defun parse-instruction (str)
  (do-register-groups (the-mask) ("mask = ([X10]{36})" str)
    (return-from parse-instruction (cons :mask (str->mask-ops the-mask))))
  
  (do-register-groups ((#'parse-integer address value)) ("mem\\[([0-9]+)\\] = ([0-9]+)" str)
    (return-from parse-instruction (cons address (integer->bit-vector *width* value)))))

(defun load-instructions ()
  (mapcar #'parse-instruction (read-day-file "14")))

(defun set-bits (ary list-conses)
  (loop with copy = (copy-array ary)
        for (index . val) in list-conses
        do (setf (aref copy index) val)
        finally (return copy)))

(defun execute-instructions (exec instructions)
  (loop with mask = nil
        with memory = (make-hash-table)
        for (address . value) in instructions
        do (if (eq address :mask)
               (setf mask value)
               (funcall exec memory mask address value))
        finally (return memory)))

(defun exec-v1 (memory mask address value)
  (setf (gethash address memory)
        (bit-vector->integer (set-bits value (remove-if #'null mask :key #'cdr)))))

(defun exec-v2 (memory mask address value)
  (let* ((addr-vec (set-bits (integer->bit-vector *width* address) (remove-if-not (curry #'eql 1) mask :key #'cdr)))
         (indexes (mapcar #'car (remove-if-not #'null mask :key #'cdr)))
         (combos (power-set indexes)))
    (labels ((comb->idxpair (comb idx) (cons idx (if (member idx comb) 1 0))) 
             (comb->conses (comb) (mapcar (curry #'comb->idxpair comb) indexes)))
      
      (dolist (comb combos)
        (setf (gethash (bit-vector->integer (set-bits addr-vec (comb->conses comb))) memory)
              (bit-vector->integer value))))))

(defun part-1 ()
  (sum-memory (execute-instructions #'exec-v1 (load-instructions))))
    
(defun part-2 ()
  (sum-memory (execute-instructions #'exec-v2 (load-instructions))))
