(defpackage :day-14
  (:use :cl)
  (:import-from :utils :read-day-file :bit-vector->integer)
  (:import-from :alexandria :copy-array :hash-table-alist)
  (:export #:part-1 #:part-2))

(in-package :day-14)

(defparameter *width* 36)
(defparameter *memory-write-version* nil)

(defun str->mask-ops (str)
  (loop for i from 0 below *width*
        for val across str
        with ops = (make-hash-table)
        do (cond ((char= #\1 val) (setf (gethash i ops) 1))
                 ((char= #\0 val) (setf (gethash i ops) 0))
                 ((char= #\X val) (setf (gethash i ops) val)))
        finally (return ops)))

(defun num->bit-vector (arg)
  (loop for i from (1- *width*) downto 0
        with bitvec = (make-array *width* :element-type 'bit)
        with num = arg
        do (progn
             (setf (aref bitvec i) (rem num 2))
             (setf num (floor (/ num 2))))
        finally (return bitvec)))

(defclass can-execute () ())
(defgeneric execute (machine to-execute))

(defclass machine (can-execute)
  ((current-mask :accessor current-mask)
   (memory :initform (make-hash-table) :accessor memory)))

(defmethod sum-memory ((m machine))
  (loop with memory = (memory m)
        for v being the hash-values in memory
        summing v into total
        finally (return total)))

(defmethod execute ((m machine) (instructions cons))
  (dolist (ins instructions)
    (execute m ins)))

(defclass mask-instruction (can-execute)
  ((mask :initarg :mask :accessor mask)))

(defmethod execute ((m machine) (ins mask-instruction))
  (setf (current-mask m) ins))

(defclass memory-write (can-execute)
  ((address :initarg :address :accessor address)
   (value :initarg :value :accessor value)))

(defmethod execute ((m machine) (ins memory-write))
  (loop with mask = (mask (current-mask m))
        with value-vector = (num->bit-vector (value ins))
        with memory = (memory m)
        for key being the hash-keys in mask
        do (if (numberp (gethash key mask))
               (setf (aref value-vector key) (gethash key mask)))
        finally (setf (gethash (address ins) memory) (bit-vector->integer value-vector))))

(defclass memory-write-v2 (memory-write) ())

(defun generate-floating (bits keys to-write)
  (if (null keys)
      (loop with new-bits = (copy-array bits :element-type :bit)
            for cell in to-write
            do (setf (aref new-bits (car cell)) (cdr cell))
            finally (return (list new-bits)))
      (append (generate-floating bits
                                 (rest keys)
                                 (cons (cons (first keys) 1) to-write))
              (generate-floating bits
                                 (rest keys)
                                 (cons (cons (first keys) 0) to-write)))))
      
(defmethod execute ((m machine) (ins memory-write-v2))
  (let* ((mask (mask (current-mask m)))
         (value (value ins))
         (memory (memory m))
         (solid-address-bits
           (loop with address-vector = (num->bit-vector (address ins))
                 for key being the hash-keys in mask
                 do (if (and (numberp (gethash key mask))
                             (= 1 (gethash key mask)))
                        (setf (aref address-vector key) (gethash key mask)))
                 finally (return address-vector)))
         (floating-keys (mapcar #'car (remove-if #'numberp (hash-table-alist mask) :key #'cdr)))
         (addresses (generate-floating solid-address-bits floating-keys nil)))
    (loop for bits in addresses
          do (setf (gethash (bit-vector->integer bits) memory) value))))

(defun make-instruction (str)
  (cl-ppcre:do-register-groups (the-mask) ("mask = ([X10]{36})" str)
    (return-from make-instruction (make-instance 'mask-instruction :mask (str->mask-ops the-mask))))
  
  (cl-ppcre:do-register-groups ((#'parse-integer address value)) ("mem\\[([0-9]+)\\] = ([0-9]+)" str)
    (return-from make-instruction (make-instance *memory-write-version* :address address :value value)))
  
  nil)

(defun load-instructions ()
  (mapcar #'make-instruction (read-day-file "14")))

(defun part-1 ()
  (let* ((*memory-write-version* 'memory-write)
         (m (make-instance 'machine))
         (instructions (load-instructions)))
    (execute m instructions)
    (sum-memory m)))

(defun part-2 ()
  (let* ((*memory-write-version* 'memory-write-v2)
         (m (make-instance 'machine))
         (instructions (load-instructions)))
    (execute m instructions)
    (sum-memory m)))
