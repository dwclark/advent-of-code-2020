(defpackage :day-13
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-13)

(defstruct terminal earliest ids)

(defun parse-ids (term)
  (mapcar #'parse-integer
          (remove-if #'(lambda (s) (string= "x" s)) (terminal-ids term))))
  
(defun load-terminal ()
  (let* ((lines (read-day-file "13")))
    (make-terminal :earliest (parse-integer (first lines))
                   :ids (cl-ppcre:split "," (car (rest lines))))))

(defun map-soonest (term)
  (reduce #'(lambda (f s)
              (if (< (car f) (car s))
                  f
                  s))
          (mapcar #'(lambda (id)
                      (cons (- (* (ceiling (/ (terminal-earliest term) id)) id)
                               (terminal-earliest term))
                            id))
                  (parse-ids term))))

;;Note, ids->vec, pre-process, fits, and find-contiguous was my building a
;;naive sieve hoping that it would be fast enough. Calling find-contiguous does produce the
;;correct results, and faster than the super naive solution of just
;;searching one by one. However, it's not fast enough to do part-2 in
;;any reasonable length of time.
(defun ids->vec (term)
  (let ((vec (make-array (length (terminal-ids term)) :fill-pointer 0)))
    (loop for elem in (terminal-ids term)
          do (if (string= "x" elem)
                 (vector-push 0 vec)
                 (vector-push (parse-integer elem) vec)))
    vec))

(defun pre-process (vec)
  (let ((table (make-hash-table))
        (max-id (reduce #'max vec))
        (max-index nil))
    
    (loop for i from 0 below (length vec)
          do (if (= max-id (aref vec i)) (setf max-index i)))
    
    (loop for i from 0 below (length vec)
          do (if (and (not (zerop (aref vec i)))
                      (not (= max-index i)))
                 (setf (gethash (aref vec i) table) (- (- max-index i)))))
    
    (values max-id max-index table)))

(defun fits (multiplier max-id max-index table)
  (let ((target (* multiplier max-id)))
    
    (loop for key being the hash-keys in table
          do (let* ((val key)
                    (val-index-diff (gethash key table))
                    (val-target (+ target val-index-diff)))
               
               (if (not (zerop (rem val-target val)))
                   (return nil)))
          finally (return t))))

(defun find-contiguous (term)
  (let* ((vec (ids->vec term)))
    (multiple-value-bind (max-id max-index table) (pre-process vec)
      (loop for i from 1 to most-positive-fixnum
            do (if (fits i max-id max-index table)
                   (let* ((first-val (aref vec 0))
                          (first-val-diff (gethash first-val table)))
                     (return (+ first-val-diff (* i max-id)))))))))

(defun part-1 ()
  (let* ((term (load-terminal))
         (soonest (map-soonest term)))
    (* (car soonest) (cdr soonest))))

;;The following 3 sieve functions were translated from this python code
;;https://gist.github.com/hamidazimy/56a8495aea39f79d6de9f372c259f7fe
;;However, I did add the sieve-legal-p to validate that the code will
;;actually give the correct result. The Chinese Remainder Theorem says that
;;this kind of algorithm will work, provided all of the divisors are coprime,
;;which is what sieve-legal-p checks for.

;;determine the proper remainders needed, take a simple case: 13,11
;;since 13 is in position 0, the remainder needed is zero (needs to evenly divide)
;;next, we need 11 to divide n times into the result with a remainder of 10.
;;Then, n+1 will hit the index exactly
(defun sieve-pairs (list-str-ids)
  (let ((ary (concatenate 'vector list-str-ids)))
    (remove-if #'null
               (loop for i from 0 below (length ary)
                     collecting (if (string= "x" (aref ary i))
                                    nil
                                    (let ((val (parse-integer (aref ary i))))
                                      (cons val (mod (- val i) val))))))))

;;validates that all divisors are coprime
(defun sieve-legal-p (pairs)
  (= 1 (apply #'gcd (mapcar 'car pairs))))

;;Basic idea: gradually build up results so that each previous pair still
;;divides into the new results with the proper remainder.
;;simple example, take 13,11,7
;;since the increment starts at 1, we will add up until we get to 13, at
;;this point it divides into 13 with a remainder of 0.
;;now keep adding 13 onto the result until we get to a number that 11 divides with
;;a remainder of 10. We arrive at 65. Now keep adding 65 onto the result until
;;we get a remainder of 5. We get 208, which is what we are looking for.
;;Verifying manually: (rem 208 13) = 0, (rem 208 11) = 10 (rem 208 7) = 5.
(defun crt-sieve (pairs)
  (let ((result 0)
        (increment 1))
    (dolist (pair pairs)
      (loop while (not (= (mod result (car pair)) (cdr pair)))
            do (incf result increment))
      (setf increment (* increment (car pair))))
    result))
      
(defun part-2 ()
  (let* ((term (load-terminal))
         (pairs (sieve-pairs (terminal-ids term))))
    (if (sieve-legal-p pairs)
        (crt-sieve pairs)
        "sieve is not guaranteed to give the smallest result because divisors are not coprime")))
