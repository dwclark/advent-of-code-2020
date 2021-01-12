(defpackage :day-23
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2 #:small-game #:right-of #:pick-up-cups
           #:contents #:max-val #:min-val #:picked-up #:current #:next-cup
           #:in-picked-up #:dest-cup #:splice #:set-new-current #:game-move
           #:game-label #:score-part-2 #:large-game))

(in-package :day-23)

(defun circular (lst)
  (setf (cdr (last lst)) lst))

(defun reset-list ()
  (circular (list 3 6 2 9 8 1 7 5 4)))

(defparameter *input* (reset-list))
(defparameter *excised* nil)

(setf *print-circle* t)

(defun make-counter (v)
  (lambda ()
    (if (= 1 v)
        (setf v 9)
        (decf v))))

(defun destination-cup (picked-up)
  (loop with counter = (make-counter (first *input*))
        do (let ((next (funcall counter)))
             (if (not (member next picked-up))
                 (return next)))))

(defun find-lst (item)
  (labels ((iter (lst)
             (if (= item (first lst))
                 lst
                 (iter (cdr lst)))))
    (iter *input*)))

(defun excise ()
  (setf *excised* (rest *input*))
  (let ((last-excised (rest (rest (rest *input*))))
        (first-not-excised (rest (rest (rest (rest *input*))))))
    
    (setf (cdr *input*) first-not-excised)
    (setf (cdr last-excised) nil)
    *excised*))

(defun splice-in ()
  (setf (cdr (last *excised*)) (rest *input*))
  (setf (cdr *input*) *excised*)
  *input*)

(defun set-destination ()
  (setf *input* (find-lst (destination-cup *excised*)))
  *input*)

(defun move ()
  (let ((me (first *input*)))
    (excise)
    (set-destination)
    (splice-in)
    (setf *input* (rest (find-lst me)))
    *input*))

(defun labeling ()
  (loop for n from 0 below 8
        for item in (rest (find-lst 1))
        collecting item into ret-list
        finally (return (apply #'concatenate (append (list 'string) (mapcar #'write-to-string ret-list))))))

(defun part-1 ()
  (let ((*input* (reset-list)))
    (loop for n below 100
          do (move)
          finally (return (labeling)))))

;; NOTE, you can solve part-1 with the below, but I kept part-1 as is
;; because that was the original solution. Part one is pretty lisp-y
;; because of the heavy use of list, car, cdr, and cons.

;; Below is solution for part 2 with some part 1 stuff mixed in. This
;; section is about as un-lisp-y as you can get. However, with the
;; types and optimizations, part-2 executes in 0.5 - 0.6 seconds.
;; That's faster than a numpy verion listed here:
;;
;; https://github.com/bsamseth/advent-of-code/blob/master/aoc-2020/day-23/day_23.py

(deftype ary-linked-list ()
  `(simple-array fixnum *))

(defstruct (cup-game (:conc-name nil))
  (contents nil :type ary-linked-list)
  (max-val 0 :type fixnum)
  (min-val 0 :type fixnum)
  (picked-up nil :type (simple-array fixnum *))
  (current 0 :type fixnum))

(defun allocate-small (lst)
  (let* ((iterable (append lst (list (first lst))))
         (max-elem (apply #'max lst))
         (ret-ary (make-array (1+ max-elem) :element-type 'fixnum)))
    (maplist #'(lambda (sub)
                 (if (<= 2 (length sub))
                     (let ((index (first sub))
                           (val (second sub)))
                       (setf (aref ret-ary index) val)))) iterable)
    ret-ary))

(defun small-game (lst)
  (make-cup-game :contents (allocate-small lst)
                 :max-val (apply #'max lst)
                 :min-val (apply #'min lst)
                 :picked-up (make-array 3 :element-type 'fixnum)
                 :current (first lst)))

(defun allocate-large (lst max-val)
  (declare (optimize speed)
           (fixnum max-val))
  
  (let* ((iterable (append lst (list 10)))
         (ret-ary (make-array (1+ max-val) :element-type 'fixnum)))
    (maplist #'(lambda (sub)
                 (if (<= 2 (length sub))
                     (let ((index (first sub))
                           (val (second sub)))
                       (setf (aref ret-ary index) val)))) iterable)
    (loop for n from 10 below max-val
          do (setf (aref ret-ary n) (1+ n)))
    (setf (aref ret-ary max-val) (first lst))
    ret-ary))

(defun large-game (lst max-val)
  (make-cup-game :contents (allocate-large lst max-val)
                 :max-val max-val
                 :min-val (apply #'min lst)
                 :picked-up (make-array 3 :element-type 'fixnum)
                 :current (first lst)))

(declaim (inline pick-up-cups next-cup in-picked-up dest-cup splice set-new-current right-of))

(defun right-of (ary num)
  (declare (optimize speed)
           (ary-linked-list ary)
           (fixnum num))
  (aref ary num))

(defun pick-up-cups (game)
  (declare (optimize speed)
           (cup-game game))
  (let* ((contents (contents game))
         (picked-up (picked-up game))
         (1st (right-of contents (current game)))
         (2nd (right-of contents 1st))
         (3rd (right-of contents 2nd)))
    (setf (aref picked-up 0) 1st
          (aref picked-up 1) 2nd
          (aref picked-up 2) 3rd)))

(defun next-cup (game cup)
  (declare (optimize speed)
           (cup-game game)
           (fixnum cup))
  
  (let ((next (1- cup)))
    (if (<= (min-val game) next)
        next
        (max-val game))))

(defun in-picked-up (game cup)
  (declare (optimize speed)
           (cup-game game)
           (fixnum cup))
  
  (loop with picked-up = (picked-up game)
        for i from 0 below 3
        do (if (= cup (aref picked-up i)) (return t))
        finally (return nil)))

(defun dest-cup (game)
  (declare (optimize speed)
           (cup-game game))
  
  (loop with next = (next-cup game (current game))
        while (in-picked-up game next)
        do (setf next (next-cup game next))
        finally (return next)))

(defun splice (game)
  (declare (optimize speed)
           (cup-game game))
  
  (let* ((contents (contents game))
         (picked-up (picked-up game))
         (current (current game))
         (dest (dest-cup game))
         (saved (right-of contents dest)))
    
    (setf (aref contents current) (right-of contents (aref picked-up 2)))
    (setf (aref contents dest) (aref picked-up 0))
    (setf (aref contents (aref picked-up 2)) saved)))

(defun set-new-current (game)
  (declare (optimize speed)
           (cup-game game))
  (setf (current game) (right-of (contents game) (current game))))

(defun game-move (game)
  (declare (optimize speed)
           (cup-game game))
  
  (pick-up-cups game)
  (splice game)
  (set-new-current game))

(defun game-label (game)
  (loop with contents = (contents game)
        with next = (right-of contents 1)
        while (not (= 1 next))
        collecting next into ret
        do (setf next (right-of contents next))
        finally (return (apply #'concatenate
                               (append (list 'string) (mapcar #'write-to-string ret))))))

(defun score-part-2 (game)
  (let* ((contents (contents game))
         (pos1 (right-of contents 1))
         (pos2 (right-of contents pos1)))
    (* pos1 pos2)))

(defun part-2 ()
  (let ((game (large-game (list 3 6 2 9 8 1 7 5 4) 1000000)))
    (dotimes (n 10000000)
      (game-move game))
    (score-part-2 game)))
