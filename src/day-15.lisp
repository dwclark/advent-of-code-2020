;;this is probably a good file to test out optimizations
;;through the use of declarations. Just stopping some/all of the
;;unboxing operations in the array will probably drastically reduce
;;memory consumption and speed things up.
(defpackage :day-15
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-15)

(defparameter *board* nil)

(declaim (inline times-seen turn-diff visit announce))

(deftype game-vector ()
  `(simple-array fixnum *))

(declaim (ftype (function (fixnum fixnum) game-vector) announce))
(defun announce (num turn)
  (declare (optimize speed))
  
  (let ((vec (gethash num *board*)))
    (if (null vec)
        (let ((new-vec (make-array 3 :initial-contents (list num turn -1) :element-type 'fixnum)))
          (setf (gethash num *board*) new-vec)
          new-vec)
        (visit vec turn))))

(declaim (ftype (function (game-vector) fixnum) times-seen))
(defun times-seen (vec)
  (declare (optimize speed))
  (if (= -1 (aref vec 2)) 1 2))

(declaim (ftype (function (game-vector) fixnum) turn-diff))
(defun turn-diff (vec)
  (declare (optimize speed))
  (- (aref vec 1) (aref vec 2)))

(declaim (ftype (function (game-vector fixnum) game-vector) visit))
(defun visit (vec turn)
  (declare (optimize speed))
  (setf (aref vec 2) (aref vec 1))
  (setf (aref vec 1) turn)
  vec)

(declaim (ftype (function (cons) game-vector) reset-board))
(defun reset-board (initial-plays)
  (setf *board* (make-hash-table :test 'eq))
  (loop for i from 0 below (length initial-plays)
        do (announce (nth i initial-plays) i)
        finally (return (gethash (car (last initial-plays)) *board*))))

(defun play (max-turns &rest start-list)
  (declare (optimize speed))
  
  (loop with prev-vec = (reset-board start-list)
        for i from (length start-list) below max-turns
        do (cond ((= 1 (times-seen prev-vec)) (setf prev-vec (announce 0 i)))
                 (t (setf prev-vec (announce (turn-diff prev-vec) i))))
        finally (return prev-vec)))

(defun part-1 ()
  (aref (play 2020 2 20 0 4 1 17) 0))

(defun part-2 ()
  (aref (play 30000000 2 20 0 4 1 17) 0))
