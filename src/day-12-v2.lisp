(defpackage :day-12-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-12-v2)

(defparameter +directions+ '((N . (0 . 1)) (E . (1 . 0)) (S . (0 . -1)) (W . (-1 . 0))))
(defparameter *position* '(0 . 0))
(defparameter *facing* '(1 0))
(defparameter *waypoint* '(10 . 1))

(defun load-instructions ()
  (flet ((decode (s) (cons (intern (string-upcase (subseq s 0 1)) :day-12-v2) (parse-integer (subseq s 1)))))
    (mapcar #'decode (read-day-file "12"))))

(defun manhattan-distance ()
  (+ (abs (car *position*)) (abs (cdr *position*))))

(defun move (current dir delta)
  (cons (+ (car current) (* delta (car dir)))
        (+ (cdr current) (* delta (cdr dir)))))

(defun rotate (cell delta)
  (if (zerop delta) cell (rotate (cons (cdr cell) (- (car cell))) (- delta 90))))

(defun play-game (func)
  (let ((*position* '(0 . 0))
        (*facing* '(1 . 0))
        (*waypoint* '(10 . 1)))
    (dolist (i (load-instructions))
      (destructuring-bind (pre . delta) i
        (funcall func pre delta)))
    (manhattan-distance)))

(defun part-1 ()
  (flet ((game-func (pre delta)
           (ecase pre
             ((N S E W) (setf *position* (move *position* (cdr (assoc pre +directions+)) delta)))
             (F (setf *position* (move *position* *facing* delta)))
             (R (setf *facing* (rotate *facing* delta)))
             (L (setf *facing* (rotate *facing* (- 360 delta)))))))
    (play-game #'game-func)))

(defun part-2 ()
  (flet ((game-func (pre delta)
           (ecase pre
             ((N S E W) (setf *waypoint* (move *waypoint* (cdr (assoc pre +directions+)) delta)))
             (F (setf *position* (move *position* *waypoint* delta)))
             (R (setf *waypoint* (rotate *waypoint* delta)))
             (L (setf *waypoint* (rotate *waypoint* (- 360 delta)))))))
    (play-game #'game-func)))
