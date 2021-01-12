(defpackage :day-12
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-12)

(defparameter *north* 0)
(defparameter *east* 1)
(defparameter *south* 2)
(defparameter *west* 3)

(defparameter *ew* 0)
(defparameter *ns* 1)

(defclass game ()
  ((travelled :initform (vector 0 0) :accessor travelled)))

(defgeneric manhattan-distance (game))
(defgeneric to-string (game))
(defgeneric N (game delta))
(defgeneric S (game delta))
(defgeneric E (game delta))
(defgeneric W (game delta))
(defgeneric R (game delta))
(defgeneric L (game delta))
(defgeneric F (game delta))

(defmethod manhattan-distance ((g game))
  (with-slots (travelled) g
    (+ (abs (aref travelled *ew*))
       (abs (aref travelled *ns*)))))

;;pointer game
(defclass pointer-game (game)
  ((pointing :initform *east* :accessor pointing)))

(defmethod N ((g pointer-game) delta)
  (with-slots (travelled) g
    (incf (aref travelled *ns*) delta)))

(defmethod S ((g pointer-game) delta)
  (with-slots (travelled) g
    (decf (aref travelled *ns*) delta)))

(defmethod E ((g pointer-game) delta)
  (with-slots (travelled) g
    (incf (aref travelled *ew*) delta)))

(defmethod W ((g pointer-game) delta)
  (with-slots (travelled) g
    (decf (aref travelled *ew*) delta)))

(defmethod R ((g pointer-game) delta)
  (with-slots (pointing) g
    (let ((rotate (/ delta 90)))
      (setf pointing (mod (+ pointing rotate) 4)))))

(defmethod L ((g pointer-game) delta)
  (with-slots (pointing) g
    (let ((rotate (/ delta 90)))
      (setf pointing (mod (- pointing rotate) 4)))))

(defmethod F ((g pointer-game) delta)
  (with-slots (pointing travelled) g
    (let ((axis (if (or (= pointing *east*) (= pointing *west*)) *ew* *ns*))
          (dirfunc (if (or (= pointing *east*) (= pointing *north*)) #'+ #'-)))
      (incf (aref travelled axis) (funcall dirfunc delta)))))

(defmethod to-string ((g pointer-game))
  (format nil "position ~A" (travelled g)))

;; waypoint game
(defclass waypoint-game (game)
  ((waypoint :initform (vector 10 1) :accessor waypoint)))

(defmethod N ((g waypoint-game) delta)
  (with-slots (waypoint) g
    (incf (aref waypoint *ns*) delta)))

(defmethod S ((g waypoint-game) delta)
  (with-slots (waypoint) g
    (decf (aref waypoint *ns*) delta)))

(defmethod E ((g waypoint-game) delta)
  (with-slots (waypoint) g
    (incf (aref waypoint *ew*) delta)))

(defmethod W ((g waypoint-game) delta)
  (with-slots (waypoint) g
    (decf (aref waypoint *ew*) delta)))

(defmethod L ((g waypoint-game) delta)
  (R g (- 360 delta)))

(defmethod R ((g waypoint-game) delta)
  (with-slots (waypoint) g
    (case delta
      (90 (setf waypoint (vector (aref waypoint *ns*)
                                 (- (aref waypoint *ew*)))))
      (180 (setf waypoint (vector (- (aref waypoint *ew*))
                                  (- (aref waypoint *ns*)))))
      (270 (setf waypoint (vector (- (aref waypoint *ns*))
                                  (aref waypoint *ew*)))))))

(defmethod F ((g waypoint-game) multiplier)
  (with-slots (travelled waypoint) g
    (incf (aref travelled *ew*) (* multiplier (aref waypoint *ew*)))
    (incf (aref travelled *ns*) (* multiplier (aref waypoint *ns*)))))

(defmethod to-string ((g waypoint-game))
  (format nil "position ~A waypoint ~A" (travelled g) (waypoint g)))

(defun decode (line)
  (cl-ppcre:do-register-groups (letter (#'parse-integer delta)) ("([NSEWRLF])([0-9]{1,})" line)
    (let ((symbol (intern (string-upcase letter) :day-12)))
      (return (cons symbol delta)))))

(defun load-instructions ()
  (mapcar #'decode (read-day-file "12")))

(defun execute (instructions the-game)
  (dolist (cell instructions)
    (funcall (car cell) the-game (cdr cell))
    (format t "~&~A" (to-string the-game))))

(defun play (g)
  (execute (load-instructions) g)
  (manhattan-distance g))

(defun part-1 ()
  (play (make-instance 'pointer-game)))

(defun part-2 ()
  (play (make-instance 'waypoint-game)))
