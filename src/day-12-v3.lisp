(defpackage :day-12-v3
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-12-v3)

(defun instructions ()
  (flet ((parse (s)
           (list (char s 0) (parse-integer s :start 1))))
    (mapcar #'parse (read-day-file "12"))))

(defun part-1 ()
  (let ((x 0) (y 0) (dir 90))
    (loop for (action amount) in (instructions)
          do (ecase action
               (#\N (incf y amount))
               (#\S (decf y amount))
               (#\E (incf x amount))
               (#\W (decf x amount))
               (#\L (setf dir (mod (- dir amount) 360)))
               (#\R (setf dir (mod (+ dir amount) 360)))
               (#\F (ecase dir
                      (0 (incf y amount))
                      (90 (incf x amount))
                      (180 (decf y amount))
                      (270 (decf x amount)))))
          finally (return (+ (abs x) (abs y))))))

(defun part-2 ()
  (let ((x 0) (y 0) (wx 10) (wy 1))
    (loop for (action amount) in (instructions)
          do (ecase action
               (#\N (incf wy amount))
               (#\S (decf wy amount))
               (#\E (incf wx amount))
               (#\W (decf wx amount))
               (#\L (ecase amount
                      (90 (psetf wx (- wy) wy wx))
                      (180 (psetf wx (- wx) wy (- wy)))
                      (270 (psetf wx wy wy (- wx)))))
			   (#\R (case amount
                      (90 (psetf wx wy wy (- wx)))
                      (180 (psetf wx (- wx) wy (- wy)))
                      (270 (psetf wx (- wy) wy wx))))
               (#\F (setf x (+ x (* wx amount)) y (+ y (* wy amount)))))
          finally (return (+ (abs x) (abs y))))))
