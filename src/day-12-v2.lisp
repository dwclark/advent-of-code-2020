(defpackage :day-12-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :define-constant)
  (:export #:part-1 #:part-2))

(in-package :day-12-v2)

(define-constant +directions+ '((N . (0 . 1)) (E . (1 . 0)) (S . (0 . -1)) (W . (-1 . 0))) :test #'equal)

(defun load-instructions ()
  (flet ((decode (s) (cons (intern (string-upcase (subseq s 0 1)) :day-12-v2) (parse-integer (subseq s 1)))))
    (mapcar #'decode (read-day-file "12"))))

(defun manhattan-distance (pos)
  (+ (abs (car pos)) (abs (cdr pos))))

(defun move (current dir delta)
  (cons (+ (car current) (* delta (car dir)))
        (+ (cdr current) (* delta (cdr dir)))))

(defun rotate (cell delta)
  (if (zerop delta) cell (rotate (cons (cdr cell) (- (car cell))) (- delta 90))))

(defun play-game (init-pos init-rot decision)
  (let ((pos init-pos) (rot init-rot))
    (loop for (i . delta) in (load-instructions)
          do (setf (values pos rot) (funcall decision pos rot i delta)))
    (manhattan-distance pos)))
  
(defun part-1 ()
  (flet ((game-func (pos rot pre delta)
           (ecase pre
             ((N S E W) (values (move pos (cdr (assoc pre +directions+)) delta) rot))
             (F (values (move pos rot delta) rot))
             (R (values pos (rotate rot delta)))
             (L (values pos (rotate rot (- 360 delta)))))))
    (play-game (cons 0 0) (cons 1 0) #'game-func)))

(defun part-2 ()
  (flet ((game-func (pos rot pre delta)
           (ecase pre
             ((N S E W) (values pos (move rot (cdr (assoc pre +directions+)) delta)))
             (F (values (move pos rot delta) rot))
             (R (values pos (rotate rot delta)))
             (L (values pos (rotate rot (- 360 delta)))))))
    (play-game (cons 0 0) (cons 10 1) #'game-func)))
