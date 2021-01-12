(defpackage :day-24
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-24)

;; use hexagonal grid coordinates here:
;; https://www.redblobgames.com/grids/hexagons/

(defparameter *center* (list 0 0 0))
(defun x (g) (car g))
(defun y (g) (cadr g))
(defun z (g) (caddr g))

(defun w (g)
  (list (1- (x g)) (1+ (y g)) (z g)))

(defun nw (g)
  (list (x g) (1+ (y g)) (1- (z g))))

(defun ne (g)
  (list (1+ (x g)) (y g) (1- (z g))))

(defun e (g)
  (list (1+ (x g)) (1- (y g)) (z g)))

(defun se (g)
  (list (x g) (1- (y g)) (1+ (z g))))

(defun sw (g)
  (list (1- (x g)) (y g) (1+ (z g))))

(defun line->walk (str)
  (loop for i from 0 below (length str)
        collecting (let* ((c1 (char str i))
                          (c2 (if (or (char= #\n c1)
                                      (char= #\s c1))
                                  (char str (incf i))
                                  nil)))
                     (cond ((char= #\e c1) #'e)
                           ((char= #\w c1) #'w)
                           ((and (char= #\n c1)
                                 (char= #\e c2)) #'ne)
                           ((and (char= #\n c1)
                                 (char= #\w c2)) #'nw)
                           ((and (char= #\s c1)
                                 (char= #\e c2)) #'se)
                           ((and (char= #\s c1)
                                 (char= #\w c2)) #'sw))) into ret-list
        finally (return ret-list)))

(defun walk->tile (walk)
  (let ((tile *center*))
    (dolist (w walk)
      (setf tile (funcall w tile)))
    tile))

(defun file->walks ()
  (mapcar #'line->walk (read-day-file "24")))

(defun initial-walk ()
  (let ((table (make-hash-table :test 'equal))
        (walks (file->walks))
        (current nil))
    (dolist (tile (mapcar #'walk->tile walks))
      (let ((color (gethash tile table :white)))
        (setf (gethash tile table) (if (eq :white color) :black :white))))

    table))

(defun count-black-tiles (table)
  (loop for color being the hash-values of table
          summing (if (eq color :black) 1 0) into total
        finally (return total)))

(defun part-1 ()
  (let ((table (initial-walk)))
    (count-black-tiles table)))

(defun number-adjacent (table tile)
  (declare (optimize speed))
  
  (loop for w in (list #'w #'e #'ne #'nw #'se #'sw)
        summing (if (eq :black (gethash (funcall w tile) table :white)) 1 0) into total
        finally (return total)))

(defun black-tile-limits (table)
  (let ((z-min 0)
        (z-max 0)
        (y-min 0)
        (y-max 0)
        (x-min 0)
        (x-max 0))
    
    (maphash #'(lambda (key value)
                 (when (eq :black value)
                   (setf z-min (min z-min (z key)))
                   (setf z-max (max z-max (z key)))
                   
                   (setf y-min (min y-min (y key)))
                   (setf y-max (max y-max (y key)))
                   
                   (setf x-min (min x-min (x key)))
                   (setf x-max (max x-max (x key)))))
             table)
    (values x-min x-max y-min y-max z-min z-max)))

(defun flip-? (tile table)
  (let* ((adj (number-adjacent table tile))
         (color (gethash tile table :white)))
    (cond ((and (eq :black color)
                (or (zerop adj)
                    (< 2 adj)))
           :white)

          ((and (eq :white color)
                (= 2 adj))
           :black)

          (t nil))))
        
(defun day-flips (table)
  (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (black-tile-limits table)
    (loop with flip-white = nil
          with flip-black = nil
          for x-val from (1- x-min) to (1+ x-max)
          do (loop for y-val from (1- y-min) to (1+ y-max)
                   do (loop for z-val from (1- z-min) to (1+ z-max)
                            do (let* ((tile (list x-val y-val z-val))
                                      (action (flip-? tile table)))
                                 (cond ((eq :white action)
                                        (setf flip-white (cons tile flip-white)))
                                       ((eq :black action)
                                        (setf flip-black (cons tile flip-black)))))))
          finally (return (values flip-white flip-black)))))

;;part 2 is fairly slow, but it makes progress and eventually gets
;;the right answer. The slow function appears to be number-adjacent,
;;but I don't see a quick way to optimize it.
(defun part-2 ()
  (let ((table (initial-walk)))
    (dotimes (day 100)
      (multiple-value-bind (flip-white flip-black) (day-flips table)
        (dolist (tile flip-white)
          (remhash tile table))
        (dolist (tile flip-black)
          (setf (gethash tile table) :black))
        (format t "~&Day ~A: ~A" (1+ day) (count-black-tiles table))))))
