(defpackage :day-24-v3
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :all-matches-as-strings)
  (:import-from :alexandria :copy-array :copy-hash-table)
  (:export #:part-1 #:part-2))

(in-package :day-24-v3)

;; use hexagonal grid coordinates here:
;; https://www.redblobgames.com/grids/hexagons/

(defparameter *center* (cons 0 0))

(defun walk! (sym tile)
  (ecase sym
    (w (decf (car tile)))
    (nw (decf (cdr tile)))
    (ne (progn (incf (car tile)) (decf (cdr tile))))
    (e (incf (car tile)))
    (se (incf (cdr tile)))
    (sw (progn (incf (cdr tile)) (decf (car tile)))))
  tile)

(defun copy-tile (tile) (cons (car tile) (cdr tile)))

(defun line->walk (str)
  (flet ((to-symbol (s) (intern (string-upcase s) *package*)))
    (mapcar #'to-symbol (all-matches-as-strings "e|w|ne|nw|se|sw" str))))

(defun walk->tile (walk)
  (loop with tile = (copy-tile *center*)
        for w in walk do (walk! w tile) finally (return tile)))

(defun file->walks ()
  (mapcar #'line->walk (read-day-file "24")))

(defun toggle (table tile)
  (let ((color (gethash tile table)))
    (if (null color)
        (setf (gethash tile table) :black)
        (remhash tile table)))
  table)

(defun initial-walk (walks)
  (reduce #'toggle (mapcar #'walk->tile walks) :initial-value (make-hash-table :test 'equal)))

(defun part-1 ()
  (hash-table-count (initial-walk (file->walks))))

(defun number-adjacent (table tile)
  (loop for w in '(w ne e se sw w)
        summing (if (gethash (walk! w tile) table) 1 0) into total fixnum
        finally (progn
                  (walk! 'ne tile)
                  (return total))))

(defun day-flip (tile table new-table)
  ;; case #1, tile is black
  (let ((adj (number-adjacent table tile)))
    (if (or (zerop adj) (< 2 adj))
        (remhash tile new-table)))

  ;; case #2 now investigate neighbors, but only white tiles, be sure to reset tile
  (loop for w in '(w ne e se sw w)
        do (if (and (null (gethash (walk! w tile) table))
                    (= 2 (number-adjacent table tile)))
               (setf (gethash (copy-tile tile) new-table) :black))))
  
(defun day-flips (table)
  (loop with new-table = (copy-hash-table table)
        for tile being the hash-keys in table
        do (day-flip (copy-tile tile) table new-table)
        finally (return new-table)))

(defun part-2 ()
  (let ((table (initial-walk (file->walks))))
    (dotimes (day 100)
      (setf table (day-flips table)))
    (hash-table-count table)))
