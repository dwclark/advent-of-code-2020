(defpackage :day-24-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :all-matches-as-strings)
  (:import-from :alexandria :copy-array)
  (:export #:part-1 #:part-2))

(in-package :day-24-v2)

;; use hexagonal grid coordinates here:
;; https://www.redblobgames.com/grids/hexagons/

(deftype grid-tile-element ()
  `(integer -1024 1024))
(deftype grid-tile ()
  `(simple-array grid-tile-element *))

(defparameter *center* (make-array 3 :element-type 'grid-tile-element :initial-element 0))
(defconstant *x* 0)
(defconstant *y* 1)
(defconstant *z* 2)

(declaim (ftype (function (grid-tile grid-tile) boolean) tile-equal))
(defun tile-equal (t1 t2)
  (declare (optimize speed))
  (and (= (aref t1 0) (aref t2 0))
       (= (aref t1 1) (aref t2 1))
       (= (aref t1 2) (aref t2 2))))

(declaim (ftype (function (grid-tile) fixnum) tile-hash))
(defun tile-hash (tile)
  (declare (optimize speed))
  (let* ((hash 37)
         (v1 (aref tile 0))
         (v1m (if (< v1 0) 31 37))
         (v2 (aref tile 1))
         (v2m (if (< v2 0) 31 37))
         (v3 (aref tile 2))
         (v3m (if (< v3 0) 31 37)))
    (if (= 31 v1m) (setf v1 (- v1)))
    (if (= 31 v2m) (setf v2 (- v2)))
    (if (= 31 v3m) (setf v3 (- v3)))
    (setf hash (+ v1 (* v1m hash))
          hash (+ v2 (* v2m hash))
          hash (+ v3 (* v3m hash)))
    hash))

(declaim (ftype (function (symbol grid-tile) grid-tile) walk!))
(defun walk! (sym tile)
  (declare (optimize speed))
  (flet ((inc-dec (inc dec)
           (incf (aref tile inc))
           (decf (aref tile dec))))
    (ecase sym
      (w (inc-dec *y* *x*))
      (nw (inc-dec *y* *z*))
      (ne (inc-dec *x* *z*))
      (e (inc-dec *x* *y*))
      (se (inc-dec *z* *y*))
      (sw (inc-dec *z* *x*)))
    tile))

(defun fresh-hash-table (&optional (prev nil))
  (let ((ret (make-hash-table :test 'tile-equal :hash-function 'tile-hash)))
    (if prev (maphash #'(lambda (k v) (setf (gethash k ret) v)) prev))
    ret))
      
(defun line->walk (str)
  (flet ((to-symbol (s) (intern (string-upcase s) *package*)))
    (mapcar #'to-symbol (all-matches-as-strings "e|w|ne|nw|se|sw" str))))

(defun walk->tile (walk)
  (loop with tile = (copy-array *center*)
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
  (reduce #'toggle (mapcar #'walk->tile walks) :initial-value (fresh-hash-table)))

(defun part-1 ()
  (hash-table-count (initial-walk (file->walks))))

(defun number-adjacent (table tile)
  (loop for w in '(w ne e se sw w)
        summing (if (gethash (walk! w tile) table) 1 0) into total
        finally (progn
                  (walk! 'ne tile)
                  (return total))))

(defun flips (tile table new-table)
  ;; case #1, tile is black
  (let ((adj (number-adjacent table tile)))
    (if (or (zerop adj) (< 2 adj))
        (remhash tile new-table)))

  ;; case #2 now investigate neighbors, but only white tiles, be sure to reset tile
  (loop for w in '(w ne e se sw w)
        do (if (and (null (gethash (walk! w tile) table))
                    (= 2 (number-adjacent table tile)))
               (setf (gethash (copy-array tile) new-table) :black))))
  
(defun day-flips (table)
  (loop with new-table = (fresh-hash-table table)
        for tile being the hash-keys in table
        do (flips (copy-array tile) table new-table)
        finally (return new-table)))

(defun part-2 ()
  (let ((table (initial-walk (file->walks))))
    (dotimes (day 100)
      (setf table (day-flips table)))
    (hash-table-count table)))
