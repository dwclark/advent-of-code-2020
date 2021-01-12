1(defpackage :day-20
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines :bit-vector->integer)
  (:import-from :alexandria :copy-array :define-constant)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-20)

(define-constant +tile-size+ 10)
(define-constant +orientations+ 8)
(define-constant +top-offset+ 0)
(define-constant +right-offset+ 1)
(define-constant +bottom-offset+ 2)
(define-constant +left-offset+ 3)

(deftype orientation ()
  `(simple-array bit (,+tile-size+ ,+tile-size+)))

(deftype orientations ()
  `(simple-vector ,+orientations+))

(deftype edges ()
  `(simple-array fixnum (32)))

(deftype grid-side ()
  `(integer 0 2048))

(deftype orientation-index ()
  `(integer 0 ,+orientations+))

(declaim (inline next-row next-col tile-id tile-edges tile-orientation
                 grid-side left-right-fit top-bottom-fit
                 top-number bottom-number left-number right-number
                 orientation-fit-p))

(defstruct tile
  (id 0 :type fixnum)
  (orientations nil :type orientations)
  (edges nil :type edges)
  (orientation 0 :type orientation-index))

(defmacro define-number-accessor (func-name offset-name)
  `(defun ,func-name (ti o-index)
     (declare (tile ti)
              (orientation-index o-index)
              (optimize speed (safety 0)))
     (aref (tile-edges ti) (+ (* 4 o-index) ,offset-name))))

(define-number-accessor top-number +top-offset+)
(define-number-accessor bottom-number +bottom-offset+)
(define-number-accessor left-number +left-offset+)
(define-number-accessor right-number +right-offset+)

(deftype grid-storage ()
  `(simple-array tile (* *)))

(defstruct grid
  (side 0 :type grid-side)
  (elements nil :type grid-storage))

(defun lines->tile-id (lines)
  (do-register-groups ((#'parse-integer tile-id)) ("Tile (\\d+):" (first lines))
    (return tile-id)))

(defun lines->binary-list (lines)
  (labels ((char->bit (c)
             (if (char= #\# c) 1 0))
           (str->list (str)
             (map 'list #'char->bit str)))
    (mapcar #'str->list lines)))

(defun lines->orientation (lines)
  (labels ((char->bit (c)
             (if (char= #\# c) 1 0))
           (str->list (str)
             (map 'list #'char->bit str)))
    (make-array (list +tile-size+ +tile-size+)
                :element-type 'bit :initial-contents (mapcar #'str->list (rest lines)))))

(defun rotate (arg)
  (loop with size = (array-dimension arg 0)
        with orientation = (copy-array arg)
        with tmp = 0
        for x from 0 below size
        do (loop for y from x below (- (- size x) 1)
                 do (progn
                      (setf tmp (aref orientation x y))
                      
                      (setf (aref orientation x y) (aref orientation y (- (- size 1) x)))
                      
                      (setf (aref orientation y (- (- size 1) x))
                            (aref orientation (- (- size 1) x) (- (- size 1) y)))
                      
                      (setf (aref orientation (- (- size 1) x) (- (- size 1) y))
                            (aref orientation (- (- size 1) y) x))
                      
                      (setf (aref orientation (- (- size 1) y) x) tmp)))
        finally (return orientation)))

(defun flip-vertical (arg)
  (loop with size = (array-dimension arg 0)
        with orientation = (copy-array arg)
        for lower from 0 below size
        for higher from (1- size) downto 0
        while (< lower higher)
        do (loop for col from 0 below size
                 do (rotatef (aref orientation lower col) (aref orientation higher col)))
        finally (return orientation)))

(defun orientation->orientations (orientation)
  (let* ((1st orientation)
         (2nd (rotate 1st))
         (3rd (rotate 2nd))
         (4th (rotate 3rd))
         (5th (flip-vertical orientation))
         (6th (rotate 5th))
         (7th (rotate 6th))
         (8th (rotate 7th)))
    (make-array +orientations+ :element-type 'orientation
                               :initial-contents (list 1st 2nd 3rd 4th 5th 6th 7th 8th))))

(defun orientation->top-number (orientation)
  (bit-vector->integer
   (make-array +tile-size+
               :element-type 'bit
               :initial-contents (loop for i from 0 below +tile-size+
                                       collecting (aref orientation 0 i)))))

(defun orientation->bottom-number (orientation)
  (bit-vector->integer
   (make-array +tile-size+
               :element-type 'bit
               :initial-contents (loop for i from 0 below +tile-size+
                                       collecting (aref orientation (1- +tile-size+) i)))))

(defun orientation->left-number (orientation)
  (bit-vector->integer
   (make-array +tile-size+
               :element-type 'bit
               :initial-contents (loop for i from 0 below +tile-size+
                                       collecting (aref orientation i 0)))))

(defun orientation->right-number (orientation)
  (bit-vector->integer
   (make-array +tile-size+
               :element-type 'bit
               :initial-contents (loop for i from 0 below +tile-size+
                                       collecting (aref orientation i (1- +tile-size+))))))
  
(defun orientations->edges (orientations)
  (loop with edges = (make-array 32 :element-type 'fixnum)
        for i from 0 below (length orientations)
        do (let ((orientation (aref orientations i)))
             (setf (aref edges (+ (* i 4) +top-offset+)) (orientation->top-number orientation))
             (setf (aref edges (+ (* i 4) +right-offset+)) (orientation->right-number orientation))
             (setf (aref edges (+ (* i 4) +bottom-offset+)) (orientation->bottom-number orientation))
             (setf (aref edges (+ (* i 4) +left-offset+)) (orientation->left-number orientation)))
        finally (return edges)))

(defun lines->tile (lines)
  (let* ((tile-id (lines->tile-id lines))
         (orientation (lines->orientation lines))
         (orientations (orientation->orientations orientation))
         (edges (orientations->edges orientations)))
    (make-tile :id tile-id :orientations orientations :edges edges)))

(defun file->tiles ()
  (mapcar #'lines->tile (split-blank-lines (read-day-file "20"))))

(defun next-row (g row col)
  (declare (grid g)
           (grid-side row col)
           (optimize speed (safety 0)))
  
  (if (= col (1- (grid-side g)))
      (1+ row)
      row))

(defun next-col (g col)
  (declare (grid g)
           (grid-side col)
           (optimize speed (safety 0)))
  
  (if (= col (1- (grid-side g)))
      0
      (1+ col)))

(defun tile-above (g row col)
  (declare (grid g)
           (grid-side row col)
           (optimize speed (safety 0)))

  (if (zerop row)
      nil
      (aref (grid-elements g) (1- row) col)))

(defun tile-left (g row col)
  (declare (grid g)
           (grid-side row col)
           (optimize speed (safety 0)))

  (if (zerop col)
      nil
      (aref (grid-elements g) row (1- col))))

(defun orientation-fit-p (above left ti i)
  (declare (type (or null tile) above left)
           (type tile ti)
           (orientation-index i)
           (optimize speed))

  (and 
   (or (null left)
       (= (right-number left (tile-orientation left))
          (left-number ti i)))
   (or (null above)
       (= (bottom-number above (tile-orientation above))
          (top-number ti i)))))
  
(defun place-tiles (g tiles row col)
  (if (null tiles)
      (return-from place-tiles t))
  
  (let ((above (tile-above g row col))
        (left (tile-left g row col)))
    
    (loop for tile in tiles
          do (loop for i from 0 below +orientations+
                   do (when (orientation-fit-p above left tile i)
                        (setf (tile-orientation tile) i)
                        (setf (aref (grid-elements g) row col) tile)
                        
                        (if (place-tiles g
                                         (remove-if #'(lambda (arg)
                                                        (= (tile-id tile) (tile-id arg))) tiles)
                                         (next-row g row col)
                                         (next-col g col))
                            (return-from place-tiles t))))
          finally (return nil))))

(defun tiles->grid (tiles)
  (let ((side (floor (sqrt (length tiles)))))
    (make-grid :side side
               :elements (make-array (list side side) :element-type 'tile))))

(defun display-grid (g)
  (loop for col from 0 below (grid-side g)
        do (progn
             (format t "~%")
             (loop for row from 0 below (grid-side g)
                   do (format t "~A " (tile-id (aref (grid-elements g) row col))))
             (format t "~%"))))

(defun find-solution ()
  (let* ((tiles (file->tiles))
         (g (tiles->grid tiles))
         (max-side (1- (grid-side g))))
    (place-tiles g tiles 0 0)
    g))

(defun part-1 ()
  (let* ((g (find-solution))
         (max-side (1- (grid-side g))))
    
    (display-grid g)
    
    (* (tile-id (aref (grid-elements g) 0 0))
       (tile-id (aref (grid-elements g) max-side 0))
       (tile-id (aref (grid-elements g) 0 max-side))
       (tile-id (aref (grid-elements g) max-side max-side)))))

(defun remove-border (ary)
  (loop with new-rows = (- (array-dimension ary 0) 2)
        with new-cols = (- (array-dimension ary 1) 2)
        with ret-ary = (make-array (list new-rows new-cols)
                                   :element-type (array-element-type ary))
        for row from 1 below (1- (array-dimension ary 0))
        do (loop for col from 1 below (1- (array-dimension ary 0))
                 do (setf (aref ret-ary (1- row) (1- col))
                          (aref ary row col)))
        finally (return ret-ary)))

(defun tile->borderless (ti)
  (let ((chosen (aref (tile-orientations ti) (tile-orientation ti))))
    (remove-border chosen)))

(defun solution->borderless (g)
  (loop with elems = (grid-elements g)
        with rows = (array-dimension elems 0)
        with cols = (array-dimension elems 1)
        with new-grid = (make-array (list rows cols))
        for row from 0 below rows
        do (loop for col from 0 below cols
                 do (setf (aref new-grid row col)
                          (tile->borderless (aref elems row col))))
        finally (return new-grid)))

(defun borderless->mega-grid (s)
  (loop with inner = (array-dimension (aref s 0 0) 0)
        with rows = (* inner (array-dimension s 0))
        with cols = (* inner (array-dimension s 1))
        with ret-grid = (make-array (list rows cols) :element-type 'bit)
        for row from 0 below rows
        do (loop for col below cols
                 do (let ((inner-grid (aref s
                                            (floor (/ row inner))
                                            (floor (/ col inner))))
                          (inner-row (mod row inner))
                          (inner-col (mod col inner)))
                      (setf (aref ret-grid row col) (aref inner-grid inner-row inner-col))))
        finally (return ret-grid)))

(defparameter *sea-monster* (list "                  # "
                                  "#    ##    ##    ###"
                                  " #  #  #  #  #  #   "))

(defparameter *sea-monster-dims* (list (length *sea-monster*) (length (first *sea-monster*))))

(defparameter *test-grid-part-2*
  (list ".#.#..#.##...#.##..#####"
        "###....#.#....#..#......"
        "##.##.###.#.#..######..."
        "###.#####...#.#####.#..#"
        "##.#....#.##.####...#.##"
        "...########.#....#####.#"
        "....#..#...##..#.#.###.."
        ".####...#..#.....#......"
        "#..#.##..#..###.#.##...."
        "#.####..#.####.#.#.###.."
        "###.#.#...#.######.#..##"
        "#.####....##..########.#"
        "##..##.#...#...#.#.#.#.."
        "...#..#..#.#.##..###.###"
        ".#.#....#.##.#...###.##."
        "###.#...#..#.##.######.."
        ".#.#.###.##.##.#..#.##.."
        ".####.###.#...###.#..#.#"
        "..#.#..#..#.#.#.####.###"
        "#..####...#.#.#.###.###."
        "#####..#####...###....##"
        "#.##..#..#...#..####...#"
        ".#.###..##..##..####.##."
        "...###...##...#...#..###"))

(defparameter *sea-monster-coordinates*
  (destructuring-bind (sm-rows sm-cols) *sea-monster-dims*
    (loop with ret-list = nil
          for row from 0 below sm-rows
          do (loop for col from 0 below sm-cols
                   do (when (char= #\# (char (nth row *sea-monster*) col))
                        (setf ret-list (append ret-list (list (cons row col))))))
          finally (return ret-list))))

(defun count-ones (ary)
  (loop with total = 0
        for row from 0 below (array-dimension ary 0)
        do (loop for col from 0 below (array-dimension ary 1)
                 summing (aref ary row col) into my-total
                 finally (incf total my-total))
        finally (return total)))

(defun sea-monster-p (ary)
  (loop for coordinate in *sea-monster-coordinates*
        summing (aref ary (car coordinate) (cdr coordinate)) into total
        finally (return (= total (length *sea-monster-coordinates*)))))

(defun fill-possible-dragon (mega-grid ary start-row start-col)
  (loop for row from 0 below (array-dimension ary 0)
        do (loop for col from 0 below (array-dimension ary 1)
                 do (setf (aref ary row col) (aref mega-grid (+ start-row row) (+ start-col col))))))

(defun water-roughness (mega-grid)
  (destructuring-bind (sm-rows sm-cols) *sea-monster-dims*
    (loop with sea-monster-count = 0
          with possible-dragon = (make-array (list sm-rows sm-cols) :element-type 'bit)
          with mega-rows = (array-dimension mega-grid 0)
          with mega-cols = (array-dimension mega-grid 1)
          for row from 0 below (- mega-rows sm-rows)
          do (loop for col from 0 below (- mega-cols sm-cols)
                   do (progn
                        (fill-possible-dragon mega-grid possible-dragon row col)
                        (if (sea-monster-p possible-dragon)
                            (incf sea-monster-count))))
          finally (return sea-monster-count))))

(defun part-2 ()
  (let* ((s (find-solution))
         (b (solution->borderless s))
         (mg (borderless->mega-grid b))
         (total-ones (count-ones mg))

         (r1 (rotate mg))
         (r2 (rotate r1))
         (r3 (rotate r2))
         (flipped (flip-vertical mg))
         (f1 (rotate flipped))
         (f2 (rotate f1))
         (f3 (rotate f2))
         (num-dragons (first (remove-if #'zerop (mapcar #'water-roughness (list mg r1 r2 r3 flipped f1 f2 f3))))))
    (- (count-ones mg) (* (length *sea-monster-coordinates*) num-dragons))))
