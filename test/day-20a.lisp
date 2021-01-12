(defpackage :day-20a-test
  (:use :cl :suite-tests :day-20a))

(in-package :day-20a-test)

(defun linear-elements (side)
  (let ((lst nil))
    (dotimes (n side)
      (setf lst (nconc lst (list n))))
    lst))

(defun fresh-tile (side)
  (let ((size (* side side)))
    (make-tile :side-size side
               :initial-state (make-array size
                                          :element-type 'fixnum
                                          :initial-contents (linear-elements size))
               :current-state (make-array size
                                          :element-type 'fixnum
                                          :initial-contents (linear-elements size)))))

(defun basic-game-tiles ()
  (list (tile-from-lists :tile-id 1 :lists '((1 1) (1 1)))
                        (tile-from-lists :tile-id 2 :lists '((1 1) (1 1)))
                        (tile-from-lists :tile-id 3 :lists '((1 1) (1 1)))
                        (tile-from-lists :tile-id 4 :lists '((1 1) (1 1)))))

(defsuite day-20a-tests ()

  (deftest test-get-at
    (let ((ary (fresh-tile 3)))
      (is (= 2 (get-at ary 0 2)))
      (is (= 0 (get-at ary 0 0)))
      (is (= 7 (get-at ary 2 1)))))

  (deftest test-put-at
    (let ((ti (fresh-tile 3)))
      (put-at ti 0 0 100)
      (put-at ti 1 1 29)
      (is (= 100 (get-at ti 0 0)))
      (is (= 29 (get-at ti 1 1)))))

  (deftest test-reset-state
    (let ((ti (fresh-tile 2)))
      (put-at ti 1 1 100)
      (is (not (equalp (current-state ti) (initial-state ti))))
      (reset-state ti)
      (is (equalp (current-state ti) (initial-state ti)))))

  (deftest test-rotate
    (let ((2-s (fresh-tile 2))
          (3-s (fresh-tile 3))
          (4-s (fresh-tile 4)))
      (rotate 2-s)
      (is (equalp (current-state 2-s) (make-array 4 :initial-contents '(1 3 0 2))))
      (rotate 3-s)
      (is (equalp (current-state 3-s) (make-array 9 :initial-contents '(2 5 8 1 4 7 0 3 6))))
      (rotate 4-s)
      (is (equalp (current-state 4-s) (make-array 16 :initial-contents '(3 7 11 15 2 6 10 14 1 5 9 13 0 4 8 12))))))
          
  (deftest test-flip-vertical
    (let ((2-s (fresh-tile 2))
          (3-s (fresh-tile 3)))

      (flip-vertical 2-s)
      (flip-vertical 3-s)
      (is (equalp (current-state 2-s) (make-array 4 :initial-contents '(2 3 0 1))))
      (is (equalp (current-state 3-s) (make-array 9 :initial-contents '(6 7 8 3 4 5 0 1 2))))))

  (deftest test-flip-horizontal
    (let ((2-s (fresh-tile 2))
          (3-s (fresh-tile 3)))
      (flip-horizontal 2-s)
      (flip-horizontal 3-s)
      (is (equalp (current-state 2-s) (make-array 4 :initial-contents '(1 0 3 2))))
      (is (equalp (current-state 3-s) (make-array 9 :initial-contents '(2 1 0 5 4 3 8 7 6))))))

  (deftest test-vertical-fit-p
    (let ((top (tile-from-lists :tile-id 1 :lists '((1 2 3) (4 5 6) (7 8 9))))
          (bottom (tile-from-lists :tile-id 2 :lists '((7 8 9) (1 2 3) (4 5 6)))))
      (is (vertical-fit-p top bottom))
      (is (not (vertical-fit-p bottom top)))))

  (deftest test-horizontal-fit-p
    (let ((left (tile-from-lists :tile-id 1 :lists '((1 2 3) (4 5 6) (7 8 9))))
          (right (tile-from-lists :tile-id 2 :lists '((3 1 2) (6 4 5) (9 7 8)))))
      (is (horizontal-fit-p left right))
      (is (not (horizontal-fit-p right left)))))

  (deftest test-game-from-tiles ()
    (let* ((tiles (basic-game-tiles))
           (the-game (game-from-tiles tiles)))
      (is (= 2 (grid-size the-game)))
      (is (= 1 (max-dimension the-game)))))

  (deftest test-next-row-col ()
    (let* ((tiles (basic-game-tiles))
           (the-game (game-from-tiles tiles)))
      (is (= 1 (next-col the-game 0)))
      (is (= 0 (next-col the-game 1)))
      (is (= 0 (next-row the-game 0 0)))
      (is (= 1 (next-row the-game 0 1)))))

  (deftest test-put-tile-at ()
    (let* ((tiles (basic-game-tiles))
           (the-game (game-from-tiles tiles)))
      (is (put-tile-at the-game (first tiles) 0 0))
      (is (put-tile-at the-game (second tiles) 0 1))
      (is (put-tile-at the-game (third tiles) 1 0))
      (is (put-tile-at the-game (fourth tiles) 1 1))
      (is (not (put-tile-at the-game (tile-from-lists :tile-id 17 :lists '((1 2) (2 1))) 0 1)))
      (is (not (put-tile-at the-game (tile-from-lists :tile-id 17 :lists '((1 2) (2 1))) 1 0)))
      (is (not (put-tile-at the-game (tile-from-lists :tile-id 17 :lists '((1 2) (2 1))) 1 1)))))

  (deftest test-put-tiles ()
    (let* ((tiles (basic-game-tiles))
           (the-game (game-from-tiles tiles)))
      (is (put-tiles the-game tiles 0 0))))

  (deftest test-remove-this-tile ()
    (let* ((tiles (basic-game-tiles))
           (removed-first (remove-tile (first tiles) tiles)))
      (is (= 3 (length removed-first)))))
  )
