(defpackage :day-23-test
  (:use :cl :fiveam :day-23))

(in-package :day-23-test)

(def-suite day-23-tests :description "Day 23 Tests")
(in-suite day-23-tests)

(test test-initialise-game
  (let ((game (small-game (list 3 6 2 9 8 1 7 5 4))))
    (is (= 1 (min-val game)))
    (is (= 9 (max-val game)))
    (is (= 10 (length (contents game))))
    (is (= 3 (current game)))))

(test test-pick-up-cups
  (let ((game (small-game (list 3 6 2 9 8 1 7 5 4))))
    (pick-up-cups game)
    (is (equalp (make-array 3 :initial-contents '(6 2 9)) (picked-up game)))
    (is (in-picked-up game 6))
    (is (in-picked-up game 9))
    (is (not (in-picked-up game 1)))))

(test test-next-cup
  (let ((game (small-game (list 3 6 2 9 8 1 7 5 4))))
    (is (= 1 (next-cup game 2)))
    (is (= 9 (next-cup game 1)))))

(test test-dest-cup
  (let ((game (small-game (list 3 6 2 9 8 1 7 5 4))))
    (is (= 2 (dest-cup game)))
    (pick-up-cups game)
    (is (= 1 (dest-cup game)))))

(test test-set-new-current
  (let ((game (small-game (list 3 6 2 9 8 1 7 5 4))))
    (set-new-current game)
    (is (= 6 (current game)))))

(test test-game-moves
  (let ((game1 (small-game (list 3 8 9 1 2 5 4 6 7)))
        (game2 (small-game (list 3 6 2 9 8 1 7 5 4))))
    (dotimes (n 100)
      (game-move game1)
      (game-move game2))
    
    (is (string= "67384529" (game-label game1)))
    (is (string= "24798635" (game-label game2)))))

(test test-large-number-moves
  (let ((game (large-game (list 3 8 9 1 2 5 4 6 7) 1000000)))
    (dotimes (n 10000000)
      (game-move game))
    (is (= 149245887792 (score-part-2 game)))))
