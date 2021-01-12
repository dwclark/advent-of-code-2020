(defpackage :day-22
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-22)

(defun load-decks ()
  (let ((decks-lines (split-blank-lines (read-day-file "22"))))
    (mapcar #'(lambda (lines)
                (mapcar #'parse-integer (rest lines))) decks-lines)))

(defun play-game-1 (arg-1 arg-2)
  (let ((deck-1 arg-1)
        (deck-2 arg-2))
    (loop while (and (not (null deck-1)) (not (null deck-2)))
          do (let ((card-1 (first deck-1))
                   (card-2 (first deck-2)))
               (if (< card-2 card-1)
                   (progn
                     (setf deck-1 (append (rest deck-1) (list card-1 card-2)))
                     (setf deck-2 (rest deck-2)))
                   (progn
                     (setf deck-2 (append (rest deck-2) (list card-2 card-1)))
                     (setf deck-1 (rest deck-1)))))
          finally (return (if deck-1 deck-1 deck-2)))))

(defun winning-score (lst)
  (loop with counter = 0
        for card in (reverse lst)
        summing (* (incf counter) card) into score
        finally (return score)))

(defstruct hand
  player-1
  player-2)

(defun deck->vector (deck)
  (make-array (length deck) :element-type 'integer :initial-contents deck))

(defparameter *bailout-engaged* nil)

(defun bailout-rule (prev-hands deck-1 deck-2)
  (let ((vec-1 (deck->vector deck-1))
        (vec-2 (deck->vector deck-2)))
    
    (setf *bailout-engaged* (or (find vec-1 prev-hands :key #'hand-player-1 :test #'equalp)
                                (find vec-2 prev-hands :key #'hand-player-2 :test #'equalp))))
  *bailout-engaged*)

(defun recurse-p (card-1 deck-1 card-2 deck-2)
  (and (<= card-1 (length deck-1))
       (<= card-2 (length deck-2))))

(defun determine-winner (deck-1 deck-2)
  (if (or *bailout-engaged* (null deck-2))
      (values :player-1 deck-1)
      (values :player-2 deck-2)))

(defun copy-n (n deck)
  (loop for i from 0 below n
        for c in deck
        collecting c into new-deck
        finally (return new-deck)))

(defun play-game-2 (arg-prev-hands arg-1 arg-2)
  (let ((prev-hands arg-prev-hands)
        (deck-1 arg-1)
        (deck-2 arg-2))
    (loop while (and (not (bailout-rule prev-hands deck-1 deck-2))
                     (not *bailout-engaged*)
                     (not (null deck-1))
                     (not (null deck-2)))
          do (let ((card-1 (first deck-1))
                   (card-2 (first deck-2))
                   (hand-winner nil))
               
               (push (make-hand :player-1 (deck->vector deck-1)
                                :player-2 (deck->vector deck-2)) prev-hands)
               
               (if (recurse-p card-1 (rest deck-1) card-2 (rest deck-2))
                   (setf hand-winner (play-game-2 nil (copy-n card-1 (rest deck-1)) (copy-n card-2(rest deck-2))))
                   (setf hand-winner (if (< card-2 card-1) :player-1 :player-2)))
               
               (if (eq hand-winner :player-1)
                   (progn
                     (setf deck-1 (append (rest deck-1) (list card-1 card-2)))
                     (setf deck-2 (rest deck-2)))
                   (progn
                     (setf deck-2 (append (rest deck-2) (list card-2 card-1)))
                     (setf deck-1 (rest deck-1))))))
    (determine-winner deck-1 deck-2)))

    
(defun part-1 ()
  (destructuring-bind (deck-1 deck-2) (load-decks)
    (let ((winner (play-game-1 deck-1 deck-2)))
      (winning-score winner))))

(defun part-2 ()
  (destructuring-bind (deck-1 deck-2) (load-decks)
    (multiple-value-bind (winner winning-deck) (play-game-2 nil deck-1 deck-2)
      (format t "~&bailout engaged ~A" *bailout-engaged*)
      (format t "~&winner ~A deck ~A" winner winning-deck)
      (winning-score winning-deck))))
