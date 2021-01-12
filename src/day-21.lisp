(defpackage :day-21
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups :split)
  (:import-from :alexandria :curry :rcurry :shuffle :hash-table-alist)
  (:export #:part-1 #:part-2))

(in-package :day-21)

(declaim (inline allergen-p ingredient-p possible-state-p))

(defclass allergen-group ()
  ((ingredients :initarg :ingredients :reader ingredients)
   (allergens :initarg :allergens :reader allergens)))

(defmethod print-object ((object allergen-group) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(allergens: ~A, ingredients ~A)" (allergens object) (ingredients object))))

(defun allergen-p (group allergen)
  (if (member allergen (allergens group)) t nil))

(defun ingredient-p (group ingredient)
  (if (member ingredient (ingredients group)) t nil))

(defun remove-combo (group allergen ingredient)
  (if (allergen-p group allergen)
      (make-instance 'allergen-group
                     :allergens (remove allergen (allergens group))
                     :ingredients (remove ingredient (ingredients group)))
      group))

(defun remove-combos (groups allergen ingredient)
  (mapcar (rcurry #'remove-combo allergen ingredient) groups))

(defun possible-state-p (group allergen ingredient)
  (if (allergen-p group allergen)
      (ingredient-p group ingredient)
      t))

(defun possible-states-p (groups allergen ingredient)
  (every (rcurry #'possible-state-p allergen ingredient) groups))

(defun process-line (line)
  (do-register-groups (ingredients allergens) ("^(.+) \\(contains (.+)\\)$" line)
    (return (make-instance 'allergen-group
                           :ingredients (mapcar #'intern (split " " ingredients))
                           :allergens (mapcar #'intern (sort (split ", " allergens) #'string<))))))
(defun process-file ()
  (mapcar #'process-line (read-day-file "21")))

(defun unique-allergens (groups)
  (remove-duplicates (reduce #'append (mapcar #'allergens groups))))

(defun unique-ingredients (groups)
  (remove-duplicates (reduce #'append (mapcar #'ingredients groups))))

(defun accum-possible-allergens (groups allergens ingredients current-chain accum-table)
  ;(format t "~&current allergens ~A, current chain ~A" allergens current-chain)
  (if (null allergens)
      (progn
        (format t "~&found matches")
        (loop for cell in current-chain
              do (progn
                   (format t "~&accumulating ~A ~A" (car cell) (cdr cell))
                   (setf (gethash (car cell) accum-table) (cdr cell))))))
  
  (loop for ingredient in ingredients
        do (when (not (gethash ingredient accum-table))
             (loop for allergen in allergens
                   do (if (possible-states-p groups allergen ingredient)
                          (accum-possible-allergens (remove-combos groups allergen ingredient)
                                                    (remove allergen allergens)
                                                    (remove ingredient ingredients)
                                                    (append current-chain (list (cons ingredient allergen)))
                                                    accum-table)))))
  accum-table)

(defun possible-allergens (groups allergens ingredients)
  (let ((accum-table (make-hash-table)))
    (accum-possible-allergens groups allergens ingredients nil accum-table)
    accum-table))

(defun part-1 ()
  (let* ((groups (process-file))
         (allergens (unique-allergens groups))
         (ingredients (unique-ingredients groups))
         (table (possible-allergens groups allergens ingredients))
         (possible (loop for key being the hash-keys of table collecting key into ret finally (return ret)))
         (impossible (set-difference ingredients possible)))

    (format t "~&possible allergens ~A" possible)
    (format t "~&impossible allergens ~A" (set-difference ingredients possible))
    
    (reduce #'+ (mapcar #'(lambda (ingredient)
                            (reduce #'+ (mapcar (lambda (ingredients)
                                                  (count ingredient ingredients))
                                                (mapcar #'ingredients groups)))) impossible))))
                 
(defun part-2 ()
  (let* ((groups (process-file))
         (allergens (unique-allergens groups))
         (ingredients (unique-ingredients groups))
         (table (possible-allergens groups allergens ingredients))
         (a-list (hash-table-alist table)))
    (format nil "~{~A~^,~}" (mapcar #'car (sort a-list #'string< :key (lambda (c) (symbol-name (cdr c))))))))
