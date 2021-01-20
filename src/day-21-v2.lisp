(defpackage :day-21-v2
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups :split)
  (:import-from :alexandria :curry :rcurry :shuffle :hash-table-alist :ensure-gethash
                :hash-table-values :alist-hash-table)
  (:export #:part-1 #:part-2))

(in-package :day-21-v2)

(defstruct (allergen-group (:conc-name nil)) ingredients allergens)

(defun process-line (line)
  (do-register-groups (ingredients allergens) ("^(.+) \\(contains (.+)\\)$" line)
    (return (make-allergen-group :ingredients (mapcar #'intern (split " " ingredients))
                                 :allergens (mapcar #'intern (split ", " allergens))))))
(defun process-file ()
  (mapcar #'process-line (read-day-file "21")))

(defun initial-possible (groups)
  (loop with table = (make-hash-table :test 'eq)
        for group in groups
        do (loop for allergen in (allergens group)
                 do (multiple-value-bind (so-far present) (gethash allergen table)
                      (if present
                          (setf (gethash allergen table) (intersection (ingredients group) so-far))
                          (setf (gethash allergen table) (ingredients group)))))
        finally (return table)))

(defun finalize-possible (table)
  (let ((alist (hash-table-alist table)))
    (labels ((length-1 (cell) (= 1 (length (cdr cell))))
             (find-singles () (remove-if-not #'length-1 alist))
             (find-multiples () (remove-if #'length-1 alist)))
      (loop with singles = (find-singles)
            with multiples = (find-multiples)
            while (/= (hash-table-count table) (length singles))
            do (loop for (allergen . (ingredient)) in singles
                     do (loop for cell in multiples
                              do (setf (cdr cell) (remove ingredient (cdr cell))))
                     finally (setf singles (find-singles)
                                   multiples (find-multiples)))
            finally (return (alist-hash-table alist))))))

(defun groups-possible-impossible ()
  (let* ((groups (process-file))
         (possible (finalize-possible (initial-possible groups)))
         (all-ingredients (reduce #'union (mapcar #'ingredients groups)))
         (impossible (set-difference all-ingredients (reduce #'append (hash-table-values possible)))))
    (values groups possible impossible)))
  
(defun part-1 ()
  (multiple-value-bind (groups possible impossible) (groups-possible-impossible)
    (labels ((count-in-group (ingredient group) (count ingredient (ingredients group))))
      (loop for ingredient in impossible
            summing (reduce #'+ (mapcar (curry #'count-in-group ingredient) groups))))))

(defun part-2 ()
  (multiple-value-bind (groups possible impossible) (groups-possible-impossible)
    (let* ((a-list (hash-table-alist possible))
           (str-alist (mapcar #'(lambda (cell) (cons (symbol-name (car cell)) (symbol-name (first (cdr cell))))) a-list))
           (sorted (sort str-alist #'string< :key #'car)))
      (format nil "~{~A~^,~}" (mapcar #'cdr sorted)))))
