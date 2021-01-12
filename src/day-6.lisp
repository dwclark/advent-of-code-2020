(defpackage :day-6
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:export #:part-1 #:part-2))

(in-package :day-6)

(defun group-count (group)
  (loop with table = (make-hash-table)
        for entry in group
        do (loop for c across entry
                 do (setf (gethash c table) 1))
        finally (return (hash-table-count table))))

(defun group-all-yes-count (group)
  (let ((table (make-hash-table))
        (all-count (length group)))
    (dolist (entry group)
      (loop for c across entry
            do (let ((current (gethash c table 0)))
                 (setf (gethash c table) (1+ current)))))
    (loop for k being the hash-keys in table
          counting (= all-count (gethash k table)) into all-yes
          finally (return all-yes))))

(defun part-1 ()
  (reduce #'+ (mapcar #'group-count (split-blank-lines (read-day-file "6")))))

(defun part-2 ()
  (reduce #'+ (mapcar #'group-all-yes-count (split-blank-lines (read-day-file "6")))))
