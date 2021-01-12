(defpackage :day-7
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2 #:parse-bag #:load-bag-rules))

(in-package :day-7)

(defun parse-bag-color (str)
  (cl-ppcre:do-register-groups (color) ("(\\w{1,} \\w{1,}) bags" str)
    (return color)))

(defun parse-bag-contents (str)
  (let (accum)
    (cl-ppcre:do-register-groups ((#'parse-integer num) color) ("([0-9]{1,}) (\\w{1,} \\w{1,}) bag" str)
      (push (list color num) accum))
    (nreverse accum)))

(defun parse-bag (line)
  (let ((split (cl-ppcre:split "contain" line)))
    (list (parse-bag-color (first split))
          (parse-bag-contents (second split)))))

(defun bag-color (bag) (first bag))
(defun bag-contents (bag) (second bag))
(defun bag-count (bag) (second bag))

(defun load-bag-rules ()
  (mapcar #'parse-bag (read-day-file "7")))

(defun can-contain (the-color all)
  (let ((found (make-hash-table :test 'equal)))
    (labels ((analyze ()
               (let ((start-count (hash-table-count found)))
                 (loop for bag in all
                       do (let ((test-color (bag-color bag)))
                            (loop for content in (bag-contents bag)
                                  do (let ((content-color (bag-color content)))
                                       (if (or (string= the-color content-color)
                                               (gethash content-color found))
                                           (setf (gethash test-color found) 1))))))
                 (if (= start-count (hash-table-count found))
                     found
                     (analyze)))))
      (analyze))
    (hash-table-count found)))

(defun total-contained (the-color all)
  (let ((bag (find the-color all :key #'bag-color :test #'equal)))
    (if (= 0 (length (bag-contents bag)))
        0
        (loop for contents in (bag-contents bag)
              summing (+ (bag-count contents) (* (bag-count contents) (total-contained (bag-color contents) all))) into total
              finally (return total)))))
                     
(defun part-1 ()
  (can-contain "shiny gold" (load-bag-rules)))

(defun part-2 ()
  (total-contained "shiny gold" (load-bag-rules)))
