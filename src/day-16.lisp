(defpackage :day-16
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :rcurry :curry)
  (:export #:part-1 #:part-2))

(in-package :day-16)

(defgeneric in-range (r val))

(defclass range ()
  ((low :initarg :low :reader low)
   (high :initarg :high :reader high)))

(defmethod in-range ((r range) val)
  (with-slots (low high) r
    (and (<= low val) (<= val high))))

(defclass rule ()
  ((name :initarg :name :reader name)
   (ranges :initarg :ranges :reader ranges)))

(defmethod in-range ((r rule) val)
  (with-slots (ranges) r
    (or (in-range (car ranges) val)
        (in-range (cdr ranges) val))))

(defun ticket-value-matches-some-rule-p (rules ticket-value)
  (some (rcurry #'in-range ticket-value) rules))

(defun ticket-values-not-matching-rules (rules ticket-values)
  (remove-if (curry #'ticket-value-matches-some-rule-p rules) ticket-values))

(defun scanning-error-rate (rules list-ticket-values)
  (reduce #'+
          (reduce #'append
                  (mapcar (curry #'ticket-values-not-matching-rules rules)
                          list-ticket-values))))

(defun valid-tickets (rules list-ticket-values)
  (remove-if #'(lambda (ticket-values)
                 (< 0 (length (ticket-values-not-matching-rules rules ticket-values))))
             list-ticket-values))

(defun single-val-matching-rule-names (rules val)
  (mapcar #'name (remove-if-not (rcurry #'in-range val) rules)))

(defun matching-rule-names (rules vals)
  (reduce #'(lambda (list-1 list-2)
              (intersection list-1 list-2 :test #'string=))
          (mapcar (curry #'single-val-matching-rule-names rules) vals)))

(defun extract-col-as-list (list-vecs col)
  (mapcar (rcurry #'aref col) list-vecs))

(defun extract-rules-for-cols (rules tickets)
  (loop with list-vecs = (mapcar (curry #'concatenate 'vector) tickets)
        for col from 0 below (length (first list-vecs))
        collecting (cons col (matching-rule-names rules (extract-col-as-list list-vecs col))) into all-matching
        finally (return all-matching)))

(defun all-with-single-matches (alist-possible)
  (remove-if-not #'(lambda (cell)
                     (= 1 (length (cdr cell)))) alist-possible))

(defun all-with-multiple-matches (alist-possible)
  (remove-if #'(lambda (cell)
                     (= 1 (length (cdr cell)))) alist-possible))

(defun remove-not-possible (val alist-multi-matches)
  (loop for cell in alist-multi-matches
        do (setf (cdr cell) (remove-if (curry #'string= val) (cdr cell)))))

(defun finalize-col-assignments (alist-possible)
  (loop with single-matches = (all-with-single-matches alist-possible)
        with multi-matches = (all-with-multiple-matches alist-possible)
        while (< 0 (length multi-matches))
        do (progn
             (loop for single-match in single-matches
                   do (remove-not-possible (first (cdr single-match)) multi-matches))
             (setf single-matches (all-with-single-matches alist-possible))
             (setf multi-matches (all-with-multiple-matches alist-possible))))
  alist-possible)

(defun load-data ()
  (declare (optimize debug))
  (loop with rules = nil
        with my = nil
        with nearby = nil
        with my-flag = nil
        with nearby-flag = nil
        for line in (read-day-file "16")
        do (progn
             (cl-ppcre:do-register-groups
                 (rule-name (#'parse-integer low1 high1 low2 high2))
                 ("([a-z\\s]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" line)
               (setf rules (cons (make-instance 'rule :name rule-name
                                                      :ranges (cons (make-instance 'range :low low1 :high high1)
                                                                    (make-instance 'range :low low2 :high high2)))
                                 rules)))
             (cl-ppcre:do-register-groups (dummy) ("(^[0-9]{1})" line)
               (let ((vals (cl-ppcre:split "," line)))
                 (if nearby-flag
                     (setf nearby (cons (mapcar #'parse-integer vals) nearby))
                     (setf my (mapcar #'parse-integer vals)))))

             (cl-ppcre:do-register-groups (your-ticket) ("(your ticket:)" line)
               (setf my-flag t))

             (cl-ppcre:do-register-groups (nearby-ticket) ("(nearby tickets:)" line)
               (setf nearby-flag t)))
           
        finally (return (values (nreverse rules) my (nreverse nearby)))))

(defun part-1 ()
  (multiple-value-bind (rules mine nearby) (load-data)
    (scanning-error-rate rules nearby)))

(defun part-2 ()
  (multiple-value-bind (rules mine nearby) (load-data)
    (let* ((col-assignments
             (finalize-col-assignments (extract-rules-for-cols rules (valid-tickets rules nearby))))
           (departure-fields (remove-if-not #'(lambda (cell)
                                                (let ((pos (search "departure" (first (cdr cell)) :test #'char=)))
                                                  (and pos (= 0 pos))))
                                            col-assignments))
           (indexes (mapcar #'car departure-fields)))
      (reduce #'* (mapcar #'(lambda (idx)
                              (nth idx mine)) indexes)))))
