(defpackage :day-4
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :rcurry)
  (:import-from :cl-ppcre :register-groups-bind :all-matches :split)
  (:export #:part-1 #:part-2))

(in-package :day-4)

(defparameter *mandatory-keys* (list "byr:" "iyr:" "eyr:" "hgt:" "hcl:" "ecl:" "pid:"))
(defparameter *optional-keys* (list "cid:"))

#|
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
|#

(defun in-range-year (str low high)
  (register-groups-bind ((#'parse-integer year)) ("^([0-9]{4})$" str)
    (and (<= low year) (<= year high))))

(defun byr (str)
  (in-range-year str 1920 2002)) 

(defun iyr (str)
  (in-range-year str 2010 2020))

(defun eyr (str)
  (in-range-year str 2020 2030))

(defun hgt (str)
  (or (register-groups-bind ((#'parse-integer num) units) ("^([0-9]{3})(cm)$" str)
        (and (<= 150 num) (<= num 193)))
      
      (register-groups-bind ((#'parse-integer num) units) ("^([0-9]{2})(in)$" str)
        (and (<= 59 num) (<= num 76)))))

(defun hcl (str)
  (all-matches "^#[0-9a-f]{6}$" str))

(defun ecl (str)
  (all-matches "amb|blu|brn|gry|grn|hzl|oth" str))

(defun pid (str)
  (all-matches "^[0-9]{9}$" str))

(defparameter *call-table*
  (list (cons "byr" #'byr)
        (cons "iyr" #'iyr)
        (cons "eyr" #'eyr)
        (cons "hgt" #'hgt)
        (cons "hcl" #'hcl)
        (cons "ecl" #'ecl)
        (cons "pid" #'pid)))

(defun check-key-value (str)
  (let* ((key (subseq str 0 3))
         (value (subseq str 4))
         (cell (assoc key *call-table* :test #'string=)))
    (if cell
        (funcall (cdr cell) value)
        nil)))

(defun load-passports ()
  (let ((lines (read-day-file "4"))
        (accum nil)
        (passport nil))
    (dolist (tmp lines)
      (if (= 0 (length tmp))
          (let ((formatted (format nil "~{~A~^ ~}" passport)))
            (setf accum (append accum (list formatted)))
            (setf passport nil))
          (progn
            (setf passport (append passport (list tmp))))))
                                   
    (setf accum (append accum (list (format nil "~{~A~^ ~}" passport))))))

(defun tokenize (line)
  (split " " line))

(defun all-present (tokens)
  (let ((sub (rcurry #'subseq 0 4)))
    (loop for token in tokens
          count (member-if #'(lambda (s) (string= (subseq token 0 4) s)) *mandatory-keys*) into total
          finally (return (= total (length *mandatory-keys*))))))
    
(defun part-1 ()
  (let ((passports (load-passports)))
    (count-if #'(lambda (p) (all-present (split " " p))) passports)))

(defun part-2 ()
  (let ((passports (load-passports)))
    (count-if #'(lambda (p)
                  (= 7 (count-if #'check-key-value (split " " p))))
              passports)))
