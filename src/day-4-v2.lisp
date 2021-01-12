(defpackage :day-4-v2
  (:use :cl)
  (:import-from :utils :read-day-file :split-blank-lines)
  (:import-from :alexandria :rcurry :curry)
  (:import-from :cl-ppcre :register-groups-bind :all-matches :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-4-v2)

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
(defparameter *call-table*
  (list (cons 'byr #'(lambda (s) (<= 1920 (parse-integer s) 2002)))
        (cons 'iyr #'(lambda (s) (<= 2010 (parse-integer s) 2020)))
        (cons 'eyr #'(lambda (s) (<= 2020 (parse-integer s) 2030)))
        (cons 'hgt #'(lambda (s)
                       (register-groups-bind ((#'parse-integer num) units) ("^([0-9]{2,3})(cm|in)$" s)
                         (if (string= "cm" units) (<= 150 num 193) (<= 59 num 76)))))
        (cons 'hcl #'(lambda (s) (all-matches "^#[0-9a-f]{6}$" s)))
        (cons 'ecl #'(lambda (s) (all-matches "amb|blu|brn|gry|grn|hzl|oth" s)))
        (cons 'pid #'(lambda (s) (all-matches "^[0-9]{9}$" s)))))

(defparameter *mandatory-keys* (mapcar #'car *call-table*))

(defun parse-passport (lst-strings)
  (let (ret)
    (do-register-groups (key val) ("([a-z]+):([^\\s]+)" (format nil "~{~A~^ ~}" lst-strings))
      (push (cons (intern (string-upcase key) :day-4-v2) val) ret))
    ret))
    
(defun load-passports ()
  (mapcar #'parse-passport (split-blank-lines (read-day-file "4"))))

(defun part-1 ()
  (labels ((all-present (p) (= (length *mandatory-keys*)
                               (count-if (rcurry #'assoc p) *mandatory-keys*))))
    (count-if #'all-present (load-passports))))

(defun part-2 ()
  (labels ((kv-matches (kv)
             (destructuring-bind (key . val) kv
               (and (assoc key *call-table*)
                    (funcall (cdr (assoc key *call-table*)) val))))
           (matches-all (p)
             (= (length *mandatory-keys*) (count-if #'kv-matches p))))
    
    (count-if #'matches-all (load-passports))))
