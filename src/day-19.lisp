(defpackage :day-19
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :split :create-scanner :scan :do-register-groups :regex-replace-all)
  (:import-from :alexandria :rcurry :curry)
  (:export #:part-1 #:part-2))

(in-package :day-19)

(defparameter *is-dense* t)
(defparameter *special-chars* (list "|" "+" "(?-1)"))
(defparameter *version* 1)

(defun begin-group (ruleset)
  (if (and (= 2 *version*)
           (= 11 (index ruleset)))
      "("
      "(?:"))

(defstruct (ruleset (:conc-name nil))
  index
  spec
  (terminal-p nil)
  expanded-rules)

(defun parse-ruleset (line)
  (cl-ppcre:do-register-groups ((#'parse-integer index) spec) ("^([0-9]+): (.*)$" line)
    (return (make-ruleset :index index :spec spec))))
                  
(defun load-rulesets (day)
  (loop with rulesets = (make-array 128 :adjustable t :fill-pointer 0)
        with patterns = nil
        with do-rulesets = t
        for line in (read-day-file day)
        do (progn
             (when (not do-rulesets)
               (push line patterns))
             
             (if (= 0 (length line))
                 (setf do-rulesets nil))
             
             (when do-rulesets
               (vector-push-extend (parse-ruleset line) rulesets)))
        finally (return (values rulesets patterns))))

(defun initialize-endpoints (rulesets)
  (loop for ruleset across rulesets
        do (when (position #\" (spec ruleset) :test #'char=)
             (setf (terminal-p ruleset) t)
             (setf (expanded-rules ruleset) (regex-replace-all "\"" (spec ruleset) "")))))

(defun for-index (rulesets index)
  (if *is-dense*
      (aref rulesets index)
      (loop for ruleset across rulesets
            do (if (= index (index ruleset))
                   (return ruleset)))))

(defun call-next (ruleset rulesets)
  (if (terminal-p ruleset)
      (expanded-rules ruleset)
      (apply #'concatenate
             (append (list 'string (begin-group ruleset))
                     (mapcar (rcurry #'resolve rulesets) (split "\\s" (spec ruleset)))
                     (list ")")))))

(defun resolve (str rulesets) 
  (if (member str *special-chars* :test #'string=)
      str
      (let* ((next-index (parse-integer str))
             (next (for-index rulesets next-index)))
        (call-next next rulesets))))
        
(defun initialize-non-terminals (rulesets)
  (loop for ruleset across rulesets
        do (when (not (terminal-p ruleset))
             (let ((resolved (call-next ruleset rulesets)))
               (setf (expanded-rules ruleset) resolved)))))

(defun initialize-rulesets (rulesets)
  (let ((sorted (sort rulesets #'< :key #'index)))
    (initialize-endpoints rulesets)
    (initialize-non-terminals rulesets)
    sorted))

(defun part-1 ()
  (let ((*is-dense* t)
        (*version* 1))
    (multiple-value-bind (rulesets patterns) (load-rulesets "19-1")
      (let* ((initialized (initialize-rulesets rulesets))
             (ruleset-0 (for-index initialized 0))
             (scanner (create-scanner (concatenate 'string "^" (expanded-rules ruleset-0) "$"))))
        (count-if (lambda (str)
                    (scan scanner str)) patterns)))))

;; I knew I needed a regex engine capable of recursion. However, cl-ppcre is
;; not one of them. I tried all kinds of hacks to balance the expressions, but
;; always came up short. I finally caved and shelled out to perl. I thought
;; this would be a pain, but uiop makes it drop dead simple; probably the simplest
;; process call from any language I have done. Anyhow, I stole the perl invocation
;; syntax from: https://github.com/FractalBoy/advent-of-code-2020/blob/main/day19_part2.py
(defun part-2 ()
  (let ((*is-dense* t)
        (*version* 2))
    (multiple-value-bind (rulesets patterns) (load-rulesets "19-2")
      (let* ((initialized (initialize-rulesets rulesets))
             (ruleset-0 (for-index initialized 0))
             (regex (concatenate 'string "^" (expanded-rules ruleset-0) "$")))
        (loop with total = 0
              for pattern in patterns
              do (let ((perl-answer
                         (uiop:run-program (concatenate 'string "perl -e 'print \"" pattern "\" =~ /" regex "/ ? 1 : 0'")
                                           :output '(:string :stripped t))))
                   (incf total (parse-integer perl-answer)))
              finally (return total))))))
