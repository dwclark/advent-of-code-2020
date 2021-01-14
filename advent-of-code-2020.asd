;;;; Advent of Code 2020

(asdf:defsystem #:advent-of-code-2020
  :description "Advent of Code 2020"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("alexandria" "cl-ppcre" "fare-memoization" "array-operations" "infix-math")
  :components ((:file "src/utils")
               (:file "src/day-1" :depends-on ("src/utils"))
               (:file "src/day-1-v2" :depends-on ("src/utils"))
               (:file "src/day-2" :depends-on ("src/utils"))
               (:file "src/day-2-v2" :depends-on ("src/utils"))
               (:file "src/day-3" :depends-on ("src/utils"))
               (:file "src/day-3-v2" :depends-on ("src/utils"))
               (:file "src/day-4" :depends-on ("src/utils"))
               (:file "src/day-4-v2" :depends-on ("src/utils"))
               (:file "src/day-5" :depends-on ("src/utils"))
               (:file "src/day-5-v2" :depends-on ("src/utils"))
               (:file "src/day-6" :depends-on ("src/utils"))
               (:file "src/day-6-v2" :depends-on ("src/utils"))
               (:file "src/day-7" :depends-on ("src/utils"))
               (:file "src/day-7-v2" :depends-on ("src/utils"))
               (:file "src/day-8" :depends-on ("src/utils"))
               (:file "src/day-8-v2" :depends-on ("src/utils"))
               (:file "src/day-9" :depends-on ("src/utils"))
               (:file "src/day-9-v2" :depends-on ("src/utils"))
               (:file "src/day-10" :depends-on ("src/utils"))
               (:file "src/day-10-v2" :depends-on ("src/utils"))
               (:file "src/day-11" :depends-on ("src/utils"))
               (:file "src/day-11-v2" :depends-on ("src/utils"))
               (:file "src/day-12" :depends-on ("src/utils"))
               (:file "src/day-12-v2" :depends-on ("src/utils"))
               (:file "src/day-13" :depends-on ("src/utils"))
               (:file "src/day-13-v2" :depends-on ("src/utils"))
               (:file "src/day-14" :depends-on ("src/utils"))
               (:file "src/day-14-v2" :depends-on ("src/utils"))
               (:file "src/day-15" :depends-on ("src/utils"))
               (:file "src/day-16" :depends-on ("src/utils"))
               (:file "src/day-17" :depends-on ("src/utils"))
               (:file "src/day-18" :depends-on ("src/utils"))
               (:file "src/day-18-v2" :depends-on ("src/utils"))
               (:file "src/day-19" :depends-on ("src/utils"))
               (:file "src/day-20" :depends-on ("src/utils"))
               (:file "src/day-21" :depends-on ("src/utils"))
               (:file "src/day-22" :depends-on ("src/utils"))
               (:file "src/day-23" :depends-on ("src/utils"))
               (:file "src/day-24" :depends-on ("src/utils"))
               (:file "src/day-25" :depends-on ("src/utils"))))

(asdf:defsystem #:advent-of-code-2020-tests
  :description "Advent of Code 2020 Tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t

  :depends-on ("advent-of-code-2020" "fiveam")

  :components ((:file "test/test-23")))

