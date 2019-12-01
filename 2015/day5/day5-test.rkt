#lang racket
(require rackunit
         "day5.rkt")

(check-true (validate-string "ugknbfddgicrmopn"))
(check-true (validate-string "u...i...o..."))
(check-true (validate-string "aaa"))
(check-false (validate-string "jchzalrnumimnmhp"))
(check-false (validate-string "haegwjzuvuyypxyu"))
(check-false (validate-string "dvszwmarrgswjxmb"))
