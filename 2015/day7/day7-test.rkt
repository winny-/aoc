#lang racket

(require rackunit
         "day7.rkt")

(check-equal? (string->gate "123 -> x") (gate 'assign '(123) "x"))
(check-equal? (string->gate "x AND y -> d") (gate 'and '("x" "y") "d"))
(define gates (map string->gate (string-split #<<END
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
END
                                                   "\n")))
(define res (make-hash '(("d" . 72)
                         ("e" . 507)
                         ("f" . 492)
                         ("g" . 114)
                         ("h" . 65412)
                         ("i" . 65079)
                         ("x" . 123)
                         ("y" . 456))))
(check-equal? (execute-circuit gates) res)
