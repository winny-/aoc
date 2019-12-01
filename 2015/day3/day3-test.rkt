#lang racket

(require rackunit
         "day3.rkt")

(define-values (a b) (list-split-index-parity '(a b c d)))
(check-equal? a '(a c))
(check-equal? b '(b d))

(define-values (c d) (list-split-index-parity '(a b c)))
(check-equal? c '(a c))
(check-equal? d '(b))

