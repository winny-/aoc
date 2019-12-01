#lang racket

(require rackunit
         "day8.rkt")

(check-equal? (strip-quotes '(#\" #\a #\")) '(#\a))
(check-exn exn:fail? (λ () (strip-quotes '())))
(check-exn exn:fail? (λ () (strip-quotes '(#\a #\b))))
(check-equal? (tokenize-hex-escape #\2 #\3) #\#)
(check-equal? (tokenize-literal-list (string->list "\"a\\\"\\x75\"")) '(#\a #\" #\u))
(check-equal? (tokenize-literal "\"abc\"") '(#\a #\b #\c))
(check-equal? (tokenize-literal "\"\\\\\"") '(#\\))
(check-equal? (sum-pairs (cons 0 0)) (cons 0 0))
(check-equal? (sum-pairs (cons 1 2) (cons 3 4)) (cons 4 6))
