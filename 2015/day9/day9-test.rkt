#lang racket

(require rackunit
         "day9.rkt")

(check-equal? (string->distance "London to Dublin = 464") (cons (set "London" "Dublin") 464))

(define lst (port->list read-distance (open-input-string #<<END
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
END
                                                         )))

(check-true (distance? (cons (set "a" "b") 55)))
(check-equal? (locations lst) '("Belfast" "Dublin" "London"))
(check-equal? (find-shortest-distance lst) 605)
