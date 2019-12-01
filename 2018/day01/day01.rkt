#lang racket

(define (part1 ls)
  (for/sum ([n ls]) n))

(define (part2 ls)
  (for/fold ([seen (set 0)]
             [m 0]
             #:result m)
            ([n (in-cycle ls)])
    (define k (+ m n))
    #:final (set-member? seen k)
    (values (set-add seen k) k)))

(module+ main
  (define ls (port->list read))
  (part1 ls)
  (part2 ls))
