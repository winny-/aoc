#|
TODO Memoize or at least use prefixes to eliminate duplicate work.
|#

#lang racket

(define (part1 ls)
  (foldl * 1 (for/first ([c (combinations ls 2)] #:when (= 2020 (foldl + 0 c))) c)))

(define (part2 ls)
  (foldl * 1 (for/first ([c (combinations ls 3)] #:when (= 2020 (foldl + 0 c))) c)))

(module+ main
  (define ls (port->list read))
  (part1 ls)
  (part2 ls))
