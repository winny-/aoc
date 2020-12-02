#|
TODO Memoize or at least use prefixes to eliminate duplicate work.
|#

#lang racket

(define ((solution N) ls)
  (foldl * 1 (for/first ([c (combinations ls N)] #:when (= 2020 (foldl + 0 c))) c)))

(define part1 (solution 2))
(define part2 (solution 3))

(module+ main
  (define ls (port->list read))
  (part1 ls)
  (part2 ls))
