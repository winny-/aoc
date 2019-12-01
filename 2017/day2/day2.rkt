#lang racket

(define (part1 ls)
  (for/sum ([n (map (curry argmax identity) ls)]
            [m (map (curry argmin identity) ls)])
    (- n m)))

(define (part2 ls)
  (for/sum ([line ls])
    (for*/first ([n line]
                 [m (remove n line)]
                 #:when (zero? (modulo n m)))
      (quotient n m))))


(module+ main
  (define ls
    (for/list ([line (port->list read-line)])
      (map string->number (string-split line #rx"\t"))))
  (displayln (format "Part 1: ~a" (part1 ls)))
  (displayln (format "Part 2: ~a" (part2 ls))))
