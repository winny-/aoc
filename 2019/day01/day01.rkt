#lang racket

(define (fuel mass)
  (max 0 (- (quotient mass 3) 2)))

(define (part1 modules)
  (for/sum ([m modules])
    (fuel m)))

(define (fuel/p2 mass)
  (for/sum ([f (let loop ([f (fuel mass)])
                 (if (positive? f)
                     (stream-cons f (loop (fuel f)))
                     empty-stream))])
    f))

(define (part2 modules)
  (for/sum ([m modules])
    (fuel/p2 m)))

(module+ main
  (define modules (port->list read))
  (printf "Part #1: ~a\n" (part1 modules))
  (printf "Part #2: ~a\n" (part2 modules)))
