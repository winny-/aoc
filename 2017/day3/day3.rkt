#lang racket

(module+ test
  (require rackunit))

(define (get-ring n)
  (define m (ceiling (sqrt n)))
  (if (odd? m)
      m
      (add1 m)))

(module+ test
  (test-case "get-ring"
    (check-= (get-ring 1) 1 0)
    (check-= (get-ring 4) 3 0)
    (check-= (get-ring 12) 5 0)
    (check-= (get-ring 23) 5 0)
    (check-= (get-ring 1024) 33 0)))

(define (get-closest-orthagonal n)
  (define ring (get-ring n))
  (if (= 1 ring)
      1
      (argmin (λ (x) (abs (- x n))) (range (+ (quotient ring 2) (sqr (- ring 2)))
                                           (add1 (sqr ring))
                                           (sub1 ring)))))

(module+ test
  (define tests
    '((2 2.0)
      (3 2.0)
      (4 4)
      (5 4.0)
      (6 6.0)
      (7 6.0)
      (8 8.0)
      (9 8)
      (10 11.0)
      (11 11.0)
      (12 11.0)
      (13 11.0)
      (14 15.0)
      (15 15.0)
      (16 15)
      (17 15.0)
      (18 19.0)
      (19 19.0)
      (20 19.0)
      (21 19.0)
      (22 23.0)
      (23 23.0)
      (24 23.0)))
  (test-case "get-closest-orthagonal"
    (for ([t tests])
      (check-= (get-closest-orthagonal (car t)) (cadr t) 0 (~a t)))))

(define (part1 target)
  (+ (abs (- target (get-closest-orthagonal target)))
     (quotient (get-ring target) 2)))

(module+ test
  (test-case "part1"
    (check-= (part1 1) 0 0)
    (check-= (part1 12) 3 0)
    (check-= (part1 23) 2 0)
    (check-= (part1 1024) 31 0)))

(define (part2-stream)
  (let loop ([a 1] [b 0] [c 0])
    (stream-cons a (loop (+ a b c) a b))))

