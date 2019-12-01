#lang racket

(define (part1 ls)
  (match-define (list-rest a tail) ls)
  (define-values (n p)
    (for/fold ([n 0]
               [prev a])
              ([m (append tail (list a))])
      (values (if (= prev m)
                  (+ n m)
                  n)
              m)))
  n)

(define (part2 ls)
  (define-values (head tail) (split-at ls (quotient (length ls) 2)))
  (for/fold ([n 0])
            ([a ls]
             [b (append tail head)])
    (if (= a b)
        (+ n a)
        n)))

(module+ main
  (define ls (map (compose string->number string)
                  (string->list (read-line))))
  (displayln (format "Part 1: ~a" (part1 ls)))
  (displayln (format "Part 2: ~a" (part2 ls))))
