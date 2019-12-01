#lang racket

(define (list->histogram ls)
  (for/fold ([acc (hash)])
            ([x ls])
    (hash-update acc x add1 0)))

(define (part1 lines)
  (define histograms (map list->histogram lines))
  (* (count (curry member 2) (map hash-values histograms))
     (count (curry member 3) (map hash-values histograms))))

(define (common as bs)
  (for/list ([a as]
             [b bs]
             #:when (equal? a b))
    a))

(define (differences as bs)
  (- (max (length as) (length bs)) (length (common as bs))))

(define (part2 ls)
  (define pairs (combinations ls 2))
  (for/first ([p pairs]
              #:when (= 1 (differences (first p) (second p))))
    (list->string (common (first p) (second p)))))

(module+ main
  (define entries (map string->list (port->list read-line)))
  (displayln (part1 entries))
  (displayln (part2 entries)))
