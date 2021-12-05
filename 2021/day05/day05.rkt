#lang racket

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [(pregexp #px"(\\d+),(\\d+) -> (\\d+),(\\d+)" (list _ x1 y1 x2 y2))
     (list (list (string->number x1) (string->number y1))
           (list (string->number x2) (string->number y2)))]))

(define (in-range/inclusive start end)
  (in-range start ((if (> start end) sub1 add1) end) (if (> start end) -1 1)))

(define/match (points line)
  [((list (list x y1) (list x y2)))
   (for/list ([d (in-range/inclusive y1 y2)])
     (list x d))]
  [((list (list x1 y) (list x2 y)))
   (for/list ([d (in-range/inclusive x1 x2)])
     (list d y))]
  [((list (list x1 y1) (list x2 y2)))
   (for/list ([x (in-range/inclusive x1 x2)]
              [y (in-range/inclusive y1 y2)])
     (list x y))])

(define (go ls select-line?)
  (for*/fold ([acc (hash)]
              #:result (for/sum ([(k v) (in-hash acc)]
                                 #:when (> v 1))
                         1))
             ([line ls]
              #:when (select-line? line)
              [point (points line)])
    (hash-update acc point add1 (const 0))))

(define (part1 ls)
  (go ls (match-lambda [(list (list x1 y1) (list x2 y2))
                        (or (= x1 x2) (= y1 y2))])))

(define (part2 ls)
  (go ls identity))

(module+ main
  (define ls (port->list read2))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day05.rkt < sample.txt"
;; End:
