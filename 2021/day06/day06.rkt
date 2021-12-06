#lang racket

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (map string->number (string-split s ","))]))

(define OLD 6)
(define (age ls)
  (for/fold ([acc empty]
             #:result (reverse acc))
            ([lanternfish ls])
    (match lanternfish
      [0 (list* (+ 2 OLD) OLD acc)]
      [_ (cons (sub1 lanternfish) acc)])))


(define (go ls days)
  (for/fold ([pool ls]
             #:result (length pool))
            ([day (in-range 1 (add1 days))])
    (printf "age: ~a\n" day)
    (age pool)))

(define (part1 ls)
  (go ls 80))

(define (part2 ls)
  (go ls 256))

(module+ main
  (define ls (read2 (current-input-port)))
  (displayln ls)
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day06.rkt < sample.txt"
;; End:
