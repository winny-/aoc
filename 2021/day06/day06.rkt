#lang racket

(require racket/hash)

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (for/fold ([acc (hash)])
                 ([n (map string->number (string-split s ","))])
         (hash-update acc n add1 (const 0)))]))

(define OLD 6)
(define (step pool)
  (for/fold ([acc (hash)])
            ([(age count) (in-hash pool)])
    (match age
      [0 (hash-union acc (hash OLD count
                               (+ 2 OLD) count))]
      [_ (hash-update acc (sub1 age)  (curry +  count) (const 0))])))


(define (go input days)
  (for/fold ([pool input]
             #:result (for/sum ([n (in-hash-values pool)]) n))
            ([day (in-range 1 (add1 days))])
    (step pool)))

(define (part1 input)
  (go input 80))

(define (part2 input)
  (go input 256))

(module+ main
  (define input (read2 (current-input-port)))
  (part1 input)
  (part2 input))

;; Local Variables:
;; compile-command: "racket day06.rkt < sample.txt"
;; End:
