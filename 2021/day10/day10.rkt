#lang debug racket

(require "../../lib.rkt")

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (string->list s)]))

(define PAIRS
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))

(define PART1-VALUE
  (hash #\) 3
        #\] 57
        #\} 1197
        #\> 25137))

(define PART2-VALUE
  (hash #\( 1
        #\[ 2
        #\{ 3
        #\< 4))

(define (part1 ls)
  (define bad-lines
    (for/fold ([h (for/hash ([k (in-hash-keys PART1-VALUE)]) (values k 0))])
              ([line ls])
      (define bad
        (let/ec esc
          (for/fold ([stack empty])
                    ([c line])
            (match (rassoc c PAIRS)
              [(cons open _) (if (char=? (car stack) open)
                                 (cdr stack)
                                 (esc c))]
              [#f (cons c stack)]))))
      (if (char? bad)
          (hash-update h bad add1)
          h)))
  (for/sum ([(k v) (in-hash bad-lines)])
    (* (hash-ref PART1-VALUE k) v)))

(define (part2 ls)
  (define bad-lines
    (filter identity
            (for/list ([line ls])
              (define bad
                (let/ec esc
                  (for/fold ([stack empty])
                            ([c line])
                    (match (rassoc c PAIRS)
                      [(cons open _) (if (char=? (car stack) open)
                                         (cdr stack)
                                         (esc #f))]
                      [#f (cons c stack)]))))
              (and (list? bad) (not (empty? bad))
                   (for/fold ([n 0])
                             ([b bad])
                     (+ (* 5 n) (hash-ref PART2-VALUE b)))
                   ))))
  (list-ref (sort bad-lines <)  (quotient (length bad-lines) 2)))

(module+ main
  (define ls (port->list read2))
  ;; (displayln ls)
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day10.rkt < sample.txt"
;; End:

