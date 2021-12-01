#lang racket

(define (part1 ls)
  (for/fold ([acc 0]
             [prev (car ls)]
             #:result acc)
            ([v (cdr ls)])
    (values
     (if (< prev v)
         (add1 acc)
         acc)
     v)))

(define (part2 ls)
  (part1 (for/list ([a ls]
                    [b (cdr ls)]
                    [c (cddr ls)])
           (+ a b c))))

(module+ main
  (define ls (port->list))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day01.rkt < input.txt"
;; End:
