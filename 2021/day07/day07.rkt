#lang debug racket

(define (read2 [ip (current-input-port)])
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (map string->number (string-split s ","))]))

(define (go ls fuel-cost)
  (for/fold ([min-fuel #f])
            ([target (in-range (argmin identity ls)
                               (add1 (argmax identity ls)))])
    target
    (define fuel (for/sum ([crab ls])
                   (fuel-cost (abs (- crab target)))))
    (if (or (not min-fuel) (<= fuel min-fuel))
        fuel
        min-fuel)))

(define (part1 ls)
  (go ls identity))

(define (part2 ls)
  ;; I plugged in the sequence of integers representing the distances into OEIS
  ;; (1,3,6,10 ...) and it says its A000217 Triangular numbers.
  (define (triangular-numbers n)
    (/ (* n (add1 n)) 2))
  (go ls triangular-numbers))

(module+ main
  (define ls (read2))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day07.rkt < sample.txt"
;; End:
