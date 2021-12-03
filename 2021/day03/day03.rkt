#lang racket

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (string->number s 2)]))

(define (integer-width i)
  (let loop ([n i])
    (if (zero? n)
        0
        (add1 (loop (arithmetic-shift n -1))))))

(define (part1 ls)
  (define width (integer-width (argmax identity ls)))
  (define gamma
    (for/fold ([acc  (make-list width 0)]
               #:result (for/fold ([n 0])
                                  ([freq acc]
                                   [pos (in-naturals 0)])
                          (if (> freq (quotient (length ls) 2))
                              (+ n (arithmetic-shift 1 pos))
                              n)))
              ([n ls])
      (for/list ([num-set acc]
                 [pos (in-naturals 0)])
        (+ (if (bitwise-bit-set? n pos) 1 0) num-set))))
  (define epsilon (bitwise-xor gamma (for/sum ([pos (in-range width)])
                                       (arithmetic-shift 1 pos))))
  (* gamma epsilon))

(define (part2 ls)
  (define width (integer-width (argmax identity ls)))
  (define (f fn)
    (for/fold ([acc ls]
               #:result (car acc))
              ([pos (in-cycle (in-range (sub1 width) -1 -1))]
               #:break (<= (length acc) 1))
      (define-values (set unset)
        (partition (curryr bitwise-bit-set? pos) acc))
      (define most-frequent (>= (length set) (length unset)))
      (if (fn most-frequent)
          set
          unset)))
  (* (f identity) (f not)))

(module+ main
  (define ls (port->list read2))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day03.rkt < sample.txt"
;; End:
