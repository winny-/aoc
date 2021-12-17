#lang debug racket

(define (read2 [ip (current-input-port)])
  (match (read-line ip)
    [(? eof-object? e) e]
    [(pregexp #px"target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)" (list _ bounds ...))
     (map string->number bounds)]))

(struct Simulation (location velocity) #:transparent)

(define/match (step sim)
  [((struct Simulation (loc (and velocity (list dx dy)))))
   (Simulation (map + loc velocity)
               (list (max 0 (sub1 dx))
                     (sub1 dy)))])

(define (summation i n)
  (for/sum ([x (in-range i (add1 n))])
    x))

(define (part1 bounds)
  (match-define (list xmin xmax ymin ymax) bounds)
  (define dmin
    (for/first ([v (in-naturals)]
                #:when (> (summation 1 v) xmin))
      v))
  (define by
    (for/fold ([besty -inf.0]
               #:result (inexact->exact (sub1 besty)))
              ([c  (in-range ymin (add1 0))]
               #:when (let loop ([z c] [pos 0])
                        (or (<= ymin pos ymax)
                            (and (>= pos ymin) (loop (sub1 z) (+ pos z))))))
      (max besty (abs c))))
  ;; (printf "~a,~a\n" dmin by)
  (summation 1 by))

(define (part2 ls)
  (void))

(module+ main
  (define ls (read2))
  (displayln ls)
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day17.rkt < sample.txt"
;; End:
