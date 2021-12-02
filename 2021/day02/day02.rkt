#lang racket

(struct Instruction [what mag] #:transparent)

(define (read-instruction [ip (current-input-port)])
  (match* ((read) (read))
    [((? eof-object? e) _) e]
    [(_ (? eof-object? e)) e]
    [(what mag) (Instruction what mag)]))

(define (part1 ls)
  (for/fold ([hor 0]
             [dep 0]
             #:result (* hor dep))
            ([i ls])
    (match-define (struct Instruction (what msg)) i)
    (match what
      ['forward (values (+ msg hor) dep)]
      ['up (values hor (- dep msg))]
      ['down (values hor (+ dep msg))])))

(define (part2 ls)
  (for/fold ([hor 0]
             [dep 0]
             [aim 0]
             #:result (* hor dep))
            ([i ls])
    (match-define (struct Instruction (what msg)) i)
    (match what
      ['forward (values (+ msg hor) (+ dep (* aim msg)) aim)]
      ['up (values hor dep (- aim msg))]
      ['down (values hor dep (+ aim msg))])))

(module+ main
  (define ls (port->list read-instruction))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day02.rkt < sample.txt"
;; End:
