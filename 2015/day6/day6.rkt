#lang typed/racket

(require math/array)

;; For http://adventofcode.com/day/6

(provide (all-defined-out))

;; Op should be 'on 'off or 'toggle
;; left should be a vector of two numbers
;; right should be a vector of two numbers
(struct instruction ([op : Symbol]
                     [left : (Pairof Exact-Nonnegative-Integer Exact-Nonnegative-Integer)]
                     [right : (Pairof Exact-Nonnegative-Integer Exact-Nonnegative-Integer)])
  #:transparent)

(: string->instruction (String . -> . (U False instruction)))
(define (string->instruction s)
  (define groups (regexp-match #rx"(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)" s))
  (if (or (false? groups)
          (< (length groups) 6)
          (not (andmap string? groups)))
      #f
      (let ([lx (string->number (third groups))]
            [ly (string->number (fourth groups))]
            [rx (string->number (fifth groups))]
            [ry (string->number (sixth groups))]
            [op (match (second groups)
                         ["turn on" 'on]
                         ["turn off" 'off]
                         ["toggle" 'toggle]
                         [_ #f])])
        (if (and (exact-nonnegative-integer? lx)
                 (exact-nonnegative-integer? ly)
                 (exact-nonnegative-integer? rx)
                 (exact-nonnegative-integer? ry)
                 (symbol? op))
            (instruction op (cons lx ly) (cons rx ry))
            #f))))

(: execute-instruction (instruction (Mutable-Array Boolean) . -> . (Mutable-Array Boolean)))
(define (execute-instruction ins grid)
  (: update (Boolean . -> . Boolean))
  (define update (match (instruction-op ins)
                   ['on (λ (cell) #t)]
                   ['off (λ (cell) #f)]
                   ['toggle not]))
  (for ([row (in-range (car (instruction-left ins)) (add1 (car (instruction-right ins))))])
    (for ([col (in-range (cdr (instruction-left ins)) (add1 (cdr (instruction-right ins))))])
      (define pos (vector row col))
      (array-set! grid pos (update (array-ref grid pos)))))
  grid)

(: play-lightshow ([(Listof instruction)] [(U (Mutable-Array Boolean) False)] . ->* . (Mutable-Array Boolean)))
(define (play-lightshow instructions [grid #f])
  (define g (if (false? grid)
                (make-grid)
                grid))
  (if (empty? instructions)
      g
      (play-lightshow (rest instructions)
                      (execute-instruction (first instructions) g))))

(: sum-grid ([(Mutable-Array Boolean)] [Boolean] . ->* . Exact-Nonnegative-Integer))
(define (sum-grid grid [state #t])
  (array-count (curry equal? state) grid))

(: make-grid ([] [Boolean] . ->* . (Mutable-Array Boolean)))
(define (make-grid [state #f])
  (array->mutable-array (make-array #(1000 1000) state)))

(: read-instruction (Input-Port . -> . (U instruction EOF False)))
(define (read-instruction ip)
  (define line (read-line ip))
  (if (string? line)
      (string->instruction line)
      line))

(module+ main
  (: instructions (Listof Any))
  (define instructions (port->list read-instruction))
  (if (andmap instruction? instructions)
      (sum-grid (play-lightshow instructions))
      "Bad input :("))
