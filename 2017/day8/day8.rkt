#lang racket

(module+ test
  (require rackunit))

(define SAMPLE-DATA #<<EOF
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
EOF
  )

(define modifiers
  (hash 'inc +
        'dec -))

(define operations
  (hash '> >
        '< <
        '!= (negate =)
        '== =
        '<= <=
        '>= >=))

(struct instruction [destination modifier source condition] #:transparent)
(struct condition [operation a b] #:transparent)

(struct state [registers address])

(define (string->symbol+number s)
  (define n (string->number s))
  (if n
      n
      (string->symbol s)))

(define (read-instruction ip)
  (define ls
    (regexp-match #rx"([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+|-?[0-9]+) (>|<|!=|<=|>=|==) ([a-z]+|-?[0-9]+)\n?"
                  ip))
  (if ls
      (let ()
        (match-define (list _ d m s a o b) (map bytes->string/utf-8 ls))
        (instruction (string->symbol+number d)
                     (string->symbol m)
                     (string->symbol+number s)
                     (condition (string->symbol o)
                                (string->symbol+number a)
                                (string->symbol+number b))))
      eof))

(define (step instructions the-state)
  (match-define (state registers address) the-state)
  (match-define (instruction destination modifier source the-condition) (list-ref instructions address))
  (match-define (condition operation a b) the-condition)
  (define (lookup reg+num)
    (if (number? reg+num)
        reg+num
        (hash-ref registers reg+num 0)))
  (state (if ((hash-ref operations operation) (lookup a) (lookup b))
             (hash-update registers destination
                          (λ (x) ((hash-ref modifiers modifier) x source))
                          (thunk* 0))
             registers)
         (add1 address)))

(define (run instructions)
  (let loop ([st (state (hash) 0)])
    (if (< -1 (state-address st) (length instructions))
        (loop (step instructions st))
        st)))

(define (part1 instructions)
  (argmax identity (hash-values (state-registers (run instructions)))))


(define (part2 instructions)
  (match-let loop ([a -inf.0] [(and st (state registers address)) (state (hash) 0)])
             (if (< -1 address (length instructions))
                 (loop 
                  (argmax identity (cons a (hash-values registers)))
                  (step instructions st))
                 a)))


(module+ main
  (define ls (port->list read-instruction))
  (displayln (format "Part 1: ~a" (part1 ls)))
  (displayln (format "Part 2: ~a" (part2 ls))))
