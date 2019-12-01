#lang racket

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

(define (string->symbol+number s)
  (define n (string->number s))
  (or n (string->symbol s)))

(define (read-instruction ip)
  (define ls
    (regexp-match
     #rx"([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+|-?[0-9]+) (>|<|!=|<=|>=|==) ([a-z]+|-?[0-9]+)\n?"
     ip))
  (if ls
      (match-let ([(list _ d m s a o b) (map bytes->string/utf-8 ls)])
        (instruction (string->symbol+number d)
                     (string->symbol m)
                     (string->symbol+number s)
                     (condition (string->symbol o)
                                (string->symbol+number a)
                                (string->symbol+number b))))
      eof))

(define (step the-instruction registers)
  (match-define (instruction destination modifier source the-condition) the-instruction)
  (match-define (condition operation a b) the-condition)
  (define (lookup reg+num)
    (if (number? reg+num)
        reg+num
        (hash-ref registers reg+num 0)))
  (if ((hash-ref operations operation) (lookup a) (lookup b))
      (hash-update registers destination
                   (curryr (hash-ref modifiers modifier) (lookup source))
                   (thunk* 0))
      registers))


(module+ main
  (define-values (intermediate1 solution2)
    (for/fold ([registers (hash)]
               [maximal -inf.0])
              ([the-instruction (port->list read-instruction)])
      (define registers2 (step the-instruction registers))
      (values registers2 (argmax identity (cons maximal (hash-values registers2))))))
  (define solution1 (argmax identity (hash-values intermediate1)))
  (displayln (format "Part 1: ~a" solution1))
  (displayln (format "Part 2: ~a" solution2)))
