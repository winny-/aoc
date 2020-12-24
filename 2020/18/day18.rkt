#lang racket

(define *operators* (hash '+ + '* *))
(define (binop? v)
  (hash-has-key? *operators* v))

(define/match (eval1 ls)
  [((or (? number? n) (list (? number? n)))) n]
  [((list (list group ...))) (eval1 group)]
  [((list lhs (? binop? op) rhs ...))
   ((hash-ref *operators* op) (eval1 lhs) (eval1 rhs))])
(define (eval2 ls)
  (let loop ([st empty] [ls ls])
    )
  [((list (list group ...)))])

(define/match (eval2 ls)
  [((or (? number? n) (list (? number? n)))) n]
  [((list (list group ...))) (eval2 group)]
  [((list lhs '* rhs ...))
   (* (eval2 lhs)
      (eval2 rhs))]
  [((list lhs '+ rhs))]
  [((list lhs (? binop? op) rhs ...))
   ((hash-ref *operators* op) (eval2 lhs) (eval2 rhs))])

(module+ main
  (define input
    (for/list ([line (port->lines)])
      (define (rev thing)
        (if (list? thing)
            (reverse (map rev thing))
            thing))
      (rev (with-input-from-string line port->list))))
  (for/sum ([expr input])
    (define n (eval1 expr))
    ;; (displayln n)
    n)
  (for/sum ([expr input])
    (eval2 expr)))
