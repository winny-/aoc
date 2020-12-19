#lang racket
(define (binop? v)
  (and (memq v '(+ *))))
(define/match (eval ls operators)
  [((or (? number? n) (list (? number? n))) _) n]
  [((list (list group ...)) _) (eval group operators)]
  [((list lhs (? binop? op) rhs ...) _)
   ((hash-ref operators op) (eval lhs operators) (eval rhs operators))])
(module+ main
  (define input
    (for/list ([line (port->lines)])
      (define (rev thing)
        (if (list? thing)
            (reverse (map rev thing))
            thing))
      (rev (with-input-from-string line port->list))))
  (for/sum ([expr input])
    (define n (eval expr (hash '+ + '* *)))
    (displayln n)
    n))
