#lang racket

;; For http://adventofcode.com/day/3

(provide (all-defined-out))

(define/contract (read-movement input-port)
  (-> input-port? (or/c eof-object? vector?))
  (match (read-char input-port)
    [#\^ #(0 -1)]
    [#\v #(0 1)]
    [#\< #(-1 0)]
    [#\> #(1 0)]
    [(? eof-object? c) c]
    [_ #(0 0)]))

(define/contract (plot-path lst #:visited [visited (set #(0 0))] #:pos [pos #(0 0)])
  ([(listof (vectorof exact-integer?))] [#:visited set? #:pos (vectorof exact-integer?)] . ->* . set?)
  (if (null? lst)
      visited
      (let ([next-pos (vector-map + pos (car lst))])
        (plot-path (cdr lst) #:visited (set-add visited next-pos) #:pos next-pos))))

(define/contract (list-split-index-parity lst)
  (list? . -> . (values list? list?))
  (cond
    [(null? lst) (values '() '())]
    [(null? (cdr lst)) (values (list (car lst)) '())]
    [else (let-values ([(a b) (list-split-index-parity (cddr lst))])
            (values (cons (car lst) a)
                    (cons (cadr lst) b)))]))

(module+ main
  (define lst (port->list read-movement))
  (displayln (format "Santa can deliver presents to ~a on his own."
                     (set-count (plot-path lst))))
  (define two-lists (call-with-values (thunk (list-split-index-parity lst)) list))
  (displayln (format "With Robot Santa, Santa can deliver presents to ~a as a team."
                     (set-count (apply set-union (map plot-path two-lists))))))
