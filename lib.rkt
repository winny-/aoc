#lang racket

(provide (all-defined-out))

(define (rassoc v alist [eq equal?])
  (for/first ([a (in-list alist)]
              #:when (eq (cdr a) v))
    a))

;; todo does not work
(define-syntax-rule (read-line/match cases ...)
  [(match (read-line)
     [(? eof-object?) eof]
     @,cases ...
     [_ eof])])
