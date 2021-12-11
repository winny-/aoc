#lang racket

(provide (all-defined-out))

(define (rassoc v alist [eq equal?])
  (for/first ([a (in-list alist)]
              #:when (eq (cdr a) v))
    a))

(define ORTHOGINAL-OFFSETS '((-1 0) (1 0) (0 -1) (0 1)))
(define DIAGONAL-OFFSETS '((-1 -1) (-1 1) (1 -1) (1 1)))
(define ALL-OFFSETS (append ORTHOGINAL-OFFSETS DIAGONAL-OFFSETS))

(define/match (lookup grid loc)
  [(_ (list x y)) (list-ref (list-ref grid y) x)])

(define/match (neighbors grid loc #:direction [direction 'all] #:lookup [do-lookup #f])
  [((list a _ ...) (list x y) _ _)
   (filter identity (for/list ([offset (match direction
                                         [(list _ ...) direction]
                                         ['all ALL-OFFSETS]
                                         ['orthoginal ORTHOGINAL-OFFSETS]
                                         ['diagonal DIAGONAL-OFFSETS])])
                      (match-define (and neigh (list nx ny)) (map + loc offset))
                      (and (not (equal? loc neigh))
                           (<= 0 nx) (> (length a) nx)
                           (<= 0 ny) (> (length grid) ny)
                           (if do-lookup
                               (list nx ny (lookup grid neigh))
                               neigh))))])

;; todo does not work
(define-syntax-rule (read-line/match cases ...)
  [(match (read-line)
     [(? eof-object?) eof]
     @,cases ...
     [_ eof])])
