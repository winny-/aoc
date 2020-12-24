#lang racket

(provide (struct-out node)
         port->orbits)

(struct node (key children) #:transparent)

(define (orbit-read [in (current-input-port)])
  (match (read-line in)
    [(? eof-object? e) e]
    [(regexp #px"([A-Z0-9]+)[)]([A-Z0-9]+)" (list _ center orbiter))
     (list (string->symbol center) (string->symbol orbiter))]
    [other (raise-syntax-error 'orbit-read "Bad orbit ~v" other)]))


(define (port->orbits [in (current-input-port)])
  (for/fold ([h (hasheq)])
            ([o (port->list orbit-read in)])
    (match-define (list center orbiter) o)
    (define h1 (hash-update h center
                            (match-lambda [(struct node (_ others))
                                           (node center (set-add others orbiter))])
                            (const (node center (seteq orbiter)))))
    (hash-update h1 orbiter identity (const (node orbiter (seteq))))))
