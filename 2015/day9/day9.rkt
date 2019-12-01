#lang racket

(provide (all-defined-out))

(define distance? (cons/c set? exact-nonnegative-integer?))

(define/contract (string->distance s)
  (string? . -> . (or/c false? distance?))
  (define groups (regexp-match #rx"([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)" s))
  (if (or (false? groups) (null? groups))
      #f
      (let ([n (string->number (cadddr groups))])
        (if (false? n)
            #f
            (cons (set (cadr groups) (caddr groups)) n)))))

(define/contract (find-shortest-distance lst)
  ((listof distance?) . -> . exact-nonnegative-integer?)
  (apply min (map (curryr calculate-distance lst) (permutations (locations lst)))))

(define/contract (find-longest-distance lst)
  ((listof distance?) . -> . exact-nonnegative-integer?)
  (apply max (map (curryr calculate-distance lst) (permutations (locations lst)))))

(define/contract (calculate-distance path distances)
  ((listof string?) (listof distance?) . -> . exact-nonnegative-integer?)
  (if (or (null? path) (null? (cdr path)))
      0
      (+ (cdr (find-distance (set (car path) (cadr path)) distances))
         (calculate-distance (cdr path) distances))))

(define/contract (find-distance segment lst)
  (set? (listof distance?) . -> . distance?)
  (car (filter (λ (x) (set=? (car x) segment)) lst)))

(define/contract (locations lst)
  ((listof distance?) . -> . (listof string?))
  (sort (set->list (apply set-union (map car lst))) string<?))

(define/contract (read-distance ip)
  (input-port? . -> . (or/c #f eof-object? distance?))
  (define line (read-line ip))
  (if (eof-object? line)
      line
      (string->distance line)))

(module+ main
  (define lst (port->list read-distance))
  (displayln (format "Shortest: ~a" (find-shortest-distance lst)))
  (displayln (format "Longest:  ~a" (find-longest-distance lst))))
