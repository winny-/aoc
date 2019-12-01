#lang racket
; Advent of code day 12: http://adventofcode.com/day/12
(require json)

(define/contract (summate-jsexpr je)
  (jsexpr? . -> . number?)
  (match je
    [(? number? n) n]
    [(? list? ls) (foldl + 0 (map summate-jsexpr ls))]
    [(? hash? h) (foldl + 0 (map summate-jsexpr (hash-values h)))]
    [_ 0]))

(define/contract (summate-jsexpr2 je)
  (jsexpr? . -> . number?)
  (match je
    [(? number? n) n]
    [(? list? ls) (foldl + 0 (map summate-jsexpr2 ls))]
    [(? hash? h)
     (if (ormap (λ (v) (and (string? v) (string-contains? v "red"))) (hash-values h))
         0
         (foldl + 0 (hash-map h (λ (k v) (summate-jsexpr2 v)))))]
    [_ 0]))

(module+ test
  (require rackunit)
  (define f (compose summate-jsexpr string->jsexpr))
  (define g (compose summate-jsexpr2 string->jsexpr))
  (check-equal? (f "[1,2,3]") 6)
  (check-equal? (f "{\"a\":2,\"b\":4}") 6)
  (check-equal? (f "[[[3]]]") 3)
  (check-equal? (f "{\"a\":{\"b\":4},\"c\":-1}") 3)
  (check-equal? (f "{\"a\":[-1,1]}") 0)
  (check-equal? (f "[-1,{\"a\":1}]") 0)
  (check-equal? (f "[]") 0)
  (check-equal? (f "{}") 0)
  (check-equal? (g "{\"a\":2,\"b\":4}") 6)
  (check-equal? (g "[1,2,3]") 6)
  (check-equal? (g "[1,{\"c\":\"red\",\"b\":2},3]") 4)
  (check-equal? (g "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") 0)
  (check-equal? (g "[1,\"red\",5]") 6))

(module+ main
  (define jo (read-json))
  (displayln (format "With part #1 rules: ~a" (summate-jsexpr jo)))
  (displayln (format "With part #2 rules: ~a" (summate-jsexpr2 jo))))
