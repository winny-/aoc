#lang typed/racket

(provide (all-defined-out))

(module+ test
  (require typed/rackunit)
  (check-equal? (look-and-say '(1) 5) '(3 1 2 2 1 1)))

(: look-and-say ((Listof Exact-Positive-Integer) Exact-Nonnegative-Integer . -> . (Listof Exact-Positive-Integer)))
(define (look-and-say lst iterations)
  (if (or (zero? iterations) (empty? lst))
      lst
      (look-and-say (step lst) (sub1 iterations))))

(module+ test
  (require typed/rackunit)
  (check-equal? (step '(1)) '(1 1))
  (check-equal? (step '(1 1)) '(2 1))
  (check-equal? (step '(2 1)) '(1 2 1 1))
  (check-equal? (step '(1 1 1 2 2 1)) '(3 1 2 2 1 1))
  (check-equal? (step '(3 1 1)) '(1 3 2 1)))

(: step ((Listof Exact-Positive-Integer) . -> . (Listof Exact-Positive-Integer)))
(define (step lst)
  (define res (flatten (map (λ (lst) (if (list? lst) (list (length lst) (car lst)) (error 'bad-list)))
                            (split-on lst equal?))))
  (if (andmap exact-positive-integer? res)
      res
      (error 'bad-list)))

(: split-on ((Listof Any) (Any Any . -> . Boolean) . -> . (Listof Any)))
(define (split-on lst func)
  (define-values (l r) (split-on-first lst func))
  (if (empty? r)
      (list l)
      (append (list l) (split-on r func))))

(: split-on-first ((Listof Any) (Any Any . -> . Boolean) . -> . (values (Listof Any) (Listof Any))))
(define (split-on-first lst func)
  (if (or (empty? lst) (empty? (cdr lst)))
      (values lst empty)
      (if (func (car lst) (cadr lst))
          (let-values ([(l r) (split-on-first (cdr lst) func)])
            (values (cons (car lst) l) r))
          (values (list (car lst)) (cdr lst)))))

(require typed/rackunit)
(define-syntax (check-values-equal? stx)
  (syntax-case stx ()
    [(_ a (b ...)) #'(check-equal? (call-with-values (thunk a) list)
                                   (list b ...))]
    [(_ a (b ...) c) #'(check-equal? (call-with-values (thunk a) list)
                                     (list b ...)
                                     c)]))

(module+ test
  (require typed/rackunit)
  (check-values-equal? (split-on-first '() equal?)
                       (empty empty))
  (check-values-equal? (split-on-first '(a) equal?)
                       ('(a) empty))
  (check-values-equal? (split-on-first '(a b) (λ (a b) (not (equal? a b))))
                       ('(a b) empty))
  (check-values-equal? (split-on-first '(a a b b c) equal?)
                       ('(a a) '(b b c)))
  (check-values-equal? (split-on-first '(a b c d e) equal?)
                       ('(a) '(b c d e)))
  (check-equal? (split-on '() equal?) '(())) ; is this the best way??
  (check-equal? (split-on '(a) equal?) '((a)))
  (check-equal? (split-on '(a b) equal?) '((a) (b)))
  (check-equal? (split-on '(a a b b c) equal?) '((a a) (b b) (c)))
  (check-equal? (split-on '(a b c d e) equal?) '((a) (b) (c) (d) (e))))

