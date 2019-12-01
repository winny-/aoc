#lang typed/racket

(: next-password (String . -> . String))
(define (next-password prev)
  (define p (increment-password prev))
  (if (valid? p)
      p
      (next-password p)))

(define a (char->integer #\a))
(define z (char->integer #\z))
(define invalid-chars '(#\i #\o #\l))

(: string->integers (String . -> . (Listof Exact-Nonnegative-Integer)))
(define (string->integers s)
  (reverse (map char->integer (string->list s))))

(: integers->string ((Listof Exact-Nonnegative-Integer) . -> . String))
(define (integers->string lst)
  (list->string (reverse (map integer->char lst))))

(: increment-password (String . -> . String))
(define (increment-password s)
  (integers->string (incr (string->integers s))))

(: incr ((Listof Exact-Nonnegative-Integer) . -> . (Listof Exact-Nonnegative-Integer)))
(define (incr lst)
  (match lst
    [(? empty?) empty]
    [(list-rest (? (curry eqv? z)) others)
     (cons a (incr others))]
    [(list-rest n others)
     (cons (add1 n) others)]))

(: valid? ((U (Listof Char) String) . -> . Boolean))
(define (valid? ls-or-s)
  (define lst (if (string? ls-or-s)
                  (string->list ls-or-s)
                  ls-or-s))
  (and (straight? lst)
       (valid-chars? lst)
       (two+-overlaps? lst)))

(: two+-overlaps? ((Listof Char) . -> . Boolean))
(define (two+-overlaps? lst)
  (> (count-overlaps lst) 1))

(: count-overlaps ((Listof Char) . -> . Exact-Nonnegative-Integer))
(define (count-overlaps lst)
  (cond
    [(< (length lst) 2) 0]
    [(equal? (car lst) (cadr lst))
     (add1 (count-overlaps (cddr lst)))]
    [(< (length lst) 3) 0]
    [else (count-overlaps (cdr lst))]))

(: straight? ((Listof Char) . -> . Boolean))
(define (straight? lst)
  (if (< (length lst) 3)
      #f
      (let*-values ([(l r) (split-at lst 3)]
                    [(il) (map char->integer l)])
        (or (= (car il) (- (cadr il) 1) (- (caddr il) 2))
            (straight? r)))))

(: valid-chars? ((Listof Char) . -> . Boolean))
(define (valid-chars? lst)
  (andmap (λ (c) (not (memv c lst))) invalid-chars))

(module+ test
  (require typed/rackunit)
  (check-true (straight? '(#\x #\y #\z)))
  (check-false (straight? '(#\z #\y #\x)))
  (check-true (straight? '(#\a #\z #\a #\b #\c #\d)))
  (check-true (valid-chars? '(#\a #\b #\c)))
  (check-false (valid-chars? '(#\o #\a #\c)))
  (check-false (valid-chars? '(#\i)))
  (check-false (valid-chars? '(#\l)))
  (check-equal? (count-overlaps '()) 0)
  (check-equal? (count-overlaps '(#\a)) 0)
  (check-equal? (count-overlaps '(#\a #\z)) 0)
  (check-equal? (count-overlaps '(#\a #\a)) 1)
  (check-equal? (count-overlaps '(#\a #\a #\a)) 1)
  (check-equal? (count-overlaps '(#\a #\a #\a #\a)) 2)
  (check-equal? (count-overlaps '(#\a #\b #\b #\c #\c #\c)) 2)
  (check-true (two+-overlaps? '(#\a #\a #\a #\a)))
  (check-false (two+-overlaps? '(#\a #\a #\c #\b)))
  (check-false (valid? "hijklmmn"))
  (check-false (valid? (string->list "abbceffg")))
  (check-false (valid? (string->list "abbcegjk")))
  (check-equal? (next-password "abcdefgh") "abcdffaa")
  (displayln "---")
  (check-equal? (next-password "ghijklmn") "ghjaabcc"))

(module+ main
  (define s (read-line))
  (displayln (if (eof-object? s)
                 "EOF recieved. Exiting."
                 (next-password s))))