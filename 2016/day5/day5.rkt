#lang typed/racket

(: next-interesting (String Exact-Nonnegative-Integer . -> . (values String Exact-Nonnegative-Integer)))
(define (next-interesting door-id integer)
  (define h (string-append door-id (number->string integer)))
  (if (string-prefix? h "00000")
      (values h integer)
      (next-interesting door-id (add1 integer))))

(: generate-password (String . -> . String))
(define (generate-password door-id)
  (let loop ([n : Exact-Nonnegative-Integer 0] [chars : (Listof Char) '()])
    (if (= (length chars) 8)
        (list->string (reverse chars))
        (let-values ([(h m) (next-interesting door-id n)])
          (loop (add1 m) (cons (string-ref h 5) chars))))))

(module+ test
  (require typed/rackunit)
  (define-syntax check-values-equal?
    (syntax-rules ()
      [(_ a [b ...]) (check-equal? (call-with-values (thunk a) list) (list b ...))]))
  (check-values-equal? (next-interesting "abc" 0) [3231929 "00000155f8105dff7f56ee10fa9b9abd"])
  (check-equal? (generate-password "abc") "18f47a30"))

(module+ main
  (let ([s (read-line)])
    (when (eof-object? s)
      (error 'bad-input))
    (displayln (generate-password (string-trim s)))))
