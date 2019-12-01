#lang racket

(provide (all-defined-out))

(define/contract (parse-literal s)
  (string? . -> . (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))
  (let* ([trimmed (string-trim s)]
         [tokenized (tokenize-literal trimmed)])
    (cons (string-length trimmed) (length tokenized))))

(define/contract (tokenize-literal s)
  (string? . -> . (listof char?))
  (tokenize-literal-list (string->list s)))

(define/contract (strip-quotes lst)
  ((listof char?) . -> . (listof char?))
  (let ([left (car lst)]
        [right (car (take-right lst 1))])
    (unless (and (char=? left #\") (char=? right #\"))
      (error "No quotes" lst left right))
    (drop-right (drop lst 1) 1)))

(define/contract (tokenize-hex-escape a b)
  (char? char? . -> . char?)
  (integer->char (string->number (list->string (list a b)) 16)))


(define/contract (tokenize-literal-list lst [tokens '()] [ate-quotes #f])
  ([(listof char?)] [(listof char?) boolean?] . ->* . (listof char?))
  (if ate-quotes
      (match lst
        [(? null?) tokens]
        [(list* #\\ #\\ _) (tokenize-literal-list (drop lst 2) (append tokens (list #\\)) ate-quotes)]
        [(list* #\\ #\" _) (tokenize-literal-list (drop lst 2) (append tokens (list #\")) ate-quotes)]
        [(list* #\\ #\x a b _) (tokenize-literal-list (drop lst 4) (append tokens (list (tokenize-hex-escape a b))) ate-quotes)]
        [(list #\\ ...) (error "Bad escape" lst)]
        [(list* c _) (tokenize-literal-list (drop lst 1) (append tokens (list c)) ate-quotes)]
        [_ (error "No matching clause" lst)])
      (tokenize-literal-list (strip-quotes lst) tokens #t)))

(define/contract (sum-pairs . args)
  ([] #:rest (listof (cons/c number? number?)) . ->* . (cons/c number? number?))
  (if (null? args)
      '(0 . 0)
      (let ([a (car args)]
            [b (apply sum-pairs (cdr args))])
        (cons (+ (car a) (car b))
              (+ (cdr a) (cdr b))))))

(module+ main
  (let ([sum (apply sum-pairs (map parse-literal (port->list read-line)))])
    (- (car sum) (cdr sum))))
