#lang racket

(define (digits n [base 10])
  (let loop ([v n] [acc empty])
    (if (zero? v)
        acc
        (let-values ([(q r) (quotient/remainder v base)])
          (loop q (cons r acc))))))

(define ((password-part1/c lower upper) password)
  (and (<= lower password upper)
       (let ([ls (digits password)])
         (and (for/or ([a ls] [b (cdr ls)])
                (= a b))
              (for/and ([a ls] [b (cdr ls)])
                (nonnegative-integer? (- b a)))))))

(define (part1 lower upper)
  (define password? (password-part1/c lower upper))
  (for/sum ([password (in-range lower (add1 upper))]
            #:when (password? password))
    1))

(define (runs ls [equal? equal?])
  (let loop ([ls ls] [acc empty])
    (match ls
      [(list) (reverse acc)]
      [(list a b ...)
       (define-values (same others) (splitf-at b (curry equal? a)))
       (loop others (cons (cons a same) acc))])))

(define ((password-part2/c lower upper) password)
  (and (<= lower password upper)
       (let ([ls (digits password)])
         (and (for/or ([r (runs ls)])
                       (= 2 (length r)))
              (for/and ([a ls] [b (cdr ls)])
                (nonnegative-integer? (- b a)))))))

(define (part2 lower upper)
  (define password? (password-part2/c lower upper))
  (for/sum ([password (in-range lower (add1 upper))]
            #:when (password? password))
    1))

(module+ main
  (define-values (lower upper)
    (match (read-line)
      [(regexp #px"(\\d+)-(\\d+)" (list _ l u))
       (values (string->number l)
               (string->number u))]))
  (part1 lower upper)
  (part2 lower upper))
