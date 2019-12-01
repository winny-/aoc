#lang racket

(define (react chain)
  (for/fold ([acc empty]
             #:result (reverse acc))
            ([c chain])
    #;(printf "; ~a ~a\n" c acc)
    (match acc
      [(list) (cons c acc)]
      [(list a rst ...)
       (cond
         [(char=? a c) (cons a acc)]
         [(char-ci=? a c) rst]
         [else (cons c acc)])])))

(define (bruteforce chain)
  (define possibilities (map char-downcase (remove-duplicates chain char-ci=?)))
  (for/fold ([best #f])
            ([p possibilities])
    (define reacted (react (filter (negate (curry char-ci=? p)) chain)))
    (if (or (not best) (< (length reacted) (length best)))
        reacted
        best)))

(module+ test
  (require rackunit)
  (define-syntax-rule (check-chain input expected)
    (test-case (format "~a -> ~a" input expected)
      (check-equal? (react (string->list input))
                    (string->list expected))))
  (check-chain "aA" "")
  (check-chain "abBA" "")
  (check-chain "abAB" "abAB")
  (check-chain "aabAAB" "aabAAB")
  (check-chain "dabAcCaCBAcCcaDA"
               "dabCBAcaDA")
  (test-case "part two"
    (check-equal? (bruteforce (string->list "dabAcCaCBAcCcaDA")) (string->list "daDA"))))

(module+ main
  (define chain (string->list (read-line)))
  (length (react chain))
  (length (bruteforce chain)))
