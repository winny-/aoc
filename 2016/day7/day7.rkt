;; just part 1 aoc day 7 2016

#lang racket

(define (read-ip7 ip)
  (let l ([regular '()] [hyper '()] [acc '()] [in-bracket #f])
    (match (read-char ip)
      [(or (? eof-object?) #\newline)
       (when in-bracket
         (error 'no-closing-bracket))
       (if (and (empty? regular) (empty? hyper) (empty? acc))
           eof
           (list (reverse (if (empty? acc)
                              regular
                              (cons (list->string (reverse acc)) regular)))
                 (reverse hyper)))]
      [#\[ (when in-bracket
             (error 'nested-brackets-not-allowed))
           (l (if (empty? acc) regular (cons (list->string (reverse acc)) regular)) hyper '() #t)]
      [#\] (unless in-bracket
             (error 'extra-closing-bracket))
           (when (empty? acc)
             (error 'empty-hypernet-sequence-pair))
           (l regular (cons (list->string (reverse acc)) hyper) '() #f)]
      [c (l regular hyper (cons c acc) in-bracket)])))

(define (abba-component? s)
  (let l ([ls (string->list s)])
    (match ls
      [(list a (and b (not a)) b a rst ...) #t]
      [(list a rst ..4) (l rst)]
      [_ #f])))

(define (find-aba ls)
  (let l ([ls ls] [acc '()])
    (match ls
      [(list a (and b (not a)) a rst ...) (l (append (list b a) rst) (cons (list a b a) acc))]
      [(list a rst ..2) (l rst acc)]
      [_ (reverse acc)])))

(define (tls? ip7)
  (and (andmap (negate abba-component?) (cadr ip7))
       (ormap abba-component? (car ip7))))


(define (ssl? ip7)
  (define (f b acc)
    (append acc (find-aba (string->list b))))
  (define aba (foldl f '() (car ip7)))
  (define bab (foldl f '() (cadr ip7)))
  (ormap (match-lambda [(list a b a) (ormap (curry equal? (list b a b)) bab)])
         aba))

(module+ test
  (require rackunit)
  (check-equal? (read-ip7 (open-input-string "abba[mnop]qrst")) '(("abba" "qrst") ("mnop")))
  (check-true (tls? '(("abba" "qrst") ("mnop"))))
  (check-false (tls? '(("abcd" "xyyx") ("bddb"))))
  (check-false (tls? '(("aaaa" "tyui") ("qwert"))))
  (check-true (tls? '(("ioxxoj" "zxcvbn") ("asdfgh"))))

  (check-equal? (find-aba (string->list "zazbz")) '((#\z #\a #\z) (#\z #\b #\z)))
  (check-true (ssl? '(("aba" "xyz") ("bab"))))
  (check-false (ssl? '(("xyx" "xyx") ("xyx"))))
  (check-true (ssl? '(("aaa" "eke") ("kek"))))
  (check-true (ssl? '(("zazbz" "cdb") ("bzb")))))

(module+ main
  (define ls (port->list read-ip7))
  (displayln (format "~a ip7 addresses read" (length ls)))
  (displayln (format "~a have TLS" (count tls? ls)))
  (displayln (format "~a have SSL" (count ssl? ls))))
