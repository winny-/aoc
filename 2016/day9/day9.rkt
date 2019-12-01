#lang racket

#|
Unit tests pass. However I can't seem to get filter-whitespace and decompress to cooperate.

THIS works: tr -d '[:blank:]' < input.txt | racket -e '(require "day9.rkt") (decompress (current-input-port) (current-output-port))'

However when I run the (decompress (filter-whitespace (current-input-port)) (current-output-port)) [in the main submodule] it never exists.
Do this with racket day9.rkt < input.txt
|#

(provide decompress
         filter-whitespace)

(define (decompress ip op)
  (let l ([state #f])
    (if state
        (let ([b (read-bytes (car state) ip)])
          (for ([_ (in-range (cdr state))])
            (write-bytes b op))
          (displayln "B1" (current-error-port))
          (l #f))
        (match (regexp-match #px#"\\(([0-9]+)x([0-9]+)\\)" ip 0 #f op)
          [(list _ n m)
           (displayln "B2" (current-error-port))
           (l (cons (string->number (bytes->string/utf-8 n))
                    (string->number (bytes->string/utf-8 m))))]
          [#f (void)]))))

(define (filter-whitespace ip)
  (define (f b v)
    (if (exact-nonnegative-integer? v)
        (let ([old-length (bytes-length b)]
              [new-b (regexp-replace* #px#"[[:space:]]+" b #"")])
          (bytes-fill! b 0)
          (bytes-copy! b 0 new-b)
          (- v (- old-length (bytes-length new-b))))
        v))
  (filter-read-input-port ip
                          (λ (b v)
                            (f b v))
                          (λ (b k e v)
                            (f b v))))


(module+ test
  (require rackunit)
  (for ([t '(("ADVENT" . "ADVENT")
             ("A(1x5)BC" . "ABBBBBC")
             ("(3x3)XYZ" . "XYZXYZXYZ")
             ("A(2x2)BCD(2x2)EFG" . "ABCBCDEFEFG")
             ("(6x1)(1x3)A" . "(1x3)A")
             ("X(8x2)(3x3)ABCY" . "X(3x3)ABC(3x3)ABCY")
             ("A(2x3)B" . "ABBB"))])
    (define ip (open-input-string (car t)))
    (define op (open-output-string))
    (decompress ip op)
    (check-equal? (get-output-string op) (cdr t))
    (check-equal? eof (read ip)))
  (define ip1 (open-input-string "a b c d e\nf\t\t\t"))
  (define op1 (open-output-string))
  (define ip2 (filter-whitespace ip1))
  (copy-port ip2 op1)
  (check-equal? (get-output-string op1) "abcdef")
  (check-equal? eof (read ip2)))

(module+ main
  (decompress (filter-whitespace (current-input-port)) (current-output-port))
  #;(decompress (current-input-port) (current-output-port)))
