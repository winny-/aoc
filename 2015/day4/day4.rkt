#lang racket

;; For http://adventofcode.com/day/4

(require openssl/md5)

(define/contract (mine-adventcoin secret n)
  (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)
  (define in (open-input-string (string-append secret (number->string n))))
  (if (string-prefix? (md5 in) "00000")
      n
      (mine-adventcoin secret (add1 n))))

(mine-adventcoin (string-trim (port->string)) 0)
