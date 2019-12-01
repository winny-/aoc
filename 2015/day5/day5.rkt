#lang racket

;; For http://adventofcode.com/day/5

(provide (all-defined-out))

(define/contract (has-3+-vowels? s)
  (-> string? boolean?)
  (define lst (string->list s))
  (define total (for/sum ([vowel (string->list "aeiou")])
                  (length (filter-map (curry equal? vowel) lst))))
  (>= total 3))

(define/contract (has-double? s)
  (-> string? boolean?)
  (define unique (remove-duplicates (string->list s)))
  (not (empty? (filter (lambda (c) (string-contains? s (make-string 2 c)))
                       unique))))

(define/contract (no-bad-strings? s)
  (-> string? boolean?)
  (empty? (filter (curry string-contains? s) '("ab" "cd" "pq" "xy"))))

(define/contract (validate-string s)
  (-> string? boolean?)
  (and (has-3+-vowels? s)
       (has-double? s)
       (no-bad-strings? s)))

(module+ main
  (length (filter validate-string (port->list read-line))))
