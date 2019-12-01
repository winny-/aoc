#lang racket

(define (read-passphrase ip)
  (match (read-line)
    [(? eof-object?) eof]
    [a (string-split a)]))

(define (valid1? passphrase)
  (for/and ([p passphrase])
    (= 1 (count (curry string=? p) passphrase))))

(define (part1 passphrases)
  (count valid1? passphrases))

(define (valid2? passphrase)
  (valid1? (map (compose1 list->string (curryr sort char<?) string->list) passphrase)))

(define (part2 passphrases)
  (count valid2? passphrases))

(module+ main
  (define passphrases (port->list read-passphrase))
  (displayln (format "Part 1: ~a" (part1 passphrases)))
  (displayln (format "Part 2: ~a" (part2 passphrases))))
