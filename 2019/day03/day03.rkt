#lang racket

(struct component (direction length) #:transparent)

(define (read-component-helper in)
  (match (regexp-match #rx"(R|L|U|D)([0-9]+)" in)
    [(list _ dir len) (component (match dir [#"L" 'left] [#"R" 'right] [#"U" 'up] [#"D" 'down])
                                 (string->number (bytes->string/utf-8 len)))]
    [a (raise-syntax-error 'read "Bad syntax")]))

(define read-component
  (case-lambda
    [(ch in) (read-component-helper in)]
    [(ch in src line col pos) (read-component-helper in)]))

(define (make-day3-readtable)
  (make-readtable (current-readtable)
                  #\, #\space #f
                  #\R 'terminating-macro read-component
                  #\L 'terminating-macro read-component
                  #\U 'terminating-macro read-component
                  #\D 'terminating-macro read-component))
