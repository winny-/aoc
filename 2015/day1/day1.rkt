#lang racket

(define (f2n c)
  (match c
    [#\( 1]
    [#\) -1]
    [_ 0]))

(define (read-floor input-port)
  (let ([char (read-char input-port)])
    (if (eof-object? char)
        char
        (f2n char))))

(define (follow lst value #:index [index 0] #:accumulator [accumulator 0])
  (if (null? lst)
      #f
      (begin
        (set! accumulator (+ (car lst) accumulator))
        (if (equal? accumulator value)
            index
            (follow (cdr lst) value #:index (add1 index) #:accumulator accumulator)))))


(module+ main
  (define lst (port->list read-floor))
  (displayln (format "Santa stopped floor ~a" (apply + lst)))
  (displayln (format "Santa first reaches the basement at position ~a" (add1 (follow lst -1)))))
