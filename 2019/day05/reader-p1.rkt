#lang racket

(require syntax/strip-context)

(define (make-day05-readtable)
  (make-readtable (current-readtable)
                  #\, 'terminating-macro (case-lambda
                                           [(ch in) (read in)]
                                           [(ch in src line col pos) (read in)])))

(define (day05-read in)
  (syntax->datum (day05-read-syntax #f in)))

(define (day05-read-syntax src in)
  (with-syntax ([memory (parameterize ([current-readtable (make-day05-readtable)])
                          (list->vector (port->list read in)))])
    (strip-context
     #'(module day05 racket
         (require "day05.rkt")
         (void (part1 'memory))))))

(provide (rename-out [day05-read read]
                     [day05-read-syntax read-syntax]))
