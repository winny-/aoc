#lang racket

(struct claim (id left top width height) #:transparent)

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

(define (read-claim [ip (current-input-port)])
  (match (regexp-match #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" ip)
    [(list _ id left-disp top-disp width height)
     (claim (bytes->number id)
            (bytes->number left-disp) (bytes->number top-disp)
            (bytes->number width) (bytes->number height))]
    [_ eof]))

(port->list read-claim)
