#lang debug racket

(require (only-in data/collection chunk))

(define (read-layer width height [in (current-input-port)])
  (match (read-string (* width height) in)
    [(? string? s) #:when (= (string-length s) (* width height))
                   (map (compose1 string->number string) (string->list s))]
    [_ eof]))

(define (read-image width height [in (current-input-port)])
  (port->list (curry read-layer width height) in))

(define (part1 im)
  (define layer (argmin (curry count zero?) im))
  (* (count (curry = 1) layer)
     (count (curry = 2) layer)))

(define (display-layer la width)
  (for ([line (chunk width la)])
    (displayln (string-replace (sequence-fold string-append "" (sequence-map number->string line)) "0" " "))))

(define (part2 im)
  (for/fold ([acc (car im)])
            ([layer (cdr im)])
    (map (match-lambda** [(2 v) v] [(v _) v]) acc layer)))

(module+ main
  (define im (read-image 25 6))
  (part1 im)
  (display-layer (part2 im) 25))
