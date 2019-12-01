#lang racket
;; For http://adventofcode.com/day/2

(define (calculate-wrapping-paper lst)
  (define areas (map (curry apply *) (combinations lst 2)))
  (apply + (cons (argmin identity areas)
                 (map (curry * 2) areas))))

(define (calculate-ribbon lst)
  (define largest (apply max lst))
  (apply + (cons (apply * lst)
                 (map (curry * 2) (remove (apply max lst) lst)))))

(define (read-dimmensions input-port)
  (define line (read-line input-port))
  (if (eof-object? line)
      line
      (map string->number (string-split line "x"))))

(module+ main
  (define boxes (port->list read-dimmensions))
  (displayln (format "Santa's elves need ~a square feet of wrapping paper."
                     (for/sum ([b boxes]) (calculate-wrapping-paper b))))
  (displayln (format "Santa's elves need ~a feet of ribbon."
                     (for/sum ([b boxes]) (calculate-ribbon b)))))
