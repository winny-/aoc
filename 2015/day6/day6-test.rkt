#lang typed/racket

(require
  typed/rackunit
  math/array
  "day6.rkt")

(check-equal? (string->instruction "turn on 0,0 through 999,999")
              (instruction 'on (cons 0 0) (cons 999 999)))
(check-equal? (string->instruction "toggle 0,0 through 999,0")
              (instruction 'toggle (cons 0 0) (cons 999 0)))
(check-equal? (string->instruction "turn off 499,499 through 500,500")
              (instruction 'off (cons 499 499) (cons 500 500)))
(check-false (string->instruction "bomb the bejesus out of those forces"))
(check-false (string->instruction "do nothing on 0,0 through 1,1"))

(define empty-grid (make-grid #f))

(check-equal? (array-shape empty-grid) #(1000 1000))
(check-equal? (array-ref empty-grid #(0 0)) #f)


(check-equal? (sum-grid (make-grid #t) #t) 1000000)
(check-equal? (sum-grid empty-grid #t) 0)

(check-equal? (play-lightshow '()) empty-grid)


(check-equal? (execute-instruction (instruction 'on (cons 0 0) (cons 999 999)) (make-grid))
              (make-grid #t))

