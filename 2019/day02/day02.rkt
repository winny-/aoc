#lang racket

(define (make-day2-readtable)
  (make-readtable (current-readtable)
                  #\, 'terminating-macro (case-lambda
                                           [(ch in) (read in)]
                                           [(ch in src line col pos) (read in)])))
(current-readtable (make-day2-readtable))

(define (port->program [in (current-input-port)])
  (list->vector (port->list read in)))

(define (run vec [a #f] [b #f])
  (define v (vector-copy vec))
  (when a
    (vector-set! v (car a) (cdr a)))
  (when b
    (vector-set! v (car b) (cdr b)))
  (let loop ([v (vector-copy v)] [pos 0])
    (define-values (le ri) (vector-split-at v pos))
    (match ri
      [(or (vector 99 _ ...)) v]
      [(vector (and op (or 1 2)) a b c _ ...)
       (vector-set! v c ((match op [1 +] [2 *])
                         (vector-ref v a) (vector-ref v b)))
       (loop (vector-copy v) (+ 4 pos))]
      [(vector a _ ...) (raise-user-error 'part1 "Invalid opcode ~a at position ~a" a pos)]
      [(vector) (raise-user-error 'part1 "Ran out of instructions")])))

(define (part1 vec)
  (define v (vector-copy vec))
  (vector-ref (run v '(1 . 12) '(2 . 2)) 0))

(define (part2 vec)
  (for*/first ([noun (in-range 0 100)]
               [verb (in-range 0 100)]
               #:when (with-handlers ([exn:fail:user? (const #f)])
                          (= 19690720 (vector-ref (run vec (cons 1 noun) (cons 2 verb)) 0))))
    (+ (* 100 noun) verb)))

(module+ main
  (define v (port->program))
  (part1 v)
  (part2 v))
