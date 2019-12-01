#lang racket

(module+ test
  (require rackunit))

(define (part1-step ls index)
  (values (list-update ls index add1)
          (+ index (list-ref ls index))))

(module+ test
  (test-case "part1-step"
    (define tests
      '((((0 3 0 1 -3) 0)
         ((1 3 0 1 -3) 0))

        (((2 3 0 1 -3) 1)
         ((2 4 0 1 -3) 4))

        (((2 4 0 1 -3) 4)
         ((2 4 0 1 -2) 1))

        (((2 4 0 1 -2) 1)
         ((2 5 0 1 -2) 5))))
    (for ([t tests])
      (check-equal? (call-with-values (thunk (apply part1-step (car t))) list) (cadr t) (~a t)))))

(define (part1 ls)
  (let loop ([ls ls] [index 0] [n 0])
    (if (not (< index (length ls)))
        n
        (let-values ([(nls nindex) (part1-step ls index)])
          (loop nls nindex (add1 n))))))

(module+ test
  (test-case "part1"
    (check-equal? (part1 '(0 3 0 1 -3)) 5)))

(module+ main
  (define ls (map string->number (port->list read-line)))
  (displayln (format "Part 1: ~a" (part1 ls))))
