#lang racket

(struct instruction (name arg1 arg2)
  #:transparent)

(struct state (registers address)
  #:transparent)

(define initial-state (state #hash((a . 0)
                                   (b . 0)
                                   (c . 0)
                                   (d . 0))
                             0))

(define initial-state-pt2
  (state (hash-set (state-registers initial-state) 'c 1)
         (state-address initial-state)))


(define (read-instruction ip)
  (define (f s)
    (define n (string->number s))
    (if n
        n
        (string->symbol s)))
  (define s (read-line ip))
  (if (eof-object? s)
      eof
      (let ([ls (string-split s)])
        (instruction (string->symbol (car ls))
                     (f (cadr ls))
                     (if (empty? (cddr ls))
                         #f
                         (f (caddr ls)))))))

(define (step ins st)
  (define (set-register r v)
    (state (hash-set (state-registers st) r v) (state-address st)))
  (define (resolve-arg arg)
    (if (number? arg)
        arg
        (hash-ref (state-registers st) arg)))
  (define valid-address? (between/c 0 (sub1 (length ins))))
  (define (increment-address st)
    (define new-address (add1 (state-address st)))
    (state (state-registers st) (if (valid-address? new-address) new-address #f)))
  (match (list-ref ins (state-address st))
    [(instruction 'inc r #f) (increment-address (set-register r (add1 (resolve-arg r))))]
    [(instruction 'dec r #f) (increment-address (set-register r (sub1 (resolve-arg r))))]
    [(instruction 'cpy src dst) (increment-address (set-register dst (resolve-arg src)))]
    [(instruction 'jnz test offset) (if (zero? (resolve-arg test))
                                        (increment-address st)
                                        (let ([new-address (+ (state-address st) (resolve-arg offset))])
                                          (state (state-registers st)
                                                 (if (valid-address? new-address)
                                                     new-address
                                                     #f))))]))

(define (run ins [initial-state initial-state])
  (let loop ([st initial-state] [executions 0])
    (if (state-address st)
        (loop (step ins st) (add1 executions))
        (begin (displayln executions)
                          st))))

(module+ main
  (define ins (port->list read-instruction))
  (displayln (format "Part #1: register a is ~a" (hash-ref (state-registers (run ins)) 'a)))
  (displayln (format "Part #2: register a is ~a" (hash-ref (state-registers (run ins initial-state-pt2)) 'a))))

