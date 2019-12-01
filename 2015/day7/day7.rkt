#lang racket

(provide (all-defined-out))

(struct gate (op args dest)
  #:transparent)

(define operations
  (hash
   'assign identity
   'not (λ (n) (- (expt 2 16) 1 n))
   'and bitwise-and
   'or bitwise-ior
   'lshift arithmetic-shift
   'rshift (λ (n m) (arithmetic-shift n (- m)))))

(define (string->gate s)
  (define (f token)
    (let ([n (string->number token)])
      (if n n token)))
  (define lst (map f (remove "->" (string-split s " "))))
  (match lst
    [(list a dest) (gate 'assign (list a) dest)]
    [(list "NOT" a dest) (gate 'not (list a) dest)]
    [(list a "AND" b dest) (gate 'and (list a b) dest)]
    [(list a "OR" b dest) (gate 'or (list a b) dest)]
    [(list a "LSHIFT" b dest) (gate 'lshift (list a b) dest)]
    [(list a "RSHIFT" b dest) (gate 'rshift (list a b) dest)]
    [_ #f]))

(define (read-gate ip)
  (define line (read-line ip))
  (if (eof-object? line)
      line
      (string->gate line)))

(define (execute-gate g state)
  (define (get-wire x)
    (if (number? x)
        x
        (hash-ref state x)))
  (hash-set state (gate-dest g) (apply (hash-ref operations (gate-op g))
                                       (map get-wire (gate-args g)))))

(define (execute-circuit circuit [state (hash)])
  (define (deps-satisfied? g)
    (andmap (λ (x) (or (number? x) (hash-ref state x #f)))
            (gate-args g)))
  (cond
    [(null? circuit) state]
    [(deps-satisfied? (car circuit)) (execute-circuit (cdr circuit) (execute-gate (car circuit) state))]
    [else (execute-circuit (append (cdr circuit) (list (car circuit))) state)]))

(module+ main
  (hash-ref (execute-circuit (port->list read-gate)) "a"))
