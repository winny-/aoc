#lang typed/racket

(struct instruction ([name : Symbol]
                     [arg1 : (U Symbol Integer)]
                     [arg2 : (U Symbol Integer False)])
  #:transparent)

(struct state ([registers : (HashTable Symbol Integer)]
               [address : (U Integer False)])
  #:transparent)

(define initial-state (state #hash((a . 0)
                                   (b . 0)
                                   (c . 0)
                                   (d . 0))
                             0))

(define initial-state-pt2
  (state (hash-set (state-registers initial-state) 'c 1)
         (state-address initial-state)))

(: read-instruction (Input-Port . -> . (U EOF instruction)))
(define (read-instruction ip)
  (: f (String . -> . (U Symbol Integer)))
  (define (f s)
    (: n (U Complex False))
    (define n (string->number s))
    (if (exact-integer? n)
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

(: step ((Listof instruction) state . -> . state))
(define (step ins st)
  (: set-register (Symbol Integer . -> . state))
  (define (set-register r v)
    (state (hash-set (state-registers st) r v) (state-address st)))
  (: resolve-arg ((U Symbol Integer) . -> . Integer))
  (define (resolve-arg arg)
    (if (number? arg)
        arg
        (hash-ref (state-registers st) arg)))
  (: valid-address? (Integer . -> . Boolean))
  (define (valid-address? address)
    (and (>= address 0) (<= address (sub1 (length ins)))))
  (: increment-address (state . -> . state))
  (define (increment-address st)
    (define addr (state-address st))
    (state (state-registers st)
           (if (exact-integer? addr)
               (let ([n (add1 addr)])
                 (if (and (exact-integer? n) (valid-address? n))
                     n
                     #f))
               #f)))
  (define addr (state-address st))
  (if (false? addr)
      st
      (match (list-ref ins addr)
        [(instruction 'inc (and (or 'a 'b 'c 'd) r) #f) (increment-address (set-register r (add1 (resolve-arg r))))]
        [(instruction 'dec (and (or 'a 'b 'c 'd) r) #f) (increment-address (set-register r (sub1 (resolve-arg r))))]
        [(instruction 'cpy src (and (or 'a 'b 'c 'd) dst)) (increment-address (set-register dst (resolve-arg src)))]
        [(instruction 'jnz test (and (not #f) offset)) (if (zero? (resolve-arg test))
                                                           (increment-address st)
                                                           (let ([new-address (+ addr (resolve-arg (if (false? offset) (error "offset cannot be false") offset)))])
                                                             (state (state-registers st)
                                                                    (if (valid-address? new-address)
                                                                        new-address
                                                                        #f))))])))

(: run ([(Listof instruction)] [state] . ->* . state))
(define (run ins [initial-state initial-state])
  (let loop : state ([st : state initial-state] [executions 0])
    (if (state-address st)
        (loop (step ins st) (add1 executions))
        (begin (displayln executions)
               st))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax guard
  (syntax-rules ()
    [(_ a b)
     (let ()
       (unless (a b)
         (error "guard failed"))
       b)]))

(define sample-input (port->list read-instruction (open-input-string #<<EOF
cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
EOF
                                                                     )))

(module* benchmark #f
  (time (run (guard (λ (ls) (andmap instruction? ls)) sample-input))))


(module+ main
  (define ins (port->list read-instruction))
  (displayln (format "Part #1: register a is ~a" (hash-ref (state-registers (run (guard (λ (ls) (andmap instruction? ls)) ins))) 'a)))
  (displayln (format "Part #2: register a is ~a" (hash-ref (state-registers (run (guard (λ (ls) (andmap instruction? ls)) ins) initial-state-pt2)) 'a))))

