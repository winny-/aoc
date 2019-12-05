#lang debug racket

(require srfi/2)

(provide part1 part2)

(struct state (memory position halted?) #:transparent)
(struct instruction (opcode operands code) #:transparent)

(define part1-instructions
  (list (instruction 99 '(pos) (const 'halt))
        (instruction 1 '(in in out) +)
        (instruction 2 '(in in out) *)
        (instruction 3 '(out) read)
        (instruction 4 '(in) displayln)))

(define part2-instructions
  (append part1-instructions
          (list (instruction 5 '(in in pos) (λ (a b pos)
                                              (if (zero? a) 'next b)))
                (instruction 6 '(in in pos) (λ (a b pos)
                                              (if (zero? a) b 'next)))
                (instruction 7 '(in in out) (λ (a b)
                                              (if (< a b) 1 0)))
                (instruction 8 '(in in out) (λ (a b)
                                              (if (= a b) 1 0))))))

(define (decode num)
  (let*-values ([(q opcode) (quotient/remainder num 100)]
                [(q1 a-mode) (quotient/remainder q 10)]
                [(q2 b-mode) (quotient/remainder q1 10)]
                [(q3 c-mode) (quotient/remainder q2 10)])
    (values opcode a-mode b-mode c-mode)))

(define (run memory instructions)
  (define lookup (for/hash ([i instructions]) (values (instruction-opcode i) i)))
  (let loop ([st (state (vector-copy memory) 0 #f)])
   ;; #R st
    (match-define (struct state (memory position halted?)) st)
    (if halted?
        st
        (let-values ([(opcode a-mode b-mode c-mode) (decode (vector-ref memory position))])
          (define modes (list a-mode b-mode c-mode))
          (match (hash-ref lookup opcode #f)
            [#f (raise-user-error 'run "Invalid opcode ~a at position ~a" opcode position)]
            [(struct instruction (_ flags proc))
             (define indexed-flags (for/fold ([acc empty]
                                              [n 0] #:result (reverse acc))
                                             ([f flags])
                                     (match f
                                       [(and f (or 'in 'out)) (values (cons (list f n) acc) (add1 n))]
                                       [other (values (cons other acc) n)])))
             (define args (for/list ([f indexed-flags] #:unless (and (list? f) (eq? (car f) 'out)))
                            (match f
                              [(list 'in idx)
                               (define parameter (vector-ref memory (+ 1 position idx)))
                               (match (list-ref modes idx) [0 (vector-ref memory parameter)] [1 parameter])]
                              ['pos position])))
             (define ret (apply proc args))
             (define next-position (if (or (symbol? ret) (not (member 'pos flags)))
                                       (+ position 1 (count (λ (f) (not (eq? f 'pos))) flags))
                                       ret))
             (and-let* ([out (findf (match-lambda [(list 'out _) #t] [_ #f]) indexed-flags)]
                        [idx (vector-ref memory (+ position 1 (cadr out)))])
                       (vector-set! memory idx ret))
             (loop (state (vector-copy memory) next-position (eq? ret 'halt)))])))))

(define (part1 memory)
  (run memory part1-instructions))

(define (part2 memory)
  (run memory part2-instructions))
