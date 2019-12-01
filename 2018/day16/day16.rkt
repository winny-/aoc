#|
An intentionally overengineered solution for AoC 2018 Day 16.
|#
#lang racket

(require (for-syntax syntax/parse racket/syntax)
         megaparsack megaparsack/text (prefix-in m: (combine-in data/monad data/applicative data/either)))

(begin-for-syntax
  (define-syntax-class operand
    #:attributes (kind)
    #:description "Operand literals"
    (pattern (~and kind (~literal immediate)))
    (pattern (~and kind (~literal register)))
    (pattern (~and kind (~literal _)))))

(define-syntax (define-instruction stx)
  (syntax-parse stx
    #:literals (define-instruction)
    [(define-instruction name:id op-a:operand op-b:operand proc:expr)
     (with-syntax ([binding-name (format-id #'name "instruction-~a" (syntax-e #'name))]
                   [actual-a (if (equal? (syntax-e #'op-a) 'register)
                                 #'(vector-ref state a)
                                 #'a)]
                   [actual-b (if (equal? (syntax-e #'op-b) 'register)
                                 #'(vector-ref state b)
                                 #'b)])
       #`(begin
           (define (binding-name state a b c)
             (define result (proc actual-a
                                  actual-b))
             (define new-state (vector-copy state))
             (vector-set! new-state c result)
             new-state)
           (set! instructions (hash-set instructions 'name binding-name))))]))

(define instructions (hash))
(define-instruction addr register register +)
(define-instruction addi register immediate +)
(define-instruction mulr register register *)
(define-instruction muli register immediate *)
(define-instruction banr register register bitwise-and)
(define-instruction bani register immediate bitwise-and)
(define-instruction borr register register bitwise-ior)
(define-instruction bori register immediate bitwise-ior)
(define-instruction setr register _ (λ (a b) a))
(define-instruction seti immediate _ (λ (a b) a))
(define (boolean->number b)
  (if b 1 0))
(define-instruction gtir immediate register (compose1 boolean->number >))
(define-instruction gtri register immediate (compose1 boolean->number >))
(define-instruction gtrr register register (compose1 boolean->number >))
(define-instruction eqir immediate register (compose1 boolean->number =))
(define-instruction eqri register immediate (compose1 boolean->number =))
(define-instruction eqrr register register (compose1 boolean->number =))

(define (valid-opcodes the-observation)
  (match-define (struct observation (before after instruction)) the-observation)
  (for/set ([(name proc) (in-hash instructions)]
            #:when (equal? after (apply proc before (cdr instruction))))
    name))

(module+ test
  (require rackunit)
  (test-case "part1-example"
    (check-equal? (valid-opcodes (observation #(3 2 1 1) #(3 2 2 1) '(9 2 1 2)))
                  (set 'mulr 'addi 'seti))))

(define state/p
  (m:do (char/p #\[)
        [xs m:<- (list/p integer/p integer/p integer/p integer/p #:sep (string/p ", "))]
        (char/p #\])
        (m:pure (list->vector xs))))

(define instruction/p
  (m:do [xs m:<- (list/p integer/p integer/p integer/p integer/p #:sep (char/p #\space))]
        (m:pure xs)))

(define many-whitespace/p (many/p space/p))

(struct observation (before after instruction) #:transparent)

(define observation/p
  (m:do many-whitespace/p
        (string/p "Before: ")
        [before m:<- state/p]
        many-whitespace/p
        [instruction m:<- instruction/p]
        many-whitespace/p
        (string/p "After:  ")
        [after m:<- state/p]
        (m:pure (observation before after instruction))))

(module+ test
  (test-case "parse-combinators"
    (check-equal? (parse-string state/p "[1, 2, 3, 4]") (m:success #(1 2 3 4)))
    (check-equal? (parse-string instruction/p "1 2 3 4") (m:success '(1 2 3 4))))
  (test-case "parse-example"
    (define text #<<EOF
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
EOF
      )
    (check-equal? (parse-string observation/p text) (m:success (observation #(3 2 1 1) #(3 2 2 1) '(9 2 1 2))))))

(define (search obs)
  (let loop ([known (hash)] [n 1])
    (define new-known
      (for/fold ([kn known])
                ([o obs]
                 #:unless (hash-has-key? kn (car (observation-instruction o))))
        (match-define (struct* observation ([instruction (list opcode _ _ _)])) o)
        (define valid (set-subtract (valid-opcodes o) (list->set (hash-values kn))))
        (match (set-count valid)
          [0 (error 'search "No valid instruction for ~a" o)]
          [1 (hash-set kn opcode (set-first valid))]
          [_ kn])))
    (cond
      [(equal? known new-known) (error 'search "Infinite search detected n:~a known:~a new-known:~a"
                                       n known new-known)]
      [(= (hash-count instructions) (hash-count new-known)) new-known]
      [else (loop new-known (add1 n))])))

(define input/p
  (m:do
   [observations m:<- (many/p (try/p observation/p))]
   many-whitespace/p
   [program m:<- (many/p (try/p (m:do
                                 [i m:<- instruction/p]
                                 many-whitespace/p
                                 (m:pure i))))]
   (m:pure (list observations program))))

(module+ main
  (match-define (list observations program) (parse-result! (parse-string input/p (port->string))))
  #;(fprintf (current-error-port) "Read ~a observations and ~a instructions\n"
             (length observations) (length program))
  (count (λ (o) (<= 3 (set-count (valid-opcodes o)))) observations)
  (define opcodes (search observations))
  (for/fold ([state #(0 0 0 0)]
             #:result (vector-ref state 0))
            ([instruction program])
    (match-define (list opcode operands ...) instruction)
    (apply (hash-ref instructions (hash-ref opcodes opcode)) state operands)))
