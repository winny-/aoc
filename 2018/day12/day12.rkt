#lang racket

(require racket/hash
         megaparsack megaparsack/text
         (prefix-in m: (combine-in data/monad data/applicative data/either)))

(struct state (state start) #:transparent)

(define pot/p
  (m:do
   [x m:<- (char-in/p "#.")]
   (m:pure (char=? x #\#))))

(define (pots->string ps)
  (list->string (map (λ (p) (if p #\# #\.)) ps)))

(define many-space/p (many/p space/p))

(define many-pot/p (many/p pot/p))

(define rule/p (m:do
                [lhs m:<- many-pot/p]
                (string/p " => ")
                [rhs m:<- pot/p]
                many-space/p
                (m:pure (hash lhs rhs))))

(define initial-state/p
  (m:do
   (string/p "initial state: ")
   [initial m:<- many-pot/p]
   many-space/p
   (m:pure (state initial 0))))

(define input/p
  (m:do
   [initial m:<- initial-state/p]
   [rules m:<- (many/p rule/p)]
   (m:pure (list initial (foldl hash-union (hash) rules)))))

(define (pairwise xs width)
  (let loop ([acc empty] [xs xs])
    (if (< (length xs) width)
        (reverse acc)
        (loop (cons (take xs width) acc)
              (drop xs 1)))))

(define (pad st)
  (match-define (struct state (current start)) st)
  (define new-left
    (make-list (count identity (take current 4)) #f))
  (define new-right
    (make-list (count identity (take-right current 4)) #f))
  (state (append new-left current new-right) (- start (length new-left))))


(define (age st rules)
  (match-define (struct state (pots start)) (pad st))
  (state
   (append (take pots 2)
           (for/list ([view (in-list (pairwise pots 5))])
             (hash-ref rules view (thunk* #f #;(list-ref view 2))))
           (take-right pots 2))
   start))

(define (age-n st rules n)
  (for/fold ([st st])
            ([generation (in-range 1 (+ 1 n))])
    (define new-st (age st rules))
    (printf "~a ~a\n" (state-start st) (pots->string (state-state st)))
    new-st))

(module+ test
  (require rackunit)
  (define example (call-with-input-file "example.txt" port->string))
  (define expected-rules (hash '(#f #f #f #t #t) #t
                               '(#f #f #t #f #f) #t
                               '(#f #t #f #f #f) #t
                               '(#f #t #f #t #f) #t
                               '(#f #t #f #t #t) #t
                               '(#f #t #t #f #f) #t
                               '(#f #t #t #t #t) #t
                               '(#t #f #t #f #t) #t
                               '(#t #f #t #t #t) #t
                               '(#t #t #f #t #f) #t
                               '(#t #t #f #t #t) #t
                               '(#t #t #t #f #f) #t
                               '(#t #t #t #f #t) #t
                               '(#t #t #t #t #f) #t))
  (define expected-state (state '(#t #f #f #t #f #t #f #f #t #t #f #f #f #f #f #f #t #t #t #f #f #f #t #t #t) 0))
  (test-case "pad"
    (check-equal? (pad (state '(#t #t) 0)) (state '(#f #f #t #t #f #f) -2))
    (check-equal? (pad (state '(#f #f #t #t #f) -1)) (state '(#f #f #t #t #f #f) -1)))
  (test-case "pairwise"
    (check-equal? (pairwise '(1 2 3 4 5 6 7 8) 5)
                  '((1 2 3 4 5)
                    (2 3 4 5 6)
                    (3 4 5 6 7)
                    (4 5 6 7 8))))
  (test-case "parsing"
    (check-equal? (parse-string input/p example) (m:success (list expected-state expected-rules))))
  (define expected-age '(#f #t #f #f #f #f #t #t #f #f #f #f #t #t #t #t #t
                         #f #f #f #t #t #t #t #t #t #t #f #f #f #f #t #f #t
                         #f #f #t #t #f))
  (define gen-1 (state '(#f #f #t #f #f #f #t #f #f #f #f #t #f #f #f
                         #f #f #t #f #f #t #f #f #t #f #f #t #f #f)
                         -2))
  (test-case "age"
    (check-equal? (age expected-state expected-rules)
                  gen-1)
    (check-equal? (age gen-1 expected-rules)
                  (state '(#f #f #t #t #f #f #t #t #f #f #f #t #t #f #f
                           #f #f #t #f #f #t #f #f #t #f #f #t #t #f #f)
                         -2)))
  (test-case "age-n"
    (check-equal? (age-n expected-state expected-rules 20) (state expected-age -3))))

(define (sum-state st)
  (match-define (and aged (struct state (pots start))) st)
  (for/sum ([(p idx) (in-indexed pots)]
            #:when p)
    (printf "(~a ~a) ~a ~a\n" p idx start (+ idx start))
    (+ idx start)))

(module+ main
  (match-define (list initial rules) (parse-result! (parse-string input/p (port->string))))
  (match-define (and aged (struct state (pots start))) (age-n initial rules 20))
  (displayln aged)
  (sum-state aged))

