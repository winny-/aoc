#lang racket

(require "read.rkt")

(require sugar/cache)

;; Caching this makes part #1 run about 80% faster
(define/caching (ancestors tree node)
  (match (parent tree node)
    [#f empty-stream]
    [n (stream-cons n (ancestors tree n))]))

(define (parent tree node)
  (for/first ([(k v) (in-hash tree)]
                     #:when (set-member? (node-children v) (node-key node)))
    v))

(define (depth tree node)
  (stream-length (ancestors tree node)))

(define (lca tree n1 n2)
  (cond [(equal? (parent tree n1) n2) n2]
        [(equal? (parent tree n2) n1) n1]
        [(not (parent tree n1)) (lca tree n1 (parent tree n2))]
        [else (lca tree (parent tree n1) n2)]))

(define (part1 orbits)
  (for*/sum ([data (in-hash-values orbits)]
             [external (in-set (node-children data))])
    (depth orbits (hash-ref orbits external))))

(define (distance tree p c)
  (let loop ([acc 0] [c c])
    (if (equal? p c)
        acc
        (loop (add1 acc) (parent tree c)))))

(define (part2 orbits)
  (define you (parent orbits (hash-ref orbits 'YOU)))
  (define san (parent orbits (hash-ref orbits 'SAN)))
  (define a (lca orbits you san))
  (+ (distance orbits a you) (distance orbits a san)))

(module+ test
  (require rackunit)
#;
  (check-equal? (call-with-input-file "example.txt" port->orbits) (hasheq 'COM (node 'COM (seteq 'B))
                                                                          'B (node 'B (seteq 'G 'C))
                                                                          'G (node 'G (seteq 'H))
                                                                          'C (node 'C (seteq 'D))
                                                                          'D (node 'D (seteq 'I 'E))
                                                                          'E (node 'E (seteq 'J 'F))
                                                                          'J (node 'J (seteq 'K))
                                                                          'K (node 'K (seteq 'L))))
  (check-equal? (part1 (call-with-input-file "example.txt" port->orbits)) 42))

(module+ main
  (define orbits (port->orbits))
  (part1 orbits)
#;  (part2 orbits)
  )
