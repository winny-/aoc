#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advent of code 13th day http://adventofcode.com/day/13 by winny.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define family/c (hash/c string? (hash/c string? exact-integer?)))

(define/contract (port->family [ip (current-input-port)])
  ([] [input-port?] . ->* . (or/c false? family/c))
  (define lst (port->list read-relation ip))
  (if (ormap false? lst)
      #f
      (for/hash ([relata (group-by car lst)])
        (values (caar relata)
                (for/hash ([r relata])
                  (values (cadr r)
                          (caddr r)))))))

(define/contract (read-relation ip)
  (input-port? . -> . (or/c eof-object? false? (list/c string? string? exact-integer?)))
  (define line (read-line ip))
  (cond
    [(eof-object? line) eof]
    [(regexp-match #rx"([A-Za-z]+) would (gain|lose) ([0-9]+) happiness units by sitting next to ([A-Za-z]+)." line)
     => (λ (m)
          (if m
              (list (second m)
                    (fifth m)
                    ((if (equal? (third m) "gain") + -) (string->number (fourth m))))
              #f))]
    [else #f]))

(define/contract (optimize-family fam)
  (family/c . -> . (values (listof string?) exact-integer?))
  (apply values (argmax cadr (map
                              (λ (ls)
                                (list ls
                                      (summate-happiness ls fam)))
                              (permutations (hash-keys fam))))))

(define/contract (summate-happiness ls fam)
  ((listof string?) family/c . -> . exact-integer?)
  (for/sum ([a ls]
            [b (append (drop ls 1) (list (car ls)))])
    (+ (hash-ref (hash-ref fam a) b)
       (hash-ref (hash-ref fam b) a))))

(module+ test
  (require rackunit)
  (check-equal? (read-relation (open-input-string "Alice would gain 54 happiness units by sitting next to Bob.")) '("Alice" "Bob" 54))
  (check-equal? (read-relation (open-input-string "Bob would lose 7 happiness units by sitting next to Carol.")) '("Bob" "Carol" -7))
  (check-false (read-relation (open-input-string "Bad input :((((")))
  (check-equal? (read-relation (open-input-string "")) eof)
  (define fam (port->family (open-input-string #<<END
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
END
                                               )))
  (check-equal? fam #hash(("Alice" . #hash(("Bob" . 54)
                                           ("Carol" . -79)
                                           ("David" . -2)))
                          ("Bob" . #hash(("Alice" . 83)
                                         ("Carol" . -7)
                                         ("David" . -63)))
                          ("Carol" . #hash(("Alice" . -62)
                                           ("Bob" . 60)
                                           ("David" . 55)))
                          ("David" . #hash(("Alice" . 46)
                                           ("Bob" . -7)
                                           ("Carol" . 41)))))
  (check-equal? (summate-happiness '("David" "Alice" "Bob" "Carol") fam) 330)
  (check-equal? (call-with-values (thunk (optimize-family fam)) list)
                '(("David" "Carol" "Bob" "Alice") 330)))

(module+ main
  (define fam (port->family))
  (define fam2 (hash-set
                (apply hash (flatten (hash-map fam (λ (k v) (list k (hash-set v "Winny" 0))))))
                "Winny" (for/hash ([name (hash-keys fam)]) (values name 0))))
  (if (or (eof-object? fam) (false? fam))
      (displayln "Bad input :(")
      (begin
        (displayln (format "Part #1: ~a" (let-values ([(ls n) (optimize-family fam)]) n)))
        (displayln (format "Part #2: ~a" (let-values ([(ls n) (optimize-family fam2)]) n))))))
