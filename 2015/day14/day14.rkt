#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Advent of code day 14 http://adventofcode.com/day/14 - by winny
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct deer (name speed running stopped)
  #:transparent
  #:guard (λ (name speed running stopped type)
            (unless (and (non-empty-string? name)
                         (exact-positive-integer? speed)
                         (exact-positive-integer? running)
                         (exact-positive-integer? stopped))
              (error (format "Bad struct ~a instantiation with \"~a\" ~a ~a ~a"
                             type
                             name speed running stopped)))
            (values name speed running stopped)))

(define/contract (read-deer [ip (current-input-port)])
  ([] [input-port?] . ->* . (or/c false? eof-object? deer?))
  (define li (read-line ip))
  (cond
    [(eof-object? li) eof]
    [(regexp-match #rx"([A-Za-z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds." li)
     => (λ (m)
          (define sp (string->number (third m)))
          (define r (string->number (fourth m)))
          (define s (string->number (fifth m)))
          (and sp r s (deer (second m) sp r s)))]
    [else #f]))

(define port->deers (curry port->list read-deer))

(define/contract (distance-travelled dr seconds)
  (deer? exact-positive-integer? . -> . exact-positive-integer?)
  (define-values (q r) (quotient/remainder seconds (+ (deer-running dr) (deer-stopped dr))))
  (+ (* q (deer-running dr) (deer-speed dr))
     (* (min r (deer-running dr)) (deer-speed dr))))

(define/contract (race deers seconds)
  ((listof deer?) exact-positive-integer? . -> . (values deer? exact-positive-integer?))
  (apply values (argmax cadr (map (λ (dr) (list dr (distance-travelled dr seconds))) deers))))

(define/contract (race2 deers seconds)
  ((listof deer?) exact-positive-integer? . -> . (listof (list/c deer? exact-nonnegative-integer?)))
  (let loop ([drs (map (curryr list 0) deers)]
             [cur 1])
    (if (> cur seconds)
        (sort drs > #:key cadr)
        (let* ([drs1 (map (λ (d) (append d (list (distance-travelled (car d) cur)))) drs)]
               [dis (caddr (argmax caddr drs1))])
          (loop (map (λ (d)
                       (list
                        (car d)
                        ((if (findf (λ (dd) (and (= (caddr dd) dis)
                                                 (equal? (car dd) (car d))))
                                    drs1)
                             add1
                             identity)
                         (cadr d))))
                     drs)
                (add1 cur))))))

(module+ test
  (require rackunit)
  (check-exn exn:fail? (thunk (deer "" 1 1 1)))
  (check-exn exn:fail? (thunk (deer "name" 0 0 0)))
  (check-exn exn:fail? (thunk (deer "name" -1 -1 -1)))
  (check-equal? (read-deer (open-input-string "")) eof)
  (check-equal? (read-deer (open-input-string "bad input :(")) #f)
  (check-equal? (read-deer (open-input-string "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."))
                (deer "Comet" 14 10 127))
  (define deers (port->deers (open-input-string #<<END
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
END
                                                )))
  (define comet (deer "Comet" 14 10 127))
  (define dancer (deer "Dancer" 16 11 162))
  (check-equal? deers (list comet dancer))
  (check-equal? (distance-travelled comet 1000) 1120)
  (check-equal? (distance-travelled dancer 1000) 1056)
  (check-equal? (call-with-values (thunk (race (list comet dancer) 1000)) list) `(,comet 1120))
  (check-equal? (race2 `(,comet ,dancer) 1000) `((,dancer 689) (,comet 312))))

(module+ main
  (define lst (port->deers))
  (if (or (eof-object? lst) (ormap false? lst))
      (displayln "Bad input or EOF. Exiting")
      (begin
        (displayln (call-with-values (thunk (race lst 2503)) (λ (deer dis)
                                                               (format "Part #1: ~a won, travelling ~a km."
                                                                       (deer-name deer) dis))))
        (displayln (let ([winner (car (race2 lst 2503))])
                     (format "Part #2: ~a won with ~a points." (deer-name (car winner)) (cadr winner)))))))

