#lang debug racket

(require memo)

(struct Input (polymer insertions) #:transparent)

(define (read2 [ip (current-input-port)])
  (let loop ([template #f] [insertions (hash)])
    (match (read-line ip)
      [(? eof-object? e) (Input template insertions)]
      ["" (loop template insertions)]
      [(regexp "^[A-Z]+$" (list the-template))
       (loop the-template insertions)]
      [(regexp "([A-Z]+) -> ([A-Z]+)" (list _ source dest))
       (loop template (hash-set insertions source dest))])))

(define/memoize (step input) #:hash hash
  (match-define (struct Input (polymer insertions)) input)
  (define seq (sequence-map string (in-string polymer)))
  (for/fold ([acc (substring polymer 0 1)]
             #:result (Input acc insertions))
            ([p seq]
             [q (sequence-tail seq 1)])
    (string-append acc
                   (hash-ref insertions (string-append p q) (const ""))
                   q)))

(define (histogram seq)
  (for/fold ([hist (hash)])
            ([v seq])
    (hash-update hist v add1 (const 0))))

(define (part1_b input)
  (go input 10))

(define (part1 input)
  (define final
    (for/fold ([acc input])
              ([n (in-range 1 (add1 10))])
      (match-define (and stepped (struct Input (poylmer _))) (step acc))
      ;; (printf "~a: ~a\n" n poylmer)
      stepped))
  (define hist (histogram (in-string (Input-polymer final))))
  (- (argmax identity (hash-values hist))
     (argmin identity (hash-values hist))))

(define memo (hash))

(define (go input n)
  (match-define (struct Input (polymer insertions)) input)
  (define seq (sequence-map string (in-string polymer)))
  (define hist (histogram (in-string (for/fold ([s input]
                                                #:result (histogram (in-string s)))
                                               ([p seq]
                                                [q (sequence-tail seq 1)])
                                       (step (Input (string-append p q) insertions))))))
  (- (argmax identity (hash-values hist))
     (argmin identity (hash-values hist)))
  )

(define (part2 input)
  (define final
    (for/fold ([acc input])
              ([n (in-range 1 (add1 40))])
      (match-define (and stepped (struct Input (poylmer _))) (step acc))
      (printf "Step ~a\n" n)
      stepped))
  (define hist (histogram (in-string (Input-polymer final))))
  (- (argmax identity (hash-values hist))
     (argmin identity (hash-values hist)))
  
  #;(go input 40)
  )

(module+ main
  (define input (read2))
  (part1 input)
  (part2 input))

;; Local Variables:
;; compile-command: "racket day14.rkt < sample.txt"
;; End:
