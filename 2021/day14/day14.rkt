#|

This was a bit tricky, I tried abusing multiprocessing (it's called "places" in
Racket) to speed up computation, however it appeared the main cost is appending
the string or something like that.

Not sure if "bigram" is the right word, as I think that means "pairs of words".
Maybe I just means "pairs of letters".  Anyway, I mean that!

My original implementation can be found in the day14-original.rkt .  It might
not run.

|#

#lang racket

(require racket/hash)

(struct Input (polymer insertions) #:prefab)
(define polymer/c  (hash/c string? exact-nonnegative-integer?))
(define insertions/c (hash/c string? string?))

(define/contract (string->bigrams s)
  (string? . -> . polymer/c)
  (define xs (in-string s))
  (for/fold ([acc (hash)])
            ([p xs]
             [q (sequence-tail xs 1)])
    (hash-update acc (string p q) add1 (const 0))))

(define/contract (step polymer insertions)
  (polymer/c insertions/c . -> . polymer/c)
  (for/fold ([acc (hash)])
            ([(bigram cnt) (in-hash polymer)])
    (match (hash-ref insertions bigram (const #f))
      [#f (hash-update acc bigram (curry + cnt) (const 0))]
      [insert (hash-union
               acc
               (hash (string-append (substring bigram 0 1) insert) cnt)
               (hash (string-append insert (substring bigram 1 2)) cnt)
               #:combine +)])))

(define (read2 [ip (current-input-port)])
  (let loop ([template #f] [insertions (hash)])
    (match (read-line ip)
      [(? eof-object? e) (Input (string->bigrams template) insertions)]
      ["" (loop template insertions)]
      [(regexp "^[A-Z]+$" (list the-template))
       (loop the-template insertions)]
      [(regexp "([A-Z]+) -> ([A-Z]+)" (list _ source dest))
       (loop template (hash-set insertions source dest))])))

(define (bigrams->letters bigrams)
  (polymer/c . -> . (hash/c string? exact-nonnegative-integer?))
  (for/fold ([acc (hash)])
            ([(bigram cnt) (in-hash bigrams)])
    (hash-union acc
                (hash (substring 0 1) cnt)
                (hash (substring 1 2) cnt)
                #:combine +)))

(define (letters bigrams)
  (define ret   (for/fold ([acc (hash)])
                          ([(k v) (in-hash bigrams)])
                  (hash-union acc
                              (hash (substring k 0 1) v)
                              (hash (substring k 1 2) v)
                              #:combine +)))
  (for/hash ([(k v) (in-hash ret)])
    (values k (ceiling (/ v 2))))
  )

(define (bigram-length bigrams)
  (add1
   (for/sum ([v (in-hash-values bigrams)])
     v)))

(define (fold/hash h start proc)
  (for/fold ([acc start])
            ([(k v) (in-hash h)])
    (proc acc k v)))

(define (go polymer insertions iterations)
  (define ret
    (for/fold ([acc polymer])
              ([x (in-range 1 (add1 iterations))])
      (step acc insertions)))
  (define lets (letters ret))
  (define most (fold/hash lets #f (match-lambda*
                                    [(list #f k v) (cons k v)]
                                    [(list (and o (cons ok ov)) k v)
                                     (if (> v ov) (cons k v) o)])))
  (define least (fold/hash lets #f (match-lambda*
                                     [(list #f k v) (cons k v)]
                                     [(list (and o (cons ok ov)) k v)
                                      (if (<= v ov) (cons k v) o)])))
  (- (cdr most) (cdr least)))

(define (part1 input)
  (go (Input-polymer input) (Input-insertions input) 10))

(define (part2 input)
  (go (Input-polymer input) (Input-insertions input) 40))

(module+ main
  (define input (read2))
  (part1 input)
  (part2 input))

;; Local Variables:
;; compile-command: "racket day14.rkt < sample.txt"
;; End:
