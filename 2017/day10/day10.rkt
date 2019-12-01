#lang racket

(require sugar)
(require (prefix-in coll: data/collection))

(define (subreverse-cycle ls start num)
  (when (>= start (length ls))
    (set! start (modulo start (length ls))))
  (define-values (a b) (split-at (shift-cycle ls (- start)) num))
  (shift-cycle (append (reverse a) b) start))

(define (hash numbers lengths [position 0] [skip-size 0])
  (let loop ([numbers numbers] [lengths lengths] [position 0] [skip-size 0])
    (if (empty? lengths)
        (values numbers position skip-size)
        (loop (subreverse-cycle numbers position (car lengths))
              (cdr lengths)
              (+ skip-size position (car lengths))
              (add1 skip-size)))))

(define (part1 line)
  (define-values (n p s) (hash (range 256) (map string->number (string-split line ",")) 0 0))
  (* (car n) (cadr n)))

(define (sparse-hash->string sh)
  (string-join
   (map (λ (n)
          (define s (number->string n 16))
          (if (= 1 (string-length s))
              (string-append "0" s)
              s))
        (for/list ([block (coll:chunk* 16 sh)])
          (for/fold ([a (coll:first block)])
                    ([b (coll:rest block)]
                     #:when (not (zero? b)))
            (bitwise-xor a b))))
   ""))

(define (part2 line)
  (define lengths
    (append (map char->integer (string->list (regexp-replace #px"\\s" line "")))
            '(17 31 73 47 23)))
  (displayln lengths)
  (let loop ([n (range 256)]
             [p 0]
             [s 0]
             [rounds 0])
    (if (> rounds 64)
        (sparse-hash->string n)
        (let-values ([(n1 p1 s1) (hash n lengths p s)])
          (loop n1 p1 s1 (add1 rounds)))))
  #;(let-values loop ([(n p s) (values (range 256) 0 0)]
                    [(rounds) 0])
              (if (> rounds 64)
                  n
                  (loop (hash n lengths p s) (add1 rounds)))))


(module+ main
  (define input (read-line))
  (displayln (format "Part 1: ~a" (part1 input))))
