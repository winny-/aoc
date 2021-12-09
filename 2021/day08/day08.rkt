#lang curly-fn debug racket

#|

My brain was in the box and this was confusing.  TODO revise to not be so
verbose, maybe use some more set relations...

|#

(require racket/pretty)

(define STANDARD-SEGMENTS
  (hash "0" (set 'a 'b 'c 'e 'f 'g)
        "1" (set 'c 'f)
        "2" (set 'a 'c 'd 'e 'g)
        "3" (set 'a 'c 'd 'f 'g)
        "4" (set 'b 'c 'd 'f)
        "5" (set 'a 'b 'd 'f 'g)
        "6" (set 'a 'b 'd 'e 'f 'g)
        "7" (set 'a 'c 'f)
        "8" (set 'a 'b 'c 'd 'e 'f 'g)
        "9" (set 'a 'b 'c 'd 'f 'g)))

(define UNIQUE-COUNTS
  (let ([counts (for/list ([v (in-hash-values STANDARD-SEGMENTS)]) (set-count v))])
    (for/hash ([(k v) (in-hash STANDARD-SEGMENTS)]
               #:unless (> (count #{= (set-count v) %} counts) 1))
      (values k v))))

(define (subset? the-set subset)
  (set-empty? (set-subtract subset the-set)))

(define (rassoc v alist [eq equal?])
  (for/first ([a (in-list alist)]
              #:when (eq (cdr a) v))
    a))

(define RELATIONS
  `((2 . "1")
    (3 . "7")
    (4 . "4")
    (7 . "8")
    (,(lambda (v known) (and (= 6 (set-count v))
                             (match (rassoc "1" known)
                               [#f #f]
                               [(cons a _) (subset? v a)])
                             (match (rassoc "4" known)
                               [#f #f]
                               [(cons a _) (not (subset? v a))]))) . "0")
    (,(lambda (v known) (and (= 6 (set-count v))
                             (rassoc "0" known)
                             (subset? v (match (rassoc "4" known)
                                          [#f (set)]
                                          [(cons a _) a])))) . "9")
    (,(lambda (v known) (and (= 6 (set-count v))
                             (rassoc "0" known)
                             (not (subset? v (match (rassoc "4" known)
                                               [#f (set)]
                                               [(cons a _) a]))))) . "6")
    (,(lambda (v known) (and (= 5 (set-count v))
                             (match (rassoc "7" known)
                               [#f #f]
                               [(cons a _) (subset? v a)]))) . "3")
    (,(lambda (v known) (and (= 5 (set-count v))
                             (match* ((rassoc "3" known) (rassoc "6" known))
                               [((cons three _) (cons six _))
                                (subset? v (set-intersect three six))]
                               [(_ _) #f]))) . "5")
    (,(lambda (v known) (and (= 5 (set-count v))
                             (match* ((rassoc "3" known) (rassoc "6" known))
                               [((cons three _) (cons six _))
                                (not (subset? v (set-intersect three six)))]
                               [(_ _) #f]))) . "2")))

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [(regexp #rx"([a-z ]+) \\| ([a-z ]+)" (list _ parts ...))
     (for/list ([p parts])
       (for/list ([digits (string-split p #:repeat? #t)])
         (for/set ([segment (string->list digits)])
           (string->symbol (string segment)))))]))

(define (solve patterns [max-steps +inf.0])
  (let loop ([steps 1] [acc (for/list ([p patterns])
                              (cons p #f))])
    (if (or (> steps max-steps)
            (andmap cdr acc))
        acc
        (loop
         (add1 steps)
         (for/list ([entry (in-list acc)])
           (match-define (cons p v) entry)
           (if v
               entry
               (let ([segments (set-count p)])
                 (match (assoc segments RELATIONS)
                   [#f (cons p (for/first ([relation RELATIONS]
                                           #:when (match relation
                                                    [(cons (? procedure? proc) _) (proc p acc)]
                                                    [_ #f]))
                                 (cdr relation)))]
                   [(cons _ val) (cons p val)]))))))))

(define (part1 ls)
  (for*/sum ([test ls]
             [segments (second test)]
             #:when (assoc (set-count segments) RELATIONS))
    1))


(define (part2 ls)
  (for/sum ([test ls])
    (match-define (list patterns output) test)
    (define solution (solve patterns 100))
    (string->number
     (string-join
        (for/list ([digit output])
          (or (match (assoc digit solution)
                [#f #f]
                [s (cdr s)])
              "X"))
        ""))))

(module+ main
  (define ls (port->list read2))
  ;; (pretty-print ls)
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day08.rkt < sample.txt"
;; End:
