#lang racket

(struct point (x y)
  #:transparent)

(define (read-directions ip)
  (define line (read-line ip))
  (if (eof-object? line)
      eof
      (map (compose string->symbol string) (string->list line))))

(define (add-point a b)
  (point (+ (point-x a) (point-x b))
         (+ (point-y a) (point-y b))))

(define direction-translations
  `#hash((U . ,(point 0 -1))
         (D . ,(point 0 1))
         (L . ,(point -1 0))
         (R . ,(point 1 0))))

(define points
  (hash
   (point 0 0) 1
   (point 1 0) 2
   (point 2 0) 3
   (point 0 1) 4
   (point 1 1) 5
   (point 2 1) 6
   (point 0 2) 7
   (point 1 2) 8
   (point 2 2) 9))

(define points-part2
  (hash
   (point 2 0) 1
   (point 1 1) 2
   (point 2 1) 3
   (point 3 1) 4
   (point 0 2) 5
   (point 1 2) 6
   (point 2 2) 7
   (point 3 2) 8
   (point 4 2) 9
   (point 1 3) 'A
   (point 2 3) 'B
   (point 3 3) 'C
   (point 2 4) 'D))


(define (move pt dir [mapping points])
  (define new-pt (add-point pt (hash-ref direction-translations dir)))
  (if (hash-ref mapping new-pt #f)
      new-pt
      pt))

(define (enter directions pt [mapping points])
  (foldl (λ (dir pt)
           #;(displayln (format "~a ~a" dir pt))
           (move pt dir mapping))
         pt
         directions))

(define (enter-many directions [mapping points])
  (let loop ([directions directions] [acc (list (point 1 1))])
    (if (empty? directions)
        (cdr (reverse acc))
        (loop (cdr directions)
              (cons (enter (car directions) (car acc) mapping)
                    acc)))))

(module+ test
  (require rackunit)
  (define directions-string #<<EOF
ULL
RRDDD
LURDL
UUUUD
EOF
    )
  (define directions-list
    '((U L L)
      (R R D D D)
      (L U R D L)
      (U U U U D)))
  (define expected-points
    (list (point 0 0)
          (point 2 2)
          (point 1 2)
          (point 1 1)))
  (check-equal? (port->list read-directions (open-input-string directions-string))
                directions-list)
  (for ([t `(((,(point 0 0) L) . ,(point 0 0))
             ((,(point 0 0) U) . ,(point 0 0))
             ((,(point 2 2) R) . ,(point 2 2))
             ((,(point 2 2) D) . ,(point 2 2)))])
    (check-equal? (apply move (car t)) (cdr t)))
  (for ([dir directions-list]
        [starts (list (point 1 1) (point 0 0) (point 2 2) (point 1 2))]
        [expected expected-points])
    #;(displayln (format "... ~a ..." starts))
    (check-equal? (enter dir starts) expected (format "(enter ~a ~a) -> ~a" dir starts expected)))
  (check-equal? (enter-many directions-list) expected-points))

(module+ main
  (define ls (port->list read-directions))
  (displayln (format "Part 1: ~a"
                     (string-join (map (compose number->string (curry hash-ref points))
                                       (enter-many ls points)) "")))
  (displayln (format "Part 2: ~a"
                     (string-join (map (λ (pt)
                                         (define v (hash-ref points-part2 pt))
                                         (if (symbol? v)
                                             (symbol->string v)
                                             (number->string v)))
                                       (enter-many ls points-part2))
                                  ""))))

