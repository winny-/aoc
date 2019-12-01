#lang racket

(define/contract (zip* ls)
  ((listof list?) . -> . (listof list?))
  (apply map list ls))

(define/contract (f ls mode)
  (list? (or/c 'most 'least) . -> . any/c)
  (cdr ((if (equal? mode 'most) argmax argmin)
        car
        (map (λ (elem)
               (cons (count (curry equal? elem) ls) elem))
             (remove-duplicates ls)))))

(define/contract (most-frequent ls)
  (list? . -> . any/c)
  (f ls 'most))

(define/contract (least-frequent ls)
  (list? . -> . any/c)
  (f ls 'least))

(define (error-correct messages)
  (list->string (map most-frequent (zip* (map string->list messages)))))

(define (error-correct-part2 messages)
  (list->string (map least-frequent (zip* (map string->list messages)))))

(module+ test
  (require rackunit)
  (define messages (string-split #<<EOF
eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
EOF
                                 "\n"))
  (check-equal? (most-frequent '(3 1 3 2 3 1 3 3 1 1)) 3)
  (check-equal? (zip* '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))
  (check-equal? (error-correct messages) "easter")
  (check-equal? (error-correct-part2 messages) "advent"))

(module+ main
  (define ls (port->list read-line))
  (displayln (error-correct ls))
  (displayln (error-correct-part2 ls)))
