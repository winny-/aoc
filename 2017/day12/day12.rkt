#lang racket

(define (read-relation [ip (current-input-port)])
  (define line (read-line ip))
  (if (eof-object? line)
      line
      (match (string-split line)
        [(list node _ edges ...) (list (string->number node)
                                       (for/set ([s edges])
                                         (string->number
                                          (regexp-replace #rx"," s ""))))])))

(define (port->relations [ip (current-input-port)])
  (for/hash ([ls (port->list read-relation ip)])
    (values (car ls) (cadr ls))))

(module+ test
  (define sample #<<EOF
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
EOF
    )
  (require rackunit)
  (test-case "reading"
    (with-input-from-file "/dev/null" (thunk (check-equal? (read-relation) eof)))
    (check-equal? (read-relation (open-input-string "1 <-> 2")) (list 1 (set 2)))
    (check-equal? (read-relation (open-input-string "1 <-> 3, 4, 5")) (list 1 (set 3 4 5)))
    (define ls (port->relations (open-input-string sample)))
    (check-equal? ls (hash 0 (set 2)
                           1 (set 1)
                           2 (set 0 3 4)
                           3 (set 2 4)
                           4 (set 2 3 6)
                           5 (set 6)
                           6 (set 4 5))))
    )

(define (find-connected relations node)
  (let loop ([visited (set)] [pending (hash-ref relations node)])
    (if (set-empty? pending)
        visited
        (let* ([a (set-first pending)]
               [p (set-rest pending)]
               [new-visited (set-add visited a)])
          (loop new-visited (set-subtract (set-union p (hash-ref relations a)) new-visited )))
        )))

(module+ test
  (test-case "connect"
    (check-equal? (find-connected (port->relations (open-input-string sample)) 0) (set 0 2 3 4 5 6))))

(define (groups relations)
  (for/set ([k (in-hash-keys relations)])
    (find-connected relations k)))

(module+ main
  (define relations (port->relations))
  (displayln (format "Part 1: ~a" (set-count (find-connected relations 0))))
  (displayln (format "Part 2: ~a" (set-count (groups relations)))))
