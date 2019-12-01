#lang racket

(require (prefix-in c: data/collection))

(define (read-lengths ip)
  (define line (read-line ip))
  (if (eof-object? line)
      eof
      (map string->number (string-split line #px"[[:blank:]]+"))))

(define (triangle? ls)
  (define sorted (reverse (sort ls <)))
  (> (+ (cadr sorted) (caddr sorted))
     (car sorted)))

(module+ test
  (require rackunit)
  (define s #<<EOF
  883  357  185
  572  189  424
  842  206  272

EOF
    )
  (define ls
    '((883 357 185)
      (572 189 424)
      (842 206 272)))
  (check-false (triangle? '(5 25 10)))
  (check-equal? (port->list read-lengths (open-input-string s)) ls))

(module+ main
  (define ls (port->list read-lengths))
  (displayln (format "Number of valid triangles (part 1): ~a" (length (filter triangle? ls))))
  (define ls1 (sequence->list (c:map sequence->list (c:chunk* 3 ls))))
  (define ls-part2 (append* (map (λ (e)
                                   (apply (curry map list) e))
                                 ls1)))
  (displayln (format "Number of valid triangles (part 2): ~a"
                     (length (filter triangle? ls-part2)))))
