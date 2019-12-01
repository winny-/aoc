#lang racket

(module+ test
  (require rackunit))

(define (read-garbage ip)
  (match (peek-char ip)
    [(? eof-object?) eof]
    [(and c (not #\<)) (error 'read-garbage "Expected first char to be `<', got ~a" c)]
    [_
     (read-char ip)
     (let loop ([garbage '()])
       (match (read-char ip)
         [(? eof-object?) (error 'read-garbage "Got eof early")]
         [#\! (read-char ip) (loop garbage)]
         [#\> (list->string (reverse garbage))]
         [c (loop (cons c garbage))]))]))

(module+ test
  (test-case "read-garbage"
    (for ([t '(("<>" "")
               ("<random characters>" "random characters")
               ("<<<<>" "<<<")
               ("<{!>}>" "{}")
               ("<!!>" "")
               ("<!!!>>" "")
               ("<{o\"i!a,<{i<a>" "{o\"i,<{i<a"))])
      (check-equal? (read-garbage (open-input-string (car t))) (cadr t)))))

(define (read-group [ip (current-input-port)])
  (match (peek-char ip)
    [(? eof-object?) eof]
    [(and c (not #\{)) (error 'read-group "Expected first char to be `{', got ~a" c)]
    [_
     (read-char ip)
     (let loop ([group '()])
       (define c (peek-char ip))
       (match c
         [#\} (read-char ip) (reverse group)]
         [#\{ (loop (cons (read-group ip) group))]
         [#\< (loop (cons (read-garbage ip) group))]
         [#\, (read-char ip) (loop group)]))]))

(module+ test
  (test-case "read-group"
    (for ([t '(("{}" ())
               ("{{{}}}" ((())))
               ("{{},{}}" (() ()))
               ("{{{},{},{{}}}}" ((() () (()))))
               ("{<{},{},{{}}>}" ("{},{},{{}}"))
               ("{<a>,<a>,<a>,<a>}" ("a" "a" "a" "a"))
               ("{{<a>},{<a>},{<a>},{<a>}}" (("a") ("a") ("a") ("a")))
               ("{{<!>},{<!>},{<!>},{<a>}}" (("},{<},{<},{<a"))))])
      (check-equal? (read-group (open-input-string (car t))) (cadr t)))))

(define (score-group group)
  (let loop ([group group] [n 1])
    (+ n
       (for/sum ([g group]
                 #:when (list? g))
         (loop g (add1 n))))))

(module+ test
  (test-case "score-group"
    (for ([t '(("{}" 1)
               ("{{{}}}" 6)
               ("{{},{}}" 5)
               ("{{{},{},{{}}}}" 16)
               ("{<a>,<a>,<a>,<a>}" 1)
               ("{{<ab>},{<ab>},{<ab>},{<ab>}}" 9)
               ("{{<!!>},{<!!>},{<!!>},{<!!>}}" 9)
               ("{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))])
      (check-equal? (score-group (read-group (open-input-string (car t)))) (cadr t) (~a t)))))

(define (count-garbage group)
  (for/sum ([i group])
    (if (list? i)
        (count-garbage i)
        (string-length i))))

(module+ main
  (define group (read-group))
  (displayln (format "Part 1: ~a" (score-group group)))
  (displayln (format "Part 2: ~a" (count-garbage group))))
