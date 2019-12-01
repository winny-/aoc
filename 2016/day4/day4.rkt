#lang racket

(struct room (name sector checksum)
  #:transparent
  #:guard (λ (name sector checksum struct-name)
            (unless (and (string? name)
                         (number? sector)
                         (string? checksum))
              (error 'room-guard "Guard failed on room ~v ~v ~v" name sector checksum))
            (values name sector checksum)))

(define/contract (read-room ip)
  (input-port? . -> . (or/c eof-object? room?))
  (match (regexp-match #px"([a-z-]+)-([0-9]+)\\[([a-z]+)\\][[:blank:]]*" ip)
    [(list a name sector checksum) (room (bytes->string/utf-8 name)
                                         (string->number (bytes->string/utf-8 sector))
                                         (bytes->string/utf-8 checksum))]
    [#f eof]))

(define/contract (count-chars ls)
  ((listof char?) . -> . (listof (cons/c exact-positive-integer? char?)))
  (map (λ (c) (cons (count (curry equal? c) ls) c))
       (remove-duplicates ls)))

(define/contract (sort-chars ls)
  ((listof (cons/c exact-positive-integer? char?)) . -> . (listof (cons/c exact-positive-integer? char?)))
  (sort ls
        (λ (a b)
          (if (equal? (car a) (car b))
              (char<? (cdr a) (cdr b))
              (> (car a) (car b))))))

(define/contract (checksum name)
  (string? . -> . string?)
  (define chars (sort-chars (count-chars (string->list (string-replace name "-" "")))))
  (list->string
   (map cdr
        (take chars 5))))

(define/contract (valid? r)
  (room? . -> . boolean?)
  (string=? (checksum (room-name r))
          (room-checksum r)))

(define (rotate-right n lower upper)
  (define m (add1 n))
  (if (< upper m)
      lower
      m))

(define/contract (decrypt s)
  (string? . -> . string?)
  (string-join (map (compose1
                     list->string
                     (λ (ls)
                       (map (compose1
                             integer->char
                             (curryr rotate-right (char->integer #\:a) (char->integer #\:z))
                             char->integer)
                            ls))
                     string->list)
                    (string-split s "-")) " "))

(module+ test
  (require rackunit)
  (define testcases
    `(("aaaaa-bbb-z-y-x-123[abxyz]" ,(room "aaaaa-bbb-z-y-x" 123 "abxyz") #t)
      ("a-b-c-d-e-f-g-h-987[abcde]" ,(room "a-b-c-d-e-f-g-h" 987 "abcde") #t)
      ("not-a-real-room-404[oarel]" ,(room "not-a-real-room" 404 "oarel") #f)
      ("totally-real-room-200[decoy]" ,(room "totally-real-room" 200 "decoy") #f)))
  (define s (string-append (string-join (map car testcases) "\n") "\n"))
  (for ([t testcases])
    (check-equal? (read-room (open-input-string (car t))) (cadr t) (~v t)))
  (for ([t testcases]
        #:when (caddr t))
    (check-equal? (checksum (room-name (cadr t))) (room-checksum (cadr t)) (~v t)))
  (for ([t testcases])
    (check-equal? (valid? (cadr t)) (caddr t) (~v t)))
  (check-equal? (port->list read-room (open-input-string s))
                (map cadr testcases)))

(module+ main
  (define ls (port->list read-room))
  (displayln (format "Read ~a rooms..." (length ls)))
  (define valid-rooms (filter valid? ls))
  (displayln (format "~a rooms are valid..." (length valid-rooms)))
  (displayln (format "Sum of the valid rooms: ~a" (foldl + 0 (map room-sector valid-rooms)))))
;;  (displayln (format "~a rooms fit criteria" (filter (compose1 (curry  decrypt room-name))
