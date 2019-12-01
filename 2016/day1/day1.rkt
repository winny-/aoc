#lang racket

(define rl-translations
  #hash((north . #hash((R . east)
                       (L . west)))
        (east . #hash((R . south)
                      (L . north)))
        (south . #hash((R . west)
                       (L . east)))
        (west . #hash((R . north)
                      (L . south)))))

(define point-translations
  (hash 'north (λ (pt dis)
                 (point (point-x pt)
                        (+ (point-y pt) dis)))
        'east (λ (pt dis)
                (point (+ (point-x pt) dis)
                       (point-y pt)))
        'south (λ (pt dis)
                 (point (point-x pt)
                        (- (point-y pt) dis)))
        'west (λ (pt dis)
                (point (- (point-x pt) dis)
                       (point-y pt)))))

(struct point (x y)
  #:transparent)

(struct state (point direction)
  #:transparent)

(struct instruction (rl distance)
  #:transparent)

(define initial-state (state (point 0 0) 'north))

(define (distance-between-points a b)
  (sqrt (+ (sqr (- (point-x a) (point-x b)))
           (sqr (- (point-y a) (point-y b))))))

(define (read-instruction [ip (current-input-port)])
  (match (regexp-match #px"([RL])([0-9]+)(, |\n)?" ip)
    [#f eof]
    [(list a rl num b) (instruction
                        (string->symbol (bytes->string/utf-8 rl))
                        (string->number (bytes->string/utf-8 num)))]
    [a (error "should not reach ~a" a)]))

(define (step st in)
  (define new-direction
    (hash-ref (hash-ref rl-translations
                        (state-direction st))
              (instruction-rl in)))
  (define new-point
    ((hash-ref point-translations new-direction) (state-point st) (instruction-distance in)))
  (state new-point new-direction))

(define (run instructions)
  (let loop ([st initial-state] [instructions instructions])
    (if (empty? instructions)
        st
        (loop (step st (car instructions)) (cdr instructions)))))

(define (run-list instructions)
  (let loop ([states (list initial-state)] [instructions instructions])
    (if (empty? instructions)
        (reverse states)
        (loop (cons (step (car states) (car instructions)) states)
                    (cdr instructions)))))

(define (find-duplicates lst [equal-proc equal?])
  (let loop ([lst lst] [acc '()])
    (if (empty? lst)
        (reverse acc)
        (loop (cdr lst)
              (if (member (car lst) lst equal-proc)
                  (cons (car lst)
                        acc)
                  acc)))))

(define (distance-from-start pt)
  (+ (abs (point-x pt))
     (abs (point-y pt))))

(module+ test
  (require rackunit)
  (define i1 (list (instruction 'R 2)
                   (instruction 'L 3)))
  (define i2 (list (instruction 'R 2)
                   (instruction 'R 2)
                   (instruction 'R 2)))
  (define i3 (list (instruction 'R 5)
                   (instruction 'L 5)
                   (instruction 'R 5)
                   (instruction 'R 3)))
  (check-equal? (state-point (run i1)) (point 2 3))
  (check-equal? (state-point (run i2)) (point 0 -2)))

(module+ main
  (define instructions (port->list read-instruction))
  (define states (run-list instructions))
  (define end-state (last states))
  (define end-point (state-point end-state))
  (displayln (format "Blocks from start: ~a" (distance-from-start end-point)))
  (define first-dupe (check-duplicates states #:key state-point))
  (define first-dupe-point (state-point first-dupe))
  (displayln (format "First block visited twice: (~a ~a), ~a blocks away"
                     (point-x first-dupe-point)
                     (point-y first-dupe-point)
                     (distance-from-start first-dupe-point)))
  #;(define dupes (find-duplicates states (λ (a b)
                                          (equal? (state-point a)
                                                  (state-point b))))))
