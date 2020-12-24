#lang debug racket

(provide (all-defined-out))

(require plot)
(plot-new-window? #t)


(define show-plot? (make-parameter #f))
(define renderers (make-parameter empty))

(struct component (direction length) #:transparent)

(define (dimensions path)
  (define (single-dimension path directions)
    (abs (for/sum ([c path]
                   #:when (memq (component-direction c) directions))
           (match-define (struct component (direction length)) c)
           (match direction
             [(or 'left 'up) (- length)]
             [(or 'right 'down) length]))))
  (list (single-dimension path '(left right))
        (single-dimension path '(up down))))

(define/match (step c pos)
  [((struct component (direction length)) (list x y))
   (match direction
     ['up (list x (- y length))]
     ['down (list x (+ y length))]
     ['left (list (- x length) y)]
     ['right (list (+ x length) y)])])

(define (traverse p)
  (for/fold ([acc '((0 0))] #:result (reverse acc))
            ([c p])
    (match-define (list pos _ ...) acc)
    (cons (step c pos) acc)))

(define (points->lines points)
  (define ls (for/list ([a points] [b (cdr points)])
               (list a b)))
  (renderers (cons (lines points) (renderers)))
  ls)

#;
(define/match (intersects a b)
  [((cons (cons xa1 ya1) (cons xa2 ya2))
    (cons (cons xb1 yb1) (cons xb2 xb2)))
   (and (not (and (= xa1 xa2) (= xb1 xb2)))
        (not (and (= ya1 ya2) (= yb1 yb2)))
        (if (= xa1 xa2)
            ))])

(define/match (intersects a b)
  [((list (list a b) (list c d))
    (list (list p q) (list r s)))
   (define determinant (- (* (- c a) (- s q))
                          (* (- r p) (- d b))))
   (and (not (zero? determinant))
        (let ([λ (/ (+ (* (- s q) (- r a))
                       (* (- p r) (- s b)))
                    determinant)]
              [γ (/ (+ (* (- b d) (- r a))
                       (* (- c a) (- s b)))
                    determinant)])
          (and (< 0 λ 1) (< 0 γ 1))))])

(define (crosses p1 p2)
  (define ls (for/list ([l1 (points->lines (traverse p1))]
                        [l2 (points->lines (traverse p2))]
                        #:when (intersection l1 l2))
               (define i (intersection l1 l2))
               i))
  (renderers (cons (points ls) (renderers)))
  ls)

(define/match (intersection l1 l2)
  [((list (and a1 (list xa1 ya1)) (and a2 (list xa2 ya2)))
    (list (and b1 (list xb1 yb1)) (and b2 (list xb2 yb2))))
   (cond
     ;; Parallel
     [(or (and (= xa1 xa2) (= xb1 xb2))
          (and (= ya1 ya2) (= yb1 yb2))) #f]
     ;; l1 should change on the x axis
     [(= xa1 xb2) (intersection l2 l1)]
     ;; Order l1 points
     [(> xa1 xa2) (intersection (list a2 a1) l2)]
     ;; Order l2 points
     [(> yb1 yb2) (intersection l1 (list b2 b1))]
     [(and (<= xa1 xb1 xa2)
           (<= yb1 ya1 yb2)) (list xa1 yb2)]
     [else #f])])

(define (part1 wires)
  (parameterize ([renderers empty])
    (define ret (argmin identity (map (match-lambda [(list x y) (+ (abs x) (abs y))]) (crosses (car wires) (cadr wires)))))
    (when (show-plot?)
      (println (renderers))
      (plot (renderers)))
    ret))
#;
(define (find-cross p1 p2)
  (for/fold ([p1 p1]
             [p2 p2])))

(define (line->wire li)
  (for/list ([s (string-split li ",")])
    (match s
      [(regexp #rx"(U|D|L|R)([0-9]+)" (list _ dir len))
       (component (match dir ["U" 'up] ["D" 'down] ["L" 'left] ["R" 'right]) (string->number len))])))

(define (read-challenge [in (current-input-port)])
  (list (line->wire (read-line in))
        (line->wire (read-line in))))

(module+ test
  (require rackunit)
  (test-case "6"
    (check-equal? (part1 (call-with-input-file "input-6.txt" read-challenge)) 6))
  (test-case "135"
    (check-equal? (part1 (call-with-input-file "input-135.txt" read-challenge)) 135))
  (test-case "159"
    (check-equal? (part1 (call-with-input-file "input-159.txt" read-challenge)) 159)))

(module+ main
  (define ch (read-challenge))
  (println ch)
  (part1 ch))
