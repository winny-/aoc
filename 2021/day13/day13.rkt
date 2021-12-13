#lang debug racket

(struct Input (points folds) #:transparent)

(define (read2 [ip (current-input-port)])
  (let loop ([points (set)] [folds empty])
    (match (read-line ip)
      [(? eof-object? e) (Input points (reverse folds))]
      [(regexp #rx"([0-9]+),([0-9]+)" (list _ x y))
       (loop (set-add points (list (string->number x) (string->number y)))
             folds)]
      ["" (loop points folds)]
      [(regexp "fold along ([xy])=([0-9]+)" (list _ axis position))
       (loop points
             (cons (list (string->symbol axis) (string->number position)) folds))])))

(define (transform mag split)
  (- mag  (* 2 (abs (- split mag)))))

(define (fold-x points reflect-at)
  (define-values (normal must-reflect)
    (partition (match-lambda [(list _ y) (< y reflect-at)]) (set->list points)))
  (set-union (list->set normal)
             (for/set ([pt (in-list must-reflect)])
               (match pt
                 [(list x y) (list x (transform y reflect-at))]))))

(define (fold-y points reflect-at)
  (define-values (normal must-reflect)
    (partition (match-lambda [(list x _) (< x reflect-at)]) (set->list points)))
  (set-union (list->set normal)
             (for/set ([pt (in-list must-reflect)])
               (match pt
                 [(list x y) (list (transform x reflect-at) y)]))))

(define/match (fold points line)
  [(_ (list 'y reflect-at)) (fold-x points reflect-at)]
  [(_ (list 'x reflect-at)) (fold-y points reflect-at)])

(define/match (part1 input)
  [((struct Input [points (list line _ ...)]))
   (set-count (fold points line))])

(define/match (part2 input)
  [((struct Input [points folds]))
   (draw
    (for/fold ([acc points])
              ([f (in-list folds)])
      (fold acc f)))])

(define (draw points [line #f])
  (define ls (set->list points))
  (define (max-dimension getter)
    (argmax identity (map getter ls)))
  (define rows (for/list ([y (in-range 0 (add1 (max-dimension second)))])
                 (define columns (for/list ([x (in-range 0 (add1 (max-dimension first)))])
                                   (if (set-member? points (list x y))
                                       "#"
                                       ".")))
                 (string-join columns "")))
  (string-join rows "\n"))

(module+ main
  (define ls (read2))
  (part1 ls)
  (displayln (part2 ls)))

;; Local Variables:
;; compile-command: "racket day13.rkt < sample.txt"
;; End:
