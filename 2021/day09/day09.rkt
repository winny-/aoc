#lang racket

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (map (compose1 string->number string) (string->list s))]))

(define/match (neighbors grid loc)
  [((list a _ ...) (list x y))
   (filter identity (for/list ([offset '((-1 0) (1 0) (0 -1) (0 1))])
                      (match-define (and neigh (list nx ny)) (map + loc offset))
                      (and (not (equal? loc neigh))
                           (<= 0 nx) (> (length a) nx)
                           (<= 0 ny) (> (length grid) ny)
                           neigh)))])

(define/match (lookup grid loc)
  [(_ (list x y)) (list-ref (list-ref grid y) x)])

(define (digit? v) (between/c 0 9))

(define (loc? v) (list/c exact-nonnegative-integer? exact-nonnegative-integer?))

(define/contract (find-basin grid loc)
  (-> (listof (listof digit?)) loc? (set/c loc?))
  (let loop ([basin (set)] [visited (set)] [queue (list loc)])
    (if (empty? queue)
        basin
        (match-let* ([(list current-loc queue2 ...) queue]
                     [current-cell (lookup grid current-loc)]
                     [current-neighbors (neighbors grid current-loc)])
          (loop (set-add basin current-loc)
                (set-union visited (set current-loc) (list->set current-neighbors))
                (append
                 (for/list ([neigh current-neighbors]
                            #:when (and (not (set-member? (set) neigh))
                                        (match (lookup grid neigh)
                                          [9 #f]
                                          [v (< current-cell v)])))
                   neigh)
                 queue2))))))

(define (low-points grid)
  (for/list ([row (in-list grid)]
             [y (in-naturals)]
             #:when #t
             [cell (in-list row)]
             [x (in-naturals)]
             #:when (for/and ([neigh (neighbors grid (list x y))])
                      (< cell (lookup grid neigh))))
    (list cell (list x y))))

(require pict)
(define (grid->pict grid [highlights (hash)])
  (apply vc-append (for/list ([row grid]
                              [y (in-naturals)])
                     (apply hc-append (for/list ([cell row]
                                                 [x (in-naturals)])
                                        (define base (colorize (text (number->string cell) null 24) "white"))
                                        ((hash-ref highlights (list x y) (const identity)) base))))))

(define (basins grid)
  (for/list ([pt (low-points grid)]
             #:unless (= 9 (car pt)))
    (list pt (find-basin grid (cadr pt)))))


(define (part1 ls)
  (for/sum ([pt (low-points ls)])
    (add1 (car pt))))

(define (part2 ls)
  (for/product ([basin (take (sort (map cadr (basins ls))
                                   >=
                                   #:key set-count) 3)])
    (set-count basin)))

(module+ main
  (require racket/hash)
  (define ls (port->list read2))
  ;; (define ls (with-input-from-file "input.txt" (thunk (port->list read2)) ))
  ;; The following allows you to visualize the basins.
#;
  (for/list ([b (basins ls)])
    (grid->pict ls (hash-union (hash (cadar b) (λ (p) (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p)) "Dark Red") p)))
                               (for/hash ([pt (cadr b)]
                                          #:unless (equal? pt (cadar b)))
                                 (values pt (λ (p) (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p)) "Royal Blue") p)))))))
  (part1 ls)
  (part2 ls))

;; Local Variables:
;; compile-command: "racket day09.rkt < sample.txt"
;; End:
