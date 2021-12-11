#|

I kept mixing up my copies of variables.  For example my copy of acc was acc2,
stepped vs acc, etc.  Placing the result of a for/fold being the same binding
that the fold introduces, without any modification (instead of the modified
binding).  Perhaps by changing my naming habits or programming style I can
avoid these sorts of problems.  Maybe there is a static analyzer or way to
modify the for/fold macro to recognize this sort of obvious mistake.

Additionally, working with a list of list as a grid data type, feels less than
ideal.  There is the array built in type, but that seems to be exceptionally
slower in vanilla racket.  You have to use Typed racket to get acceptable
performance.  Maybe there are type-unsafe array operations one could use?  I'm
okay with a segfault as long as it runs at least as fast as the list of lists.

This uses a silly hack to redirect all stdout to stderr instead of fixing the
print-debugging to print to stderr.  I dunno, I'm lazy and it worked for me.

|#

#lang racket

(require racket/pretty
         "../../lib.rkt")

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (map (compose1 string->number string) (string->list s))]))

(define (in-grid grid)
  (for/stream ([row (in-list grid)]
               [y (in-naturals)]
               #:when #t
               [cell (in-list row)]
               [x (in-naturals)])
    (list x y cell)))

(define (grid-map proc grid)
  (for/list ([row (in-list grid)]
             [y (in-naturals)])
    (for/list ([cell (in-list row)]
               [x (in-naturals)])
      (proc (list x y cell)))))

(define (update grid loc updater)
  (list-update grid (second loc) (curryr list-update (first loc) updater)))

(define (step grid)
  (define universal-growth
    (for*/list ([row (in-list grid)])
      (for*/list ([cell (in-list row)])
        (add1 cell))))
  (define mapped (grid-map (match-lambda [(list _ _ cell) (list cell #f)]) universal-growth))
  (define flashed
    (let loop ([acc mapped]
               [queue (for/list ([pt (in-grid mapped)]
                                 #:when (match-let ([(list _ _ (list cell _)) pt]) (> cell 9)))
                        (take pt 2))])
      (match queue
        [(list)
         (grid-map (match-lambda [(list _ _ (list cell _)) cell]) acc)]
        [(list loc queue2 ...)
         (match-define (list cell flashed) (lookup acc loc))
         (printf "Loop (~a,~a) = ~a [~a]\n" (first loc) (second loc) cell flashed)
         (display-grid (grid-map (match-lambda [(list _ _ (list cell _)) cell]) acc) loc) 

         (if (or flashed (<= cell 9))
             (loop acc queue2)
             (call-with-values
              (thunk
               (for/fold ([acc2 (update acc loc (match-lambda [(list cell _) (list cell #t)]))]
                          [queue3 queue2])
                         ([neigh (neighbors acc loc)])
                 (values (update acc2 neigh (curryr list-update 0 add1))
                         (cons neigh queue3))))
              loop))])))
  (define drained
    (for*/list ([row (in-list flashed)])
      (for*/list ([cell (in-list row)])
        (if (> cell 9)
            0
            cell))))
  drained)

(define (display-grid grid [current-cell #f])
  (for ([row (in-list grid)]
        [y (in-naturals)])
    (displayln
     (string-join
      (for/list ([cell (in-list row)]
                 [x (in-naturals)])
        (string-append
         (if (equal? (list x y) current-cell) "\e[47m\e[30m" "")
         (match cell
           [0 "\e[1m0"]
           [_ #:when (> cell 9) "\e[1mX"]
           [_ (~a cell)])
         "\e[0m"))
      "")))
  (newline))

(define (part1 ls)
  (parameterize ([current-output-port (current-error-port)])
    (displayln "Initial")
    (display-grid ls)
    (for/fold ([acc ls]
               [flashes 0]
               #:result flashes)
              ([x (in-range 1 101)])
      (printf "Step ~a START --------------------------------------------------------------\n" x)
      (define stepped
        (step acc))
      (displayln "Result --------------------------------------------------------------------")
      (display-grid stepped)
      (printf "Step ~a END ----------------------------------------------------------------\n" x)
      (values stepped
              (+ flashes (for/sum ([cell (in-grid stepped)] #:when (zero? (third cell))) 1))))))

(define (part2 ls)
  (parameterize ([current-output-port (current-error-port)])
    (let/ec escape
      (for/fold ([acc ls])
                ([x (in-naturals 1)])
        (printf "Step ~a\n" x)
        (define stepped (step acc))
        (when (for/and ([cell (in-grid stepped)])
                (displayln cell)
                (zero? (third cell)))
          (displayln "escaping...")
          (escape x))
        stepped))))

(module+ main
  (define ls (port->list read2))
  (pretty-print (part1 ls))
  (pretty-print (part2 ls)))

;; Local Variables:
;; compile-command: "racket day11.rkt < sample.txt 2>/dev/null"
;; End:
