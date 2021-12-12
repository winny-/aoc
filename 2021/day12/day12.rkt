#lang debug racket

(require racket/hash)

(define/contract (small? cave)
  (symbol? . -> . boolean?)
  (define s (symbol->string cave))
  (string=? s (string-downcase s)))

(define (read2 ip)
  (match (read-line ip)
    [(? eof-object? e) e]
    [(regexp "([A-Za-z]+)-([A-Za-z]+)" (list _ start end)) (cons (string->symbol start)
                                                                 (string->symbol end))]))

(define (dfs graph can-visit-twice)
  (let loop ([vertex 'start] [visited (set)] [smol-twice #f] [num 0])
    (define all-edges (hash-ref graph vertex (const (set))))
    (define edges (set-subtract all-edges (if (or smol-twice (not can-visit-twice))
                                              visited
                                              (for/set ([v visited] #:when (not (small? v)))
                                                v))))
    (for/fold ([acc num])
              ([next edges])
      (match next
        ['end (add1 acc)]
        [(? small?) (loop next (set-add visited next) (or smol-twice (set-member? visited next)) acc)]
        [_ (loop next visited smol-twice acc)]))))

(define (part1 graph)
  (dfs graph #f))

(define (part2 graph)
  (dfs graph #t))

(module+ main
  (define ls (port->list read2))
  (define graph (for/fold ([acc (hasheq)])
                          ([edge (in-list ls)])
                  (match-define (cons start end) edge)
                  ;; Omit some edges and vertices that do not make sense.
                  (define/match (create-singleton-hash e-start e-end)
                    [('end _) (hasheq)]
                    [(_ 'start) (hasheq e-start (set))]
                    [('start _) (hasheq e-start (set e-end))]
                    [(_ 'end) (hasheq e-start (set e-end))]
                    [(_ _) (hasheq e-start (set e-end)
                                   e-end (set e-start))])
                  (hash-union acc
                              (create-singleton-hash start end)
                              (create-singleton-hash end start)
                              #:combine set-union)))
  (part1 graph)
  (part2 graph))

;; Local Variables:
;; compile-command: "racket day12.rkt < sample.txt"
;; End:
