#|

A bit of a mess.  Duplicated code and stuff.  Should come back and fix this up
later...  Maybe!

|#

#lang racket

(struct Game [draws boards] #:transparent)
(struct Board [board tokens size] #:transparent)

(define/match (board->string the-board)
  [((struct Board [board tokens size]))
   (for*/fold ([s ""])
             ([row size]
              [column size])
     (define num-str (~a (vector-ref board (+ (* row size) column)) #:width 3 #:align 'center))
     (string-append s (if (vector-ref tokens (+ (* row size) column))
                          (format "(~a)" num-str)
                          (format " ~a " num-str))
                    (if (= column (sub1 size)) "\n" " ")))])

(define (read-draws [ip (current-input-port)])
  (match (read-line ip)
    [(? eof-object? e) e]
    [s (map string->number (string-split s ","))]))

(define (read-board [ip (current-input-port)])
  (let loop ([acc empty] [start #t] [size #f])
    (match (read-line)
      ["" #:when start (loop acc #f size)]
      [(or (? eof-object?) "") #:when (not (empty? acc))
                               (Board (list->vector acc) (make-vector (length acc) #f) size)]
      [(? eof-object? e) e]
      [s
       (define row (port->list read (open-input-string s)))
       (loop (append acc row) #f (length row))])))

(define (read-game [ip (current-input-port)])
  (match (read-draws ip)
    [(? eof-object? e) e]
    [draws (Game draws (parameterize ([current-input-port ip])
                         (port->list read-board)))]))
(define (win-conditions size)
  (append
   ;; Horizontal
   (for/list ([i size])
     (for/list ([j (in-range (* i size) (+ (* i size) size))])
       j))
   ;; Vertical
   (for/list ([i size])
     (for/list ([j (in-range i (sqr size) size)])
       j))
   #|

   Well that wasn't needed at all :)

   (list
   ;; Main diagonal
   (for/list ([i size]
   [j size])
   (+ i (* j size)))
   ;; Anti diagonal
   (for/list ([i size]
   [j (in-range (sub1 size) -1 -1)])
   (+ i (* j size))))
   |#
   ))

(define/match (board-win the-board)
  [((struct Board [board tokens size]))
   (for/first ([condition (win-conditions size)]
               #:when (for/and ([idx condition]) (vector-ref tokens idx)))
     condition)])

(define/match (play-board the-board value)
  [((struct Board [board tokens size]) _)
   (define tokens2
     (for/vector ([v board]
                  [t tokens])
       (or t (= v value))))
   (Board board tokens2 size)])

(define (part1 game)
  (let loop ([g game] [last-drawn #f])
    (match (for/first ([b (Game-boards g)]
                       #:when (board-win b))
             b)
      [(and b (struct Board [board tokens size]))
       ;; (displayln (board->string b))
       (* last-drawn
          (for/sum ([value board]
                    [token tokens]
                    #:when (not token))
            value))]
      [#f
       (match-define (struct Game [(list current-draw draws ...) boards]) g)
       (loop (Game draws (map (curryr play-board current-draw) boards)) current-draw)])))

(define (part2 game)
  (let loop ([g game] [last-drawn #f])
    (match (for/first ([b (Game-boards g)]
                       #:when (board-win b))
             b)
      [(and b (struct Board [board tokens size]))
       ;; (displayln (board->string b))
       (if (= 1 (length (Game-boards g)))
           (* last-drawn
              (for/sum ([value board]
                           [token tokens]
                           #:when (not token))
                   value))
           (loop (struct-copy Game g [boards (remove b (Game-boards g))]) last-drawn))]
      [#f
       (match-define (struct Game [(list current-draw draws ...) boards]) g)
       (loop (Game draws (map (curryr play-board current-draw) boards)) current-draw)])))

(module+ main
  (define game (read-game))
  ;; (displayln game)
  (part1 game)
  (part2 game))

;; Local Variables:
;; compile-command: "racket day04.rkt < sample.txt"
;; End:
