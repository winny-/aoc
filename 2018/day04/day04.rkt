#lang racket

(require racket/date
         racket/hash)

(struct Record (date) #:transparent)
(struct Wake Record () #:transparent)
(struct Sleep Record () #:transparent)
(struct Begin Record (id) #:transparent)

(define (read-entry [ip (current-input-port)])
  (match (read-line ip)
    [(? eof-object? o) o]
    [s
     (match-define (list _ year month day hour minute change)
       (regexp-match #px"\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] ([^\n]+)$" s))
     (define d (seconds->date (find-seconds 0 (string->number minute) (string->number hour)
                                            (string->number day) (string->number month)
                                            (string->number year))))
     (match change
       ["wakes up" (Wake d)]
       ["falls asleep" (Sleep d)]
       [(regexp #px"Guard #(\\d+) begins shift" (list _ id)) (Begin d (string->number id))])]))

(define (hash-max h #:key [key identity])
  (for/fold ([acc #f])
            ([(k v) (in-hash h)])
    (if (or (not acc) (> (key v) (key (cdr acc))))
        (cons k v)
        acc)))

(define (part12 records)
  (define-values (time-slept frequency)
    (for/fold ([time-slept (hasheq)]
               [frequency (hasheq)]
               [guard #f]
               [sleeping #f]
               #:result (values time-slept frequency))
              ([rec records])
      (match rec
        [(struct Wake (d))
         (if sleeping
             (let* ([duration (abs (- (date->seconds sleeping) (date->seconds d)))]
                    [m-sleep (date-minute sleeping)]
                    [m-wake (date-minute d)]
                    [minutes-awake
                     (for/fold ([h (hasheq)])
                               ([minute (if (> m-sleep m-wake)
                                            (append (range m-sleep 61) (range 0 (add1 m-wake)))
                                            (range m-sleep (add1 m-wake)))])
                       (hash-update h minute add1 (const 0)))])
               (values
                (hash-update time-slept guard
                             (curry + duration)
                             (const 0))
                (hash-update frequency guard
                             (λ (histogram) (hash-union minutes-awake histogram #:combine +))
                             (const (hasheq)))
                guard
                #f))
             (values time-slept frequency guard #f))]
        [(struct Sleep (d))
         (values time-slept frequency guard d)]
        [(struct Begin (d id))
         (values time-slept frequency id #f)])))
  (define id (car (hash-max time-slept)))
  (define most-sleepy-minute (car (hash-max (hash-ref frequency id))))
  (values
   (* id most-sleepy-minute)
   (match (hash-max (for/hash ([(k v) (in-hash frequency)])
                      (values k (hash-max v))) #:key cdr)
     [(cons id (cons minute _)) (* id minute)])))

(define (part1 entries)
  (define-values (p1 p2) (part12 entries))
  p1)

(module+ test
  (define EXAMPLE #<<EOF
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
EOF
    )
(define entries
  (port->list read-entry (open-input-string EXAMPLE)))
(part1 entries))

(module+ main
  (define entries (sort (port->list read-entry) < #:key (compose1 date->seconds Record-date)))
  ;; (define entries (port->list read-entry))
  (part12 entries))
