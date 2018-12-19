#lang debug br
(require racket/file)

(define (nth-mpr mprs n)
  (for/fold ([mprs mprs])
            ([i (in-range n)])
    (mcdr mprs)))

(define (★)
  (match-define (list player-count max-marbles)
    (map string->number (regexp-match* #px"\\d+" (file->string "09.txt"))))
  (define scores (make-hasheqv))
  (define circle (mcons #f (mcons 0 null)))
  (let loop ([marble 1] [marbles-in-circle 1] [pos 0])
    (cond
      [(> marble max-marbles) (cdr (argmax cdr (hash->list scores)))]
     [(zero? (modulo marble 23))
       (define deletion-pos (modulo (+ (- pos 7) marbles-in-circle) marbles-in-circle))
       (define last-left-mpr (nth-mpr circle deletion-pos))
       (define removed-marble (mcar (mcdr last-left-mpr)))
       (set-mcdr! last-left-mpr (mcdr (mcdr last-left-mpr)))
       (define player (modulo marble player-count))
       (hash-update! scores player (λ (sc) (+ removed-marble marble sc)) 0)
       (loop (add1 marble) (sub1 marbles-in-circle) deletion-pos)]
      [else
       (define next-pos (add1 (modulo (add1 pos) marbles-in-circle)))
       (define last-left-mpr (nth-mpr circle next-pos))
       (set-mcdr! last-left-mpr (mcons marble (mcdr last-left-mpr)))
       (loop (add1 marble) (add1 marbles-in-circle) next-pos)])))

#;(define (★★)
    )
#;(★★)

(module+ test
    (require rackunit)
    (check-equal? (time (★)) 437654)
    #;(check-equal? (time (★★)) 566))