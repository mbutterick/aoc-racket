#lang debug br
(require racket/file)

(define (mlength mprs [acc 0])
  (if (null? mprs)
      acc
      (mlength (mcdr mprs) (add1 acc))))

(define (nth-mpr mprs n)
  (for/fold ([mprs mprs])
            ([i (in-range n)])
    (mcdr mprs)))

(define (★)
  (match-define (list player-count marble-count)
    (map string->number (regexp-match* #px"\\d+" (file->string "09.txt"))))
  (define scores (make-hasheqv))
  (define circle (mcons #f (mcons 0 null)))
  (let loop ([marble 1] [pos 0])     
    (cond
      [(> marble marble-count) (argmax cdr (hash->list scores))]
     [(zero? (modulo marble 23))
       (define marble-count (sub1 (mlength circle)))
       (define deletion-pos (modulo (+ (- pos 7) marble-count) marble-count))
       (define last-left-mpr (nth-mpr circle deletion-pos))
       (define removed-marble (mcar (mcdr last-left-mpr)))
       (set-mcdr! last-left-mpr (mcdr (mcdr last-left-mpr)))
       (define player (modulo marble player-count))
       (hash-update! scores player (λ (sc) (+ removed-marble marble sc)) 0)
       (loop (add1 marble) deletion-pos)]
      [else
       (define marble-count (sub1 (mlength circle)))
       (define next-pos (add1 (modulo (add1 pos) marble-count)))
       (define last-left-mpr (nth-mpr circle next-pos))
       (set-mcdr! last-left-mpr (mcons marble (mcdr last-left-mpr)))
       (loop (add1 marble) next-pos)])))

(time (★))

#;(define (★★)
    )
#;(★★)

#;(module+ test
    (require rackunit)
    (check-equal? (time (★)) 437654)
    (check-equal? (time (★★)) 566))