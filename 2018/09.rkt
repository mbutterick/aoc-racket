#lang debug br
(require racket/file)

#|
This puzzle seemed annoying at first but turned out to be educational.
An epic difference between the naive solution (= make a list and iterate from the start)
and using a double-linked list, which minimizes traversal.
|#

(struct dll (val prev next) #:mutable)

(define (move-by dll n)
  (define iterator
    (match n
      [(? positive?) dll-next]
      [(? negative?) dll-prev]
      [_ values]))
  (for/fold ([dll dll])
            ([i (in-range (abs n))])
    (iterator dll)))

(define (remove-marble! marble)
  (set-dll-next! (dll-prev marble) (dll-next marble))
  (set-dll-prev! (dll-next marble) (dll-prev marble)))

(define (find-winner player-count max-marbles)
  (define scores (make-hasheqv))
  (define first-marble (dll 0 #f #f))
  (set-dll-prev! first-marble first-marble)
  (set-dll-next! first-marble first-marble)
  (for/fold ([current-marble first-marble]
             #:result (cdr (argmax cdr (hash->list scores))))
            ([marble (in-range 1 max-marbles)])
    (cond
      [(zero? (modulo marble 23))
       (define marble-to-remove (move-by current-marble -7))
       (remove-marble! marble-to-remove)
       (define player (modulo marble player-count))
       (hash-update! scores player (λ (sc) (+ (dll-val marble-to-remove) marble sc)) 0)
       (dll-next marble-to-remove)]
      [else
       (define left-marble (move-by current-marble 1))
       (define right-marble (dll-next left-marble))
       (define new-marble (dll marble left-marble right-marble))
       (set-dll-next! left-marble new-marble)
       (set-dll-prev! right-marble new-marble)
       new-marble])))

(match-define (list player-count max-marbles)
  (map string->number (regexp-match* #px"\\d+" (file->string "09.txt"))))

(define (★) (find-winner player-count max-marbles))

(define (★★) (find-winner player-count (* 100 max-marbles)))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 437654)
  (check-equal? (time (★★)) 3689913905))