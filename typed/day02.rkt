#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(define-type Box (List Natural Natural Natural))

(: string->boxes (-> String (Listof Box)))
(define (string->boxes str)
  (for/list : (Listof Box)
            ([ln (in-list (string-split str "\n"))])
            (cast (map string->number (string-split ln "x")) Box)))

(: box->paper (-> Box Natural))
(define (box->paper box)
  (match-define (list x y z) box)
  (define sides (list (* x y) (* y z) (* x z)))
  (+ (* 2 (apply + sides)) (apply min sides)))

(: q1 (-> String Natural))
(define (q1 str)
  (define boxes (string->boxes str))
  (apply + (map box->paper boxes)))

(: box->ribbon (-> Box Natural))
(define (box->ribbon box)
  (match-define (list x y z) box)
  (: perimeter (-> Natural Natural Natural))
  (define (perimeter dim1 dim2) (* 2 (+ dim1 dim2)))
  (define perimeters
    (list (perimeter x y) (perimeter y z) (perimeter x z)))
  (+ (apply min perimeters) (* x y z)))

(: q2 (-> String Natural))
(define (q2 str)
  (define boxes (string->boxes str))
  (apply + (map box->ribbon boxes)))

(module+ test
  (define input-str (file->string "../day02-input.txt"))
  (check-equal? (q1 input-str) 1586300)
  (check-equal? (q2 input-str) 3737498))
