#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(define first-code 20151125)

(: next-code (-> Integer Integer))
(define (next-code code)
  (modulo (* code 252533) 33554393))

(: nth-code (-> Integer Integer))
(define (nth-code n)
  (for/fold : Integer ([code-so-far first-code])
            ([i (in-range (sub1 n))])
    (next-code code-so-far)))

(: rc->n (-> Integer Integer Integer))
(define (rc->n row col)
  (define first-col-val (add1 (apply + (range row))))
  (define col-offset-val (apply + (range (add1 row) (+ row col))))
  (+ first-col-val col-offset-val))

(: q1 (-> String Integer))
(define (q1 input-str)
  (match-define (list _ row col)
    (map (λ ([s : (U #f String)]) (and s (string->number s)))
         (or (regexp-match #px"row (\\d+), column (\\d+)" input-str) (error 'bg))))
  (nth-code (rc->n (cast row Integer) (cast col Integer))))

(module+ test
  (define input-str (file->string "../day25-input.txt"))
  (check-equal? (q1 input-str) 19980801))

