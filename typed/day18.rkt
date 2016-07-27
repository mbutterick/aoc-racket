#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))
(require/typed racket/vector ;;bg; not sure how to express "a pattern, repeating"
  (vector-set*! (All (A) (-> (Vectorof A) Integer A Integer A Integer A Integer A Void))))

(define grid-side 102)

(: rowcol->idx (-> Integer Integer Integer))
(define (rowcol->idx row col) (+ (* grid-side row) col))

(: idx->rowcol (-> Integer (Values Integer Integer)))
(define (idx->rowcol idx) (quotient/remainder idx grid-side))

(: count-lit (-> (Vectorof Integer) Integer))
(define (count-lit grid) (apply + (vector->list grid)))

(define bulb-on 1)
(define bulb-off 0)

(: input->grid (-> String (Vectorof Integer)))
(define (input->grid str)
  (define grid-vec : (Vectorof Integer) (make-vector (* grid-side grid-side) bulb-off))
  (for* ([(bulb-row bulb-row-idx) (in-indexed (string-split str))]
         [(bulb bulb-col-idx) (in-indexed (regexp-match* #rx"." bulb-row))])
        (vector-set! grid-vec (rowcol->idx (add1 bulb-row-idx) (add1 bulb-col-idx))
                     (if (equal? bulb "#") bulb-on bulb-off)))
  grid-vec)

(: bulb+adjacents (-> (Vectorof Integer) Integer (Vectorof Integer)))
(define (bulb+adjacents grid grid-idx)
  (define-values (row col) (idx->rowcol grid-idx))
  (for*/vector ([r (in-range (sub1 row) (+ row 2))]
                [c (in-range (sub1 col) (+ col 2))])
               : Integer
               (vector-ref grid (rowcol->idx r c))))

(: iterate-grid (-> (Vectorof Integer) (Vectorof Integer)))
(define (iterate-grid grid)
  (for*/vector ([row (in-range grid-side)]
                [col (in-range grid-side)])
               : Integer
               (cond
                 [(or (= row 0) (= col 0)
                      (= row (sub1 grid-side))
                      (= col (sub1 grid-side)))
                  bulb-off]
                 [else
                  (define bulb-idx (rowcol->idx row col))
                  (define bulb (vector-ref grid bulb-idx))
                  (define lit-neighbors
                    (- (count-lit (bulb+adjacents grid bulb-idx)) bulb))
                  (cond
                    [(= bulb-on bulb) (if (<= 2 lit-neighbors 3) bulb-on bulb-off)]
                    [(= 3 lit-neighbors) bulb-on]
                    [else bulb-off])])))

(: q1 (-> String Integer))
(define (q1 input-str)
  (define initial-grid (input->grid input-str))
  (define iterations 100)
  (define final-grid (for/fold : (Vectorof Integer)
                               ([grid-so-far : (Vectorof Integer) initial-grid])
                               ([i (in-range iterations)])
                       (iterate-grid grid-so-far)))
  (count-lit final-grid))



(: light-corners (-> (Vectorof Integer) (Vectorof Integer)))
(define (light-corners grid)
  (vector-set*! grid
                (rowcol->idx 1 1) bulb-on
                (rowcol->idx 1 100) bulb-on
                (rowcol->idx 100 1) bulb-on
                (rowcol->idx 100 100) bulb-on)
  grid)

(: q2 (-> String Integer))
(define (q2 input-str)
  (define initial-grid (light-corners (input->grid input-str)))
  (define iterations 100)
  (define final-grid (for/fold : (Vectorof Integer)
                               ([grid-so-far initial-grid])
                               ([i (in-range iterations)])
                       (light-corners (iterate-grid grid-so-far))))
  (count-lit final-grid))

(module+ test
  (define input-str (file->string "../day18-input.txt"))
  (check-equal? (q1 input-str) 821)
  (check-equal? (q2 input-str) 886))


