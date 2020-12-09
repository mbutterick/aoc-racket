#lang br
(require racket/file rackunit)

(define vec (list->vector (map string->number (file->lines "09.rktd"))))

(define preamble-length 25)

(define (idx-valid? idx)
  (define predecessors (for/list ([val (in-vector vec (- idx preamble-length) idx)])
                         val))
  (define target (vector-ref vec idx))
  (for/or ([ints (in-combinations predecessors 2)])
    (eq? (apply + ints) target)))

(define invalid-number
  (for/first ([idx (in-range (add1 preamble-length) (vector-length vec))]
              #:unless (idx-valid? idx))
    (vector-ref vec idx)))

(check-equal? invalid-number 14144619)

(check-equal?
 (for/or ([lidx (in-range 0 (vector-length vec))])
   (let/ec skip-to-next-lidx
     (for/or ([ridx (in-range (add1 lidx) (vector-length vec))])
       (define ints (for/list ([val (in-vector vec lidx (add1 ridx))])
                      val))
       (define sum (apply + ints))
       (cond
         [(> sum invalid-number) (skip-to-next-lidx #false)]
         [(eq? sum invalid-number) (+ (apply min ints) (apply max ints))]
         [else #false]))))
 1766397)
