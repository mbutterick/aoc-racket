#lang reader "../aoc-lang.rkt"

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb (STARS) (NUMBER) ...)
  #'(#%module-begin ((if (eq? 'STARS '★) dist larger-sum) NUMBER) ...))

(define (ring-side r) (* 2 r))
(define (ring-last r) (expt (add1 (ring-side r)) 2))
(define (ring-first r) (if (zero? r) 1 (add1 (ring-last (sub1 r)))))

(define (ring int)
  (for/first ([i (in-naturals)]
              #:when (<= int (ring-last i)))
             i))

(define (nth-coordinate n)
  (cond
    [(= n 1) 0]
    [else
     (define ring-idx (ring n))
     (define offset (- n (ring-first ring-idx)))
     (define-values (quadrant pos)
       (quotient/remainder offset (ring-side ring-idx)))
     (* (+ ring-idx (* +i (- pos (sub1 ring-idx)))) (expt +i quadrant))]))

(define (dist n)
  (define c (nth-coordinate n))
  (+ (abs (real-part c)) (abs (imag-part c))))

(define vals (make-hash))
(define (neighbor-sum n)
  (define c (nth-coordinate n))
  (hash-ref! vals c (λ () (if (= c 0)
                              1
                              (for*/sum ([h (in-list '(-1 0 1))]
                                         [v (in-list '(-1 0 1))])
                                        (define neighbor (+ h (* +i v)))
                                        (hash-ref vals (+ c neighbor) 0))))))

(define (larger-sum x)
  (for*/first ([n (in-naturals 1)]
               #:when (> (neighbor-sum n) x))
              (neighbor-sum n)))