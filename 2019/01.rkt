#lang br
(require racket/file rackunit)

(define (fuel-required mass) (- (floor (/ mass 3)) 2))

(define masses
  (for/list ([ln (in-port read (open-input-file "01.rktd"))])
            ln))

;; 1
(check-eq? (apply + (map fuel-required masses)) 3167282)

(define (recursive-fuel mass [acc 0])
  (match (fuel-required mass)
    [(? positive? fuel)
     (recursive-fuel fuel (+ acc fuel))]
    [else acc]))

;; 2
(check-eq? (apply + (map recursive-fuel masses)) 4748063)