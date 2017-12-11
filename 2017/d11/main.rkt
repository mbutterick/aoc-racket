#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]))

(define-macro (#%mb (STARS) (HEX ...) ...)
  #'(#%module-begin
     ((if (eq? 'STARS '★) one-star two-star) (list HEX ...)) ...))

(define origin '(0 0 0))
(define ne '(1 0 -1)) (define sw '(-1 0 1))
(define n '(0 1 -1)) (define s '(0 -1 1))
(define nw '(-1 1 0)) (define se '(1 -1 0))
(provide ne sw n s nw se)

(define (dist h1 h2) (/ (apply + (map abs (map - h1 h2))) 2))

(define (one-star hexes) (dist origin (apply map + hexes)))

(define (two-star hexes)
  (for/fold ([sum origin]
             [max-dist 0]
             #:result max-dist)
            ([h (in-list hexes)])
    (define this-sum (map + h sum))
    (values this-sum (max max-dist (dist origin this-sum)))))