#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) (Generator X starts with NUM) ...)
  #`(#%module-begin
     (time (STARS 'NUM ...))))

(define (lower-16-bits x) (bitwise-bit-field x 0 16))

(define (generator-fold start factor mod)
  (for/fold ([val start])
            ([i (in-naturals)]
             #:break (and (positive? i) (zero? (modulo val mod))))
    (remainder (* factor val) 2147483647)))

(define (generator-base a-start b-start rounds [modulo-a 1] [modulo-b 1])
  (for/fold ([a-start a-start]
             [b-start b-start]
             [sum 0]
             #:result sum)
            ([i (in-range rounds)])
    (define a (generator-fold a-start 16807 modulo-a))
    (define b (generator-fold b-start 48271 modulo-b))
    (values a b (+ sum (if (= (lower-16-bits a) (lower-16-bits b)) 1 0)))))

(define (★ a-start b-start) (generator-base a-start b-start (* 40 1000000)))

(define (★★ a-start b-start) (generator-base a-start b-start (* 5 1000000) 4 8))