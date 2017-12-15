#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) (Generator X starts with NUM) ...)
  #`(#%module-begin
     (apply STARS '(NUM ...))))

(define (lower-16-bits x) (bitwise-bit-field x 0 16))

(define (generator-base a b rounds [modulo-a 1] [modulo-b 1])
  (for/fold ([a-last a]
             [b-last b]
             [sum 0]
             #:result sum)
            ([i (in-range rounds)])
    (define a (for/fold ([a a-last])
                        ([i (in-naturals)]
                         #:break (and (positive? i) (zero? (modulo a modulo-a))))
                (remainder (* 16807 a) 2147483647)))
    (define b (for/fold ([b b-last])
                        ([i (in-naturals)]
                         #:break (and (positive? i) (zero? (modulo b modulo-b))))
                (remainder (* 48271 b) 2147483647)))
    (values a b (+ sum (if (= (lower-16-bits a) (lower-16-bits b)) 1 0)))))

(define (★ a-start b-start) (generator-base a-start b-start 40000000))

(define (★★ a-start b-start) (generator-base a-start b-start 5000000 4 8))