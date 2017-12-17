#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) (STEP-SIZE))
  #`(#%module-begin
     (time (STARS STEP-SIZE))))

(define (★ step-size)
  (define iterations 2017)
  (for/fold ([buf '(0)]
             [current-pos 0]
             #:result (second (member iterations buf)))
            ([i (in-range iterations)])
    (define pos (add1 (modulo (+ current-pos step-size) (add1 i))))
    (define-values (head tail) (split-at buf pos))
    (values (append head (list (add1 i)) tail) pos))) 

(define (★★ step-size)
  (for/fold ([in-first-pos #f]
             [current-pos 0]
             #:result in-first-pos)
            ([i (in-range (* 50 1e6))])
    (define pos (add1 (modulo (+ current-pos step-size) (add1 i))))
    (values (if (= 1 pos) (add1 i) in-first-pos) pos))) 
