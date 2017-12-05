#lang reader "../aoc-lang.rkt"

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb (STARS) (JMP) ...)
  #`(#%module-begin
     (escape (list->vector '(JMP ...)) 'STARS)))

(define (escape vec stars)
  (let/ec exit
    (for/fold ([pos 0])
              ([i (in-naturals)])
      (unless (<= 0 pos (sub1 (vector-length vec)))
        (exit i))
      (define jmp (vector-ref vec pos))
      (vector-set! vec pos (if (and (eq? stars '★★) (>= jmp 3))
                               (sub1 jmp)
                               (add1 jmp)))
      (+ pos jmp))))

