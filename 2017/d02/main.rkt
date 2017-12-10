#lang reader "../aoc-lang.rkt"

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb (STARS) (NUMBER ...) ...)
  #'(#%module-begin (checksum 'STARS '((NUMBER ...) ...))))

(define (checksum stars intss)
  (define (max-min-diff ints) (- (apply max ints) (apply min ints)))
  (define (no-remainder ints)
    (for*/first ([duo (in-combinations ints 2)]
                 [result (in-value (apply / (sort duo >)))]
                 #:when (integer? result))
      result))
  (define row-proc (if (eq? stars '★) max-min-diff no-remainder))
  (apply + (map row-proc intss)))
