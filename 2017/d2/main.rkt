#lang br/quicklang
(require "../helper.rkt")

(provide read-syntax)
(define (read-syntax path port)
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([line (in-lines port)])
                          (with-input-from-string line (λ ()
                                                         (for/list ([datums (in-port)])
                                                           datums)))))))

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
