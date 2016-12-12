#lang br/quicklang
;; http://adventofcode.com/2016/day/12

(provide read-syntax)

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (equal? str "")))
            (format-datum `(handle ,@(map string->symbol (string-split str))))))))

(define (mb . INSTS)
  #'(#%module-begin
     (define insts (list . INSTS))
     (define regs (make-hash '((a . 0)(b . 0)(c . 0)(d . 0))))
     (let loop ([ptr 0])
       (define inst (list-ref insts ptr))
       (inst regs))))
(provide (rename-out [mb #%module-begin]))

(define-macro-cases handle
  [(_ cpy X Y) #'(λ(regs) 42)])
(provide handle)
       