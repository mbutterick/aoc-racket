#lang br/quicklang
(provide read-syntax)
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([inst (in-lines port)]
                     #:when (not (equal? inst "")))
            (format-datum '(~a) inst)))))

(provide (rename-out [mb #%module-begin]))
(define-macro (mb INST ...)
  #'(#%module-begin
     (define g (for/fold ([g (grid 50 6)])
               ([inst (in-list (list INST ...))])
       (inst g)))
     (for-each displayln (map (λ(gr) (map (λ(gri) (if (= gri 1) "X" " ")) gr)) g))
     (apply + (flatten g))))

(require (for-syntax racket/string))
(define-macro (rect ARG)
  (with-pattern ([(COLS ROWS) (map string->number (string-split (symbol->string (syntax->datum #'ARG)) "x"))])
    #'(curryr fill COLS ROWS)))
(provide rect)

(define-macro (rotate DIR WHICH-RAW by DIST)
  (with-pattern ([PROC (prefix-id "shift-" #'DIR)]
                 [WHICH (string->number (car (string-split (symbol->string (syntax->datum #'WHICH-RAW)) #rx"[xy]=")))])
    #'(curryr PROC WHICH DIST)))
(provide rotate)

(define (grid x y)
  (make-list y (make-list x 0)))

(define (fill g cols rows)
  (for/list ([(row ridx) (in-indexed g)])
    (for/list ([(col cidx) (in-indexed (list-ref g ridx))])
      (if (and (< ridx rows) (< cidx cols))
          1
          col))))

(require sugar/list)
(define (shift-row g which dist)
  (for/list ([(row ridx) (in-indexed g)])
    (if (= which ridx)
        (shift row dist #f #t)
        row)))
(provide shift-row)

(define (shift-column g which dist)
  (apply map list (shift-row (apply map list g) which dist)))
(provide shift-column)