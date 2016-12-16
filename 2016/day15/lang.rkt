#lang br/quicklang ;; http://adventofcode.com/2016/day/15
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([line (in-list (string-split (port->string port) "\n"))])
            `(disc ,@(map string->number (regexp-match* #px"\\d+" line)))))))

(define-macro (mb . DISCS)
  #'(#%module-begin
     (solve . DISCS)))

(define-macro (solve . DISCS)
  (with-pattern ([(DISC-ID ...) (generate-temporaries #'DISCS)]
                 [(DISC-SLOTS ...) #'DISCS])
    #'(for/first ([DISC-ID (in-cycle DISC-SLOTS)] ...
                  [i (in-naturals)]
                  #:when (= 0 DISC-ID ...))
        i)))

(require sugar/list)
(define-macro (disc TIME-OFFSET SIZE _ START)
  #'(shift-left-cycle (range SIZE) (+ START TIME-OFFSET)))
(provide disc)