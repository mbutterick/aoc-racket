#lang br/quicklang ;; http://adventofcode.com/2016/day/15
(require openssl/md5)
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
  (with-pattern ([(DISC# ...) (generate-temporaries #'DISCS)]
                 [(DISC-SLOTS ...) #'DISCS])
  #'(for/or ([DISC# (in-cycle DISC-SLOTS)] ...
             [i (in-naturals)]
             #:when (= DISC# ...))
      i)))

(require sugar/list)
(define-macro (disc TIME-OFFSET SIZE _ START)
  #'(shift (range SIZE) (modulo (- (+ START TIME-OFFSET)) SIZE) #f #t))
(provide disc)