#lang br/quicklang

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for*/list ([triangle-str (in-list (string-split (port->string port) "\n"))])
                     `(triangle ,@(string-split triangle-str))))))
(provide read-syntax)

(define-macro (mb . TRIANGLES)
  #'(#%module-begin
     (length (filter valid-triangle? (list . TRIANGLES)))))
(provide (rename-out [mb #%module-begin]))

(define-macro (triangle A B C)
  #'(map string->number (list A B C)))
(provide triangle)

(define (valid-triangle? triangle)
  (match-define (list a b c) triangle)
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ a c) b)))