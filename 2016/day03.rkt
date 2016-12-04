#lang br/quicklang
(module+ reader
  (provide read-syntax)
  (define (read-syntax path port)
    (strip-bindings
     #`(module day01-mod "day03.rkt"
         #,@(for*/list ([triangle-str (in-list (string-split (port->string port) "\n"))])
                       `(triangle ,@(string-split triangle-str)))))))

(define-macro (mb . TRIANGLES)
  #'(#%module-begin
     (length (filter valid-triangle? (list . TRIANGLES)))))
(provide (rename-out [mb #%module-begin]))

(define-macro (triangle A B C)
  #'(list (string->number A)
          (string->number B)
          (string->number C)))
(provide triangle)

(define (valid-triangle? triangle)
  (match-define (list a b c) triangle)
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ a c) b)))