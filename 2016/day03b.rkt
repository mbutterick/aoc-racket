#lang br/quicklang
(require sugar/list)

(define (read-syntax path port)
    (define triads (slice-at (map string-split (string-split (port->string port) "\n")) 3))
    (define new-triples
      (slice-at (flatten (for/list ([triad (in-list triads)])
                                   (apply map list triad))) 3))                              
    (strip-bindings
     #`(module mod "day03b.rkt"
         #,@(for*/list ([triple (in-list new-triples)])
                       `(triangle ,@triple)))))
(module+ reader (provide read-syntax))

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