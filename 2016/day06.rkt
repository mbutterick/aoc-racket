#lang br/quicklang

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "day06.rkt"
       (solve #,@(string-split (port->string port))))))
(module+ reader (provide read-syntax))

(require sugar/list)
(define (solve . ws)
  (define-values (s1 s2)
    (for/lists (acc acc2) ([vert-cs (in-list (apply map list (map string->list ws)))])
     (define freqs (hash->list (frequency-hash vert-cs)))
     (values (car (argmax cdr freqs)) (car (argmin cdr freqs)))))
  (displayln (list->string s1))
  (displayln (list->string s2)))
(provide solve)

(provide #%module-begin)