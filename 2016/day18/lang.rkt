#lang br/quicklang ;; http://adventofcode.com/2016/day/18
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb STR)
  #'(#%module-begin
     (define (traps cs)
       (length (filter (λ (c) (char=? #\. c)) cs)))
     (let loop ([cs (string->list STR)]
                [count (traps (string->list STR))]
                [i 0])
       (if (= i 399999) ; number of rows
           count
           (let* ([result (next-cs cs)]
                  [this-count (traps result)])
             (loop result (+ this-count count) (add1 i)))))))

(define (next-cs cs)
  (define adj-cs (append (list #\.) cs (list #\.)))
  (for/list ([c1 (in-list adj-cs)]
             [c2 (in-list (cdr adj-cs))]
             [c3 (in-list (cddr adj-cs))])
    (case (list c1 c2 c3)
      [((#\^ #\^ #\.) (#\. #\^ #\^) (#\^ #\. #\.) (#\. #\. #\^)) #\^]
      [else #\.])))
