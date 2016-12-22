#lang br/quicklang ;; http://adventofcode.com/2016/day/22
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   (let ([lines (string-split (port->string port) "\n")])
     #`(module mod "lang.rkt"
         #,@(for/list ([args (in-list (map string-split (cddr lines)))])
              `(node ,@args))))))

(define-macro (mb . NODES)
  #'(#%module-begin
     (define nodes (list . NODES))
     (count-viable-pairs nodes)))

(struct $node (pos used avail) #:transparent)
    
(define-macro (node NAME _ USED AVAIL _)
  #'($node
     (apply (λ(r i) (+ (string->number r)
                       (* (string->number i) +i))) (regexp-match* #px"\\d+" NAME))
     (string->number (string-trim USED "T"))
     (string->number (string-trim AVAIL "T"))))
(provide node)

(define (count-viable-pairs nodes)
  (for*/sum ([a (in-list nodes)]
             [b (in-list nodes)]
             #:when (and (not (zero? ($node-used a)))
                         (not (equal? ($node-pos a) ($node-pos b)))
                         (<= ($node-used a) ($node-avail b))))
    1))