#lang br/quicklang ;; http://adventofcode.com/2016/day/18
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb NUM-STR)
  #'(#%module-begin
     #;(displayln (solve (string->number NUM-STR) #f))
     (displayln (solve (string->number NUM-STR) #t))))

(define (idx-after vec x)
  (or
   (for/first ([idx (in-range (modulo (add1 x) (vector-length vec)) (vector-length vec))]
               #:when (vector-ref vec idx))
     idx)
   (idx-after vec -1)))

(define (solve num [circle? #f])
  (define elves (make-vector num #t))
  (let loop ([taker 0][elves-left num])
    (cond
      [(= elves-left 2) (add1 taker)]
      [else
       (define giver (for/fold ([elf taker])
                               ([i (in-range (if circle? (floor (/ elves-left 2)) 1))])
                       (idx-after elves elf)))
       (vector-set! elves giver #f)
       (loop (idx-after elves taker) (sub1 elves-left))])))

