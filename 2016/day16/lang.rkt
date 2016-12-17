#lang br/quicklang ;; http://adventofcode.com/2016/day/16
(require openssl/md5)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(string-split (string-trim (port->string port)) ", "))))

(define-macro (mb SIZE INIT)
  #'(#%module-begin
     (display (list->string (checksum (fill-disk (string->list INIT) (string->number SIZE)))))))

(define (dragonize cs)
  (append cs '(#\0)
          (for/list ([c (in-list (reverse cs))])
            (if (eqv? c #\1)
                #\0
                #\1))))

(define (fill-disk init size)
  (let loop ([cs init])
    (if (>= (length cs) size)
        (take cs size)
        (loop (dragonize cs)))))

(require sugar/list)
(define (checksum cs)
  (let loop ([cs cs])
    (if (odd? (length cs))
        cs
        (loop
         (for/list ([match (in-list (slice-at cs 2))])
           (if (member match '((#\0 #\0) (#\1 #\1)))
               #\1
               #\0))))))
  
