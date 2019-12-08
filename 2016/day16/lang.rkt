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
     (time (display (list->string (checksum (fill-disk (string->list INIT) (string->number SIZE))))))))

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

(define (checksum cs)
  (define cvec (list->vector cs))
  (let loop ([cvec cvec])
    (match (vector-length cvec)
      [(? odd?) (vector->list cvec)]
      [len
       (define newvec (make-vector (/ len 2) #\0))
       (for ([idx (in-range 0 (vector-length cvec) 2)]
             #:when (char=? (vector-ref cvec idx) (vector-ref cvec (add1 idx))))
            (vector-set! newvec (/ idx 2) #\1))
       (loop newvec)])))
  
