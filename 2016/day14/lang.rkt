#lang br/quicklang
(require openssl/md5)
;; http://adventofcode.com/2016/day/13
(require graph)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb SALT)
  #'(#%module-begin
     (md5 (open-input-string SALT))))