#lang br/quicklang ;; http://adventofcode.com/2016/day/14
(require openssl/md5)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb SALT)
  #'(#%module-begin
     (solve SALT)
     (parameterize ([key-stretch 2017])
       (solve SALT))))

(define key-stretch (make-parameter 0))
(require sugar/cache)
(define/caching (get-hash salt i)
  (for/fold ([str (string-downcase (format "~a~a" salt i))])
            ([i (in-range (key-stretch))])
    (md5 (open-input-string str))))

(define (valid? hash salt i)
  (let* ([triple-char-pat (pregexp "(.)\\1\\1")]
         [result (regexp-match triple-char-pat hash)])
    (and result
         (let* ([repeated-char (cadr result)]
                [penta-char-pat (pregexp (format "(~a)\\1\\1\\1\\1" repeated-char))])
           (for/or ([idx (in-range (add1 i) (+ 1001 i))])
             (regexp-match penta-char-pat (get-hash salt idx)))))))

(define (solve salt)
  (caar
   (for/fold ([keys empty])
             ([i (in-naturals)]
              #:break (= (length keys) 64))
     (define hash (get-hash salt i))
     (if (valid? hash salt i)
         (report* i hash (cons (cons i hash) keys))
         keys))))
