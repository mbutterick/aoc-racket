#lang br/quicklang
(require openssl/md5)

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "day05.rkt"
       (solve-part-a #,(string-trim (port->string port))))))
(module+ reader (provide read-syntax))

(provide #%module-begin)

(define (solve-part-a key)
  (display "part a solution: ")
  (for*/fold ([password-cs empty])
            ([idx (in-naturals)]
             [this-key (in-value (format "~a~a" key idx))]
             #:break (= 8 (length password-cs)))
    (define this-hash (md5 (open-input-string this-key)))
    (if (string-prefix? this-hash "00000")
        (let ([next-str (substring this-hash 5 6)])
          (display next-str)
          (cons next-str password-cs))
        password-cs))
  (displayln ""))
(provide solve-part-a)
