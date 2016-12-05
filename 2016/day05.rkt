#lang br
(require openssl/md5)

(define key "ojvtpuvg")

(let loop ([idx 0][password-cs empty])
  (cond
    [(= 8 (length password-cs))
     (apply string-append (reverse password-cs))]
    [else
     (define this-key (format "~a~a" key idx))
     (define next-hash (md5 (open-input-string this-key)))
     (if (string-prefix? next-hash "00000")
         (loop (add1 idx) (cons (substring next-hash 5 6) password-cs))
         (loop (add1 idx) password-cs))]))
