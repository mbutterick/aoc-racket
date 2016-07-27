#lang typed/racket
(require typed/rackunit typed/openssl/md5)
(provide (all-defined-out))

(: q1 (-> String (U #f Natural)))
(define (q1 str)
  (for/or : (U #f Natural) ([i : Natural (in-naturals)])
    (define md5-key (string-append str (~a i)))
    (define md5-hash (md5 (open-input-string md5-key)))
    (and (string-prefix? md5-hash "00000") i)))

(: q2 (-> String (U #f Natural)))
(define (q2 str)
  (for/or : (U #f Natural) ([i : Natural (in-naturals)])
    (define md5-key (string-append str (~a i)))
    (define md5-hash (md5 (open-input-string md5-key)))
    (and (string-prefix? md5-hash "000000") i)))

(module+ test
  (define input-str (file->string "../day04-input.txt"))
  (check-equal? (q1 input-str) 346386)
  (check-equal? (q2 input-str) 9958218))
