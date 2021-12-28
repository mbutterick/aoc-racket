#lang br
(require racket/file sugar rackunit)

(define fish (map string->number (string-split (file->string "06.rktd") ",")))

(define (simulate fish days)
  (for/fold ([freqs (frequency-hash fish)]
             #:result (apply + (hash-values freqs)))
            ([d (in-range days)])
    (hash-set*
     (for/hasheq ([(k v) (in-hash freqs)]
                  #:unless (zero? k))
       (values (sub1 k) v))
     6 (+ (hash-ref freqs 7 0) (hash-ref freqs 0 0))
     8 (hash-ref freqs 0 0))))

(check-equal? (simulate fish 80) 361169)
(check-equal? (simulate fish 256) 1634946868992)