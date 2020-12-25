#lang br
(require racket/file rackunit)

(match-define (list card-pubkey door-pubkey) '(18356117 5909654))

(define (transform subject-number [break-proc void])
  (for/fold ([val 1]
             [count 0]
             #:result (cons val count))
            ([i (in-naturals)]
             #:break (break-proc val i))
    (values (remainder (* val subject-number) 20201227) (add1 count))))

(define card-loop-size (cdr (transform 7 (λ (val i) (eq? val card-pubkey)))))
(define door-loop-size (cdr (transform 7 (λ (val i) (eq? val door-pubkey)))))

(check-equal? (car (transform card-pubkey (λ (val i) (eq? i door-loop-size)))) 16902792) 