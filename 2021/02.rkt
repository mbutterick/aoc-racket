#lang br
(require racket/file sugar/list rackunit)

(define instructions
  (slice-at (for/list ([datum (in-port read (open-input-file "02.rktd"))])
              datum) 2))

(define (solve matcher)
  (for/fold ([pos 0]
             [depth 0]
             [aim 0]
             #:result (* pos depth))
            ([i (in-list instructions)])
    (matcher pos depth aim i)))

(define (solve-1)
  (solve (λ (pos depth aim i)
           (match i
             [(list 'forward amt) (values (+ pos amt) depth aim)]
             [(list 'down amt) (values pos (+ depth amt) aim)]
             [(list 'up amt) (values pos (- depth amt) aim)]))))

(check-equal? (solve-1) 1488669)

(define (solve-2)
  (solve (λ (pos depth aim i)
           (match i
             [(list 'forward amt) (values (+ pos amt) (+ depth (* aim amt)) aim)]
             [(list 'down amt) (values pos depth (+ aim amt))]
             [(list 'up amt) (values pos depth (- aim amt))]))))

(check-equal? (solve-2) 1176514794)