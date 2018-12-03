#lang debug br

#;(define (★)
  )

#;(define (★★)
  )

#;(module+ test
  (require rackunit)
  (check-equal? (time (★)) 454)
  (check-equal? (time (★★)) 566))