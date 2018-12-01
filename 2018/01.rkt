#lang debug br
(require racket/set)

(define fp (open-input-file "01.txt"))

(define frequencies
  (map string->number (port->lines fp)))

(define (★)
  (apply + frequencies))

(define (★★)
  (for/fold ([last-sum 0]
             [sums (set)]
             #:result last-sum)
            ([freq (in-cycle frequencies)]
             #:break (set-member? sums last-sum))
    (values (+ freq last-sum) (set-add sums last-sum))))

(module+ main
  (require rackunit)
  (check-equal? (time (★)) 454)
  (check-equal? (time (★★)) 566))