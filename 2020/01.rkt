#lang br
(require racket/file rackunit)

(define (solve combo-length)
  (for/first ([c (in-combinations (map string->number (file->lines "01.rktd")) combo-length)]
            #:when (eq? 2020 (apply + c)))
  (apply * c)))

(check-equal? (solve 2) 1007331)
(check-equal? (solve 3) 48914340)