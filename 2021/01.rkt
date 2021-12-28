#lang br
(require racket/file rackunit)

(define depths (map string->number (file->lines "01.rktd")))

(define (positive-deltas depths)
  (filter positive?
          (for/list ([d1 (in-list depths)]
                     [d2 (in-list (cdr depths))])
            (- d2 d1))))

(check-equal? (length (positive-deltas depths)) 1167)

(define (trios depths)
  (for/list ([d1 (in-list depths)]
             [d2 (in-list (cdr depths))]
             [d3 (in-list (cddr depths))])
    (+ d1 d2 d3)))

(check-equal? (length (positive-deltas (trios depths))) 1130)