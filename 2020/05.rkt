#lang br
(require racket/file srfi/13 rackunit)

(define (seat-id seat)
  (for/sum ([(c i) (in-indexed (string-reverse seat))]
            #:when (memv c '(#\R #\B)))
    (arithmetic-shift 1 i)))

(define seat-ids (map seat-id (file->lines "05.rktd")))

(check-equal? (apply max seat-ids) 915)

(check-equal?
 (for/first ([first (sort seat-ids <)]
             [second (cdr (sort seat-ids <))]
             #:when (= 2 (- second first)))
   (sub1 second))
 699)