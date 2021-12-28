#lang br
(require racket/file sugar rackunit)

(define posns (map string->number (string-split (file->string "07.rktd") ",")))

(define/caching (gauss-summation x) (* (/ x 2) (+ x 1)))

(define (fuel-cost alignment [post-proc values])
  (foldl (λ (posn res) (+ res (post-proc (abs (- alignment posn))))) 0 posns))

(define possible-alignments (remove-duplicates posns))
(check-equal? (apply min (map fuel-cost possible-alignments)) 349357)
(check-equal? (apply min (map (λ (pa) (fuel-cost pa gauss-summation)) possible-alignments)) 96708205)