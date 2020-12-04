#lang br
(require racket/file rackunit)

(define (trees-in-slope horiz vert)
  (for/fold ([mod 0]
             [sum 0]
             #:result sum)
            ([(ln lidx) (in-indexed (file->lines "03.rktd"))]
             #:when (zero? (modulo lidx vert)))
    (values (+ mod horiz)
            (if (char=? #\# (string-ref ln (modulo mod (string-length ln)))) (add1 sum) sum))))

(check-equal? (trees-in-slope 3 1) 151)

(check-equal? (apply * (list
                        (trees-in-slope 1 1)
                        (trees-in-slope 3 1)
                        (trees-in-slope 5 1)
                        (trees-in-slope 7 1)
                        (trees-in-slope 1 2))) 7540141059)