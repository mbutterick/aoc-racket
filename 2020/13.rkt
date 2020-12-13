#lang br
(require racket/file rackunit)

(match-define (cons target buses-all)
  (for/list ([tok
              (in-port read
                       (open-input-string
                        (string-replace (file->string "13.rktd") "," " ")))])
    tok))

(define buses (filter integer? buses-all))

(define (overshoot bus)
  (for/last ([i (in-naturals)]
             #:final (> (* i bus) target))
    (- (* i bus) target)))

(define winner (argmin overshoot buses))

(check-equal? (* winner (overshoot winner)) 246)