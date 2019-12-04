#lang br
(require racket/file rackunit)

(match-define (list low high)
  (map string->number (string-split (file->string "04.rktd") "-")))

(define (double-digit? num)
  (define numstr (number->string num))
  (match (for/list ([c0 (in-string numstr)]
                    [c1 (in-string numstr 1)]
                    #:when (char=? c0 c1))
           (string->number (string c0)))
    [(? null?) #false]
    [nums nums]))

(define (num->digits num)
  (for/list ([c (in-string (number->string num))])
    (string->number (string c))))

(define (monotonic? num)
  (apply <= (num->digits num)))

;; 1
(check-eq?
 (for/sum ([num (in-range low (add1 high))]
           #:when (and (double-digit? num) (monotonic? num)))
   1)
 1079)

(define (triple-digit? num digit)
  (define numstr (number->string num))
  (define digitc (car (string->list (number->string digit))))
  (for/or ([c0 (in-string numstr)]
           [c1 (in-string numstr 1)]
           [c2 (in-string numstr 2)])
    (char=? c0 c1 c2 digitc)))

;;2
(check-eq?
 (for/sum ([num (in-range low (add1 high))]
           #:when (and (for/or ([digits (in-value (double-digit? num))]
                                #:when digits
                                [digit (in-list digits)])
                         (not (triple-digit? num digit)))
                       (monotonic? num)))
   1)
 699)

