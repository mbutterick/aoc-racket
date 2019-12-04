#lang br
(require racket/file rackunit)

(match-define (list low high)
  (map string->number (string-split (file->string "04.rktd") "-")))

(define (find-doubles digits)
  (match (for/list ([d0 (in-list digits)]
                    [d1 (in-list (cdr digits))]
                    #:when (= d0 d1))
                   d0)
    [(? null?) #false]
    [dds (remove-duplicates dds)]))

(define (num->digits num)
  (for/list ([c (in-string (number->string num))])
            (string->number (string c))))

(define (increasing? digits) (apply <= digits))

;; 1
(check-eq?
 (for/sum ([digits (in-list (map num->digits (range low (add1 high))))])
          (match digits
            [(and (? find-doubles) (? increasing?)) 1]
            [_ 0]))
 1079)

(define (triple-digit? ds d)
  (and (pair? ds)
       (or (list-prefix? (list d d d) ds)
           (triple-digit? (cdr ds) d))))

;; 2
(check-eq?
 (for/sum ([digits (in-list (map num->digits (range low (add1 high))))])
          (match digits
            [(and (app find-doubles (? list? doubled-digits)) (? increasing?))
             #:when (for/or ([dd (in-list doubled-digits)])
                            (not (triple-digit? digits dd))) 1]
            [_ 0]))
 699)

