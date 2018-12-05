#lang debug br
(require racket/file racket/set)

(define fp (open-input-file "05.txt"))

(define fpbs (port->bytes fp))

(define (react fpbs)
  (let/ec exit
    (for/fold ([bs fpbs])
              ([i (in-naturals)])
      (or
       (for/first ([(left lidx) (in-indexed bs)]
                   [right (in-bytes (subbytes bs 1))]
                   #:when (= 32 (abs (- left right))))
         (bytes-append (subbytes bs 0 lidx) (subbytes bs (+ lidx 2))))
       (exit (bytes-length bs))))))

(define (★)
  (react fpbs))

(define possible-units
  (map char->integer (remove-duplicates (map char-upcase (map integer->char (bytes->list fpbs))) char=?)))

(define (remove-unit fbps unit)
  (apply bytes
         (for/list ([b (in-bytes fbps)]
                    #:unless (or (= b unit)
                                 (= b (+ unit 32))))
           b)))

;; very slow.
(define (★★)
  (argmin cdr
          (for/list ([(unit idx) (in-indexed possible-units)])
            (cons unit (react (remove-unit fpbs unit))))))

(module+ test
    (require rackunit)
    (check-equal? (time (★)) 10564)
    (check-equal? (time (★★)) 6336))