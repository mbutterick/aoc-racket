#lang debug br

(define fp (open-input-file "03.txt"))

(struct pt (x y) #:transparent)
(struct rect (ul lr) #:transparent) 

(define (parse-claim ln)
  (match-define (list left top width height)
    (map string->number (cdr (regexp-match #px"(\\d+),(\\d+): (\\d+)x(\\d+)" ln))))
  (rect (pt left top) (pt (+ left width) (+ top height))))

(define claims
  (map parse-claim (port->lines fp)))

(define coverage (make-hash))
(for ([(claim idx) (in-indexed claims)])
  (define cidx (add1 idx))
  (match-define (rect (pt left top) (pt right bottom)) claim)
  (for* ([x (in-range left right)]
         [y (in-range top bottom)])
    (hash-update! coverage (pt x y) (λ (cidxs) (cons cidx cidxs))  empty)))

(define (★)
  (for/sum ([cidxs (in-list (hash-values coverage))]
            #:when (<= 2 (length cidxs)))
    1))

(define (★★)
  (define cidxss (hash-values coverage))
  (for/first ([cidx (in-range 1 (add1 (length claims)))]
              #:when (for/and ([cidxs (in-list cidxss)]
                               #:when (memv cidx cidxs))
                       (= (length cidxs) 1)))
    cidx))

(module+ main
  (require rackunit)
  (check-equal? (time (★)) 110827)
  (check-equal? (time (★★)) 116))