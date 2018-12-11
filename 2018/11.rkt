#lang debug br
(require sugar/cache)

(define (hundreds-digit x)
  (quotient (modulo x 1000) 100))

(define current-serial (make-parameter 0))

(define/caching (power-at x y)
  (define rack-id (+ x 10))
  (- (hundreds-digit (* (+ (* rack-id y) (current-serial)) rack-id)) 5))

(define (grid-scores size-min [size-max size-min])
  (for*/list ([side (in-range size-min (add1 size-max))]
              [x (in-range 1 (- 301 side))]
              [y (in-range 1 (- 301 side))])
    (list x y
          (for*/sum ([x (in-range x (+ side x))]
                     [y (in-range y (+ side y))])
            (power-at x y)))))

(define (★)
  (parameterize ([current-serial 7803])
               (take (argmax third (grid-scores 3)) 2)))

#;(grid-scores 1 300)

#;(define (★★)
    )
#;(★★)

(module+ test
  (require rackunit)
  #;(check-equal? (time (★)) '(20 51))
  #;(check-equal? (time (★★)) _))