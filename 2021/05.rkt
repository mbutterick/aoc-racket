#lang br
(require racket/file sugar rackunit racket/set)

(define lines (map (λ (s) (map (λ (s2) (define ints (map string->number (string-split s2 ",")))
                                 (+ (first ints) (* +i (second ints)))) (string-split s " -> "))) (file->lines "05.rktd")))

(define (Line-not-diagonal line)
  (match line
    [(list left right) (or (= (real-part left) (real-part right)) (= (imag-part left) (imag-part right)))]))

;; why does make-polar cause the solution to run faster?
;; both functions create an imaginary number
;; make-polar is slower to create the numbers (because it has to call trig functions)
;; but storing the polar numbers with frequency-hash is much faster
;; using inexact coefficients makes make-rectangular go faster
;; but still not as fast as make-polar
(define go-fast? #true)
(define imag-func (if go-fast? make-polar make-rectangular))

(define (expand line)
  (match-define (list x1 x2) (map real-part line))
  (match-define (list y1 y2) (map imag-part line))
  (cond
    [(= x1 x2)
     (for/list ([i (in-range (apply min (list y1 y2)) (add1 (apply max (list y1 y2))))])
       (imag-func x1 i))]
    [(= y1 y2)
     (for/list ([i (in-range (apply min (list x1 x2)) (add1 (apply max (list x1 x2))))])
       (imag-func i y1))]
    [else (for/list ([x (in-range x1 ((if (> x1 x2) sub1 add1) x2) (if (> x1 x2) -1 1))]
                     [y (in-range y1 ((if (> y1 y2) sub1 add1) y2) (if (> y1 y2) -1 1))])
            (imag-func x y))]))

(define (calc-result points)
  (length (filter (λ (x) (>= x 2)) (hash-values (time (frequency-hash points))))))

(check-equal? (calc-result (append-map expand (filter Line-not-diagonal lines))) 6113)
(check-equal? (calc-result (append-map expand lines)) 20373)