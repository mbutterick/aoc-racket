#lang debug br

(struct pt (x y xv yv) #:transparent #:mutable)

(define pts
  (for/list ([ln (in-lines (open-input-file "10.txt"))])
    (apply pt
           (map string->number (regexp-match* #px"-?\\d+" ln)))))

(define (inc pt)
  (set-pt-x! pt (+ (pt-x pt) (pt-xv pt)))
  (set-pt-y! pt (+ (pt-y pt) (pt-yv pt)))
  pt)

(define (normalize pts)
  (define xmin (apply min (map pt-x pts)))
  (define ymin (apply min (map pt-y pts)))
  (for ([pt (in-list pts)])
    (set-pt-x! pt (- (pt-x pt) xmin))
    (set-pt-y! pt (- (pt-y pt) ymin))))

(define (fonted? pts)
  (define ys (map pt-y pts))
  (for/and ([y (in-list ys)])
    (<= 4 y 11)))

(require racket/draw racket/gui)


(define (print-pts pts)
  (define xmax (apply max (map pt-x pts)))
  (define ymax (apply max (map pt-y pts)))
  (define prs (map cons (map pt-x pts) (map pt-y pts)))
  (when (= #R (- xmax (apply min (map pt-x pts))) 61)
    #R xmax #R ymax
    (define target (make-bitmap (+ 3 xmax) (+ 3 ymax)))
    (define dc (new bitmap-dc% [bitmap target]))
    (for* ([y (in-range (+ 3 ymax))]
           [x (in-range (+ 3 xmax))]
           #:when (member (cons x y) prs))
      (send dc set-pixel x y (make-object color% "DarkSlateGray")))
    (make-object image-snip% target)))


(let loop ([idx 0][pts pts])
  (normalize pts)
  (display idx)
  (display (print-pts pts))
  (if (fonted? pts)
      (display (print-pts pts))
      (loop (add1 idx) (map inc pts))))


#;(define (★)
    )
#;(★)

#;(define (★★)
    )
#;(★★)

#;(module+ test
    (require rackunit)
    (check-equal? (time (★)) 454)
    (check-equal? (time (★★)) 566))