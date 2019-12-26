#lang br
(require racket/file racket/dict rackunit)

(define (bug? x) (char=? x #\#))

(define (parse-grid lns)
  (for*/list ([(ln row) (in-indexed lns)]
              [(c col) (in-indexed ln)])
    (cons (make-rectangular col row) c)))

(define (adjacent-bugs g tile)
  (for/sum ([delta '(+i -i 1 -1)]
            #:when (bug? (dict-ref g (+ tile delta) #\.)))
    1))

(define (step g)
  (for*/list ([row (in-range 5)]
              [col (in-range 5)])
    (define tile (make-rectangular col row))
    (cons tile
          (match (cons (dict-ref g tile) (adjacent-bugs g tile))
            [(cons #\# 1) #\#]
            [(cons #\# _) #\.]
            [(cons #\. (or 1 2)) #\#]
            [(cons other _) other]))))

(define (biodiversity-rating g)
  (for/sum ([(rec idx) (in-indexed g)]
            #:when (bug? (cdr rec)))
    (expt 2 idx)))

(define (solve lns)
  (let loop ([gs (list (parse-grid lns))])
    (define next-g (step (car gs)))
    (cond
      [(member next-g gs) (biodiversity-rating next-g)]
      [else (loop (cons next-g gs))])))

(check-eq?
 (solve (string-split "....#
#..#.
#..##
..#..
#....")) 2129920)

;; 1
(check-eq? (solve (file->lines "24.rktd")) 18407158)