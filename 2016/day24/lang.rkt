#lang br/quicklang ; http://adventofcode.com/2016/day/24
(require graph)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb STR)
  #'(#%module-begin
     (solve STR)))

(define (open? c) (not (equal? c "#")))

(define (solve str)
  (define g (undirected-graph empty))
  (define ptcols
    (for*/list ([(row ridx) (in-indexed (string-split str))]
                [(col cidx) (in-indexed (regexp-match* #rx"." row))]
                [pt (in-value (+ cidx (* +i ridx)))]
                #:when (open? col))
      (list pt col)))
  (for ([p (in-list ptcols)])
    (let ([right (assoc (+ (car p) 1) ptcols)])
      (when right (add-edge! g (car p) (car right))))
    (let ([down (assoc (+ (car p) +i) ptcols)])
      (when down (add-edge! g (car p) (car down)))))
  (define colpts (apply hash (flatten (map reverse ptcols))))
  (define highest-num (apply max (filter number? (map (compose1 string->number cadr) ptcols))))
  (displayln
   (argmin cdr
           (for/list ([path-digits (in-permutations (range 1 (add1 highest-num)))])
             (cons path-digits
                   (for/sum ([start-num (in-list (cons 0 path-digits))]
                             [end-num (in-list path-digits)])
                     (define start (hash-ref colpts (~a start-num)))
                     (define stop (hash-ref colpts (~a end-num)))
                     (define spath (fewest-vertices-path g start stop))
                     (sub1 (length spath)))))))
  (displayln
   (argmin cdr
           (for/list ([path-digits (in-permutations (range 1 (add1 highest-num)))])
             (cons path-digits
                   (for/sum ([start-num (in-list (append '(0) path-digits '(0)))]
                             [end-num (in-list (append path-digits '(0)))])
                     (define start (hash-ref colpts (~a start-num)))
                     (define stop (hash-ref colpts (~a end-num)))
                     (define spath (fewest-vertices-path g start stop))
                     (sub1 (length spath))))))))
