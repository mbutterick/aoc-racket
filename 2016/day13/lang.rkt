#lang br/quicklang
;; http://adventofcode.com/2016/day/13
(require graph)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string->number (string-trim (port->string port))))))

(define-macro (mb NUM)
  #'(#%module-begin
     (solve 50 NUM)
     (solve2 NUM)))

(define (solve dim num)
  (define open? (make-open-pred num))
  (define g (undirected-graph '((0 0))))
  (for* ([y (in-range dim)]
         [x (in-range dim)]
         #:when (open? (cons x y)))
    (when (open? (cons (add1 x) y))
      (add-edge! g (cons x y) (cons (add1 x) y)))
    (when (open? (cons x (add1 y)))
      (add-edge! g (cons x y) (cons x (add1 y)))))
  (define path (fewest-vertices-path g '(1 . 1) '(31 . 39)))
  (displayln (and path (sub1 (length path)))))

(define (solve2 num)
  (define open? (make-open-pred num))
  (define (nonnegative? pt) (and (not (negative? (car pt)))
                                 (not (negative? (cdr pt)))
                                 pt))
  (let loop ([all-visited-pts empty]
             [last-visited-pts '((1 . 1))]
             [step 0])
    (cond
      [(= step 50)
       (length (remove-duplicates (append all-visited-pts last-visited-pts)))]
      [else
       (loop (append last-visited-pts all-visited-pts)
             (append*
              (for/list ([lvp (in-list last-visited-pts)])
                (match-define (cons x y) lvp)
                (for/list ([pt (in-list (list (cons (add1 x) y)
                                              (cons x (add1 y))
                                              (cons (sub1 x) y)
                                              (cons x (sub1 y))))]
                           #:when (and (nonnegative? pt)
                                       (open? pt)
                                       (not (member pt all-visited-pts))))
                  pt)))
             (add1 step))])))

(define (make-open-pred num)
  (λ(pt)
    (match-define (cons x y) pt)
    (define sum
      (+ (* x x) (* 3 x) (* 2 x y) y (* y y) num))
    (even?
     (for/sum ([c (in-string (format "~b" sum))]
               #:when (char=? #\1 c))
       1))))