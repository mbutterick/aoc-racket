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
  (for* ([x (in-range dim)]
         [y (in-range dim)]
         #:when (open? (list y x)))
    (when (open? (list (add1 y) x))
      (add-edge! g (list y x) (list (add1 y) x)))
    (when (open? (list y (add1 x)))
      (add-edge! g (list y x) (list y (add1 x)))))
  (define path (fewest-vertices-path g '(1 1) '(31 39)))
  (displayln (and path (sub1 (length path)))))

(define (solve2 num)
  (define open? (make-open-pred num))
  (define (nonnegative? pt) (and (not (negative? (car pt)))
                                 (not (negative? (cadr pt)))
                                 pt))
  (let loop ([all-visited-pts empty]
             [last-visited-pts '((1 1))]
             [step 0])
    (cond
      [(= step 50)
       (length (remove-duplicates (append all-visited-pts last-visited-pts)))]
      [else
       (loop (append last-visited-pts all-visited-pts)
             (append*
              (for/list ([lvp (in-list last-visited-pts)])
                (match-define (list x y) lvp)
                (for/list ([pt (in-list (list (list (add1 x) y)
                                              (list x (add1 y))
                                              (list (sub1 x) y)
                                              (list x (sub1 y))))]
                           #:when (and (nonnegative? pt)
                                       (open? pt)
                                       (not (member pt all-visited-pts))))
                  pt)))
             (add1 step))])))

(define (make-open-pred num)
  (λ(pt)
    (match-define (list x y) pt)
    (define sum
      (+ (* x x) (* 3 x) (* 2 x y) y (* y y) num))
    (even?
     (for/sum ([c (in-string (format "~b" sum))]
               #:when (char=? #\1 c))
       1))))