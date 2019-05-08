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
     (solve 50 NUM) ; 50 is arbitrarily large space to search
     (solve2 NUM)))

(define starting-pt 1+1i)

(define (solve dim num)
  (define open? (make-open-pred num))
  (define g (undirected-graph (list starting-pt)))
  (for* ([row (in-range dim)]
         [col (in-range dim)]
         [p (in-value (+ col (* +i row)))]
         #:when (open? p))
        (when (open? (+ p 1)) (add-edge! g p (+ p 1)))
        (when (open? (+ p +i)) (add-edge! g p (+ p +i))))
  (define path (fewest-vertices-path g 1+1i 31+39i))
  (displayln (and path (sub1 (length path)))))

(define (solve2 num)
  (define open? (make-open-pred num))
  (define (nonnegative? pt) (and (not (negative? (real-part pt)))
                                 (not (negative? (imag-part pt)))
                                 pt))
  (let take-step ([all-visited-pts empty]
                  [last-visited-pts (list starting-pt)]
                  [count 0])
    (if (= count 50)
        (length (remove-duplicates (append all-visited-pts last-visited-pts)))
        (take-step
         (append last-visited-pts all-visited-pts)
         (flatten
          (for*/list ([p (in-list last-visited-pts)]
                      [next-p (in-list (map (curry + p) '(1 -1 +i -i)))]
                      #:when (and (nonnegative? next-p)
                                  (open? next-p)
                                  (not (member next-p all-visited-pts))))
                     next-p))
         (add1 count)))))

(define (make-open-pred num)
  (λ (pt)
    (define col (real-part pt))
    (define row (imag-part pt))
    (define sum
      (+ (* col col) (* 3 col) (* 2 col row) row (* row row) num))
    (even? (length (regexp-match* "1" (format "~b" sum))))))