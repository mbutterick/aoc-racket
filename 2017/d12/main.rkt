#lang reader "../aoc-lang.rkt"
(require graph)
(provide (rename-out [#%mb #%module-begin]))

(define-macro (#%mb (STARS) (NUM <-> . NUMS) ...)
  #'(#%module-begin
     (time
      (define g (unweighted-graph/undirected null))
      (for-each (curry add-edge! g NUM) (list . NUMS)) ...     
      (if (eq? 'STARS '★)
          (programs-in-group g 0)
          (number-of-groups g (list NUM ...))))))

(define (programs-in-group g x) (length (group-of g x)))

(define (group-of g x)
  (define-values (connects _) (dijkstra g x))
  (for/list ([(k v) (in-mutable-hash connects)]
             #:when (integer? v))
    k))

(define (number-of-groups g nums)
  (for/fold ([nums-seen null]
             [group-count 0]
             #:result group-count)
            ([num (in-list nums)]
             #:unless (memv num nums-seen))
    (values (append (group-of g num) nums-seen) (add1 group-count))))

