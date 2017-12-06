#lang reader "../aoc-lang.rkt"

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb (STARS) (BANK ...))
  #`(#%module-begin
     (count-redists (list->vector '(BANK ...)) 'STARS)))

(define (redist starting-vec)
  (define vec (vector-copy starting-vec))
  (define max-blocks (vector-argmax values vec))
  (define start-at (vector-member max-blocks vec))
  (vector-set! vec start-at 0)
  (for ([block (in-range max-blocks)]
        [idx (in-cycle (shift-left-cycle (range (vector-length vec)) (add1 start-at)))])
    (vector-set! vec idx (+ (vector-ref vec idx) 1)))
  vec)

(define (count-redists bankvec stars)
  (let/ec exit
    (for/fold ([bankvecs-seen (list bankvec)])
              ([i (in-naturals)])
      (cond
        [(member (car bankvecs-seen) (cdr bankvecs-seen))
         => (λ (tail) (exit (if (eq? stars '★)
                                i
                                (- (length bankvecs-seen) (length tail)))))])
      (cons (redist (car bankvecs-seen)) bankvecs-seen))))

