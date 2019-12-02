#lang br
(require racket/file rackunit)

(define (string->regs str)
  (list->vector (map string->number (string-split str ","))))

(define (solve regs)
  (define (deref ptr) (vector-ref regs ptr))
  (let loop ([ptr 0])
    (match (vector-ref regs ptr)
      [(and (or 1 2) opcode)
       (vector-set! regs (deref (+ ptr 3))
                    ((match opcode [1 +][_ *])
                     (deref (deref (+ ptr 1)))
                     (deref (deref (+ ptr 2)))))
       (loop (+ ptr 4))]
      [99 regs])))

(define test "1,1,1,4,99,5,6,0,99")
(check-equal? (solve (string->regs test)) '#(30 1 1 4 2 5 6 0 99))

(define (nv-result noun verb)
  (define regs (string->regs (file->string "02.rktd")))
  (vector-set*! regs 1 noun 2 verb)
  (vector-ref (solve regs) 0))

;; 1
(check-eq? (nv-result 12 2) 6730673)

;; 2
(check-eq?
 (for*/first ([noun (in-range 91)]
              [verb (in-range 91)]
              #:when (eq? (nv-result noun verb) 19690720))
             (+ (* 100 noun) verb))
 3749)