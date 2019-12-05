#lang br
(require racket/file rackunit)

(define (parse-ptr ptr proc)
  (match (for/list ([c (in-string (~r ptr #:min-width 5 #:pad-string "0"))])
           (string->number (string c)))
    [(list d4 d3 d2 d1 d0) (cons (+ (* 10 d1) d0)
                                 (for/list ([val (list d2 d3 d4)])
                                   (if (zero? val) proc values)))]))

(define (string->regs str)
  (list->vector (map string->number (string-split (string-replace str "," " ")))))

(define (run str starting-input)
  (define regs (string->regs str))
  (define (deref ptr) (vector-ref regs ptr))
  (let loop ([ptr 0])
    (match-define (list opcode mode1 mode2 mode3) (parse-ptr (vector-ref regs ptr) deref))
    (match opcode
      ;; add & multiply
      [(or 1 2) 
       (vector-set! regs (deref (+ ptr 3))
                    ((match opcode [1 +][_ *])
                     (mode1 (deref (+ ptr 1)))
                     (mode2 (deref (+ ptr 2)))))
       (loop (+ ptr 4))]
      ;; input
      [3 (vector-set! regs (mode1 (+ ptr 1)) starting-input)
       (loop (+ ptr 2))]
      ;; output
      [4 (println (vector-ref regs (mode1 (+ ptr 1))))
       (loop (+ ptr 2))]
      ;; jump
      [(or 5 6) 
       (if ((match opcode [5 not][_ values]) (zero? (mode1 (deref (+ ptr 1)))))
           (loop (mode2 (deref (+ ptr 2))))
           (loop (+ ptr 3)))]
      ;; compare
      [(or 7 8)
       (vector-set! regs (deref (+ ptr 3))
                    (if ((match opcode [7 <][_ =]) (mode1 (deref (+ ptr 1))) (mode2 (deref (+ ptr 2))))
                        1
                        0))
       (loop (+ ptr 4))]
      ;; terminate
      [99 (void)]
      [_ (error 'unknown-opcode)])))

(define-syntax-rule (last-output-line func)
  (last (map string->number (string-split (with-output-to-string (λ () func))))))

(check-eq? (last-output-line (run "3,0,4,0,99" 42)) 42)
;; 1
(check-eq?
 (last-output-line (run  (file->string "05.rktd") 1))
 15386262)

(check-eq? (last-output-line (run "3,9,8,9,10,9,4,9,99,-1,8" 8)) 1)
(check-eq? (last-output-line (run "3,9,7,9,10,9,4,9,99,-1,8" 8)) 0)
(check-eq? (last-output-line (run "3,3,1108,-1,8,3,4,3,99" 8)) 1)
(check-eq? (last-output-line (run "3,3,1107,-1,8,3,4,3,99" 8)) 0)
(check-eq? (last-output-line (run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0)) 0)
(check-eq? (last-output-line (run "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0)) 0)

(check-eq?
 (last-output-line
  (run "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" 0))
 999)
(check-eq?
 (last-output-line
  (run "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" 8))
 1000)
(check-eq?
 (last-output-line
  (run "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" 10))
 1001)

;; 2
(check-eq?
 (last-output-line (run  (file->string "05.rktd") 5))
 10376124)