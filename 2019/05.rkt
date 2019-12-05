#lang br
(require racket/file rackunit)

(define (string->regs str)
  (list->vector (map string->number (string-split str #px",\\s*"))))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (run str starting-input)
  (define regs (string->regs str))
  (define (reg-ref ptr) (vector-ref regs ptr))
  (define (reg-set! ptr val) (vector-set! regs (reg-ref ptr) val))
  (let/ec terminate
    (let loop ([ptr 0])
      (match-define (list opcode mode1 mode2 mode3)
        (match (for/list ([c (in-string (~r (reg-ref ptr) #:min-width 5 #:pad-string "0"))])
                         (string->number (string c)))
          [(list d4 d3 d2 d1 d0) (cons (+ (* 10 d1) d0)
                                       (for/list ([mode (list d2 d3 d4)])
                                                 (if (zero? mode) reg-ref values)))]))
      (define next-ptr
        (match opcode
          [(or 1 2 7 8) ; 4-arity: add & multiply & compare
           (reg-set! (+ ptr 3) ((match opcode
                                  [1 +]
                                  [2 *]
                                  [7 (binarize <)]
                                  [8 (binarize =)]) (mode1 (reg-ref (+ ptr 1)))
                                                    (mode2 (reg-ref (+ ptr 2)))))
           (+ ptr 4)]
          [(or 3 4) ; 2-arity: input & output
           (match opcode
             [3 (reg-set! (+ ptr 1) starting-input)]
             [4 (println (reg-ref (mode1 (+ ptr 1))))])
           (+ ptr 2)]
          [(or 5 6) ; 3-arity: jump
           (if ((match opcode
                  [5 not]
                  [6 values]) (zero? (mode1 (reg-ref (+ ptr 1)))))
               (mode2 (reg-ref (+ ptr 2)))
               (+ ptr 3))]
          [99 (terminate)]
          [_ (error 'unknown-opcode)]))
      (loop next-ptr))))

(define-syntax-rule (last-output func)
  (last (map string->number (string-split (with-output-to-string (λ () func))))))

(check-eq? (last-output (run "3,0,4,0,99" 42)) 42)
;; 1
(check-eq? (last-output (run (file->string "05.rktd") 1)) 15386262)

(check-eq? (last-output (run "3,9,8,9,10,9,4,9,99,-1,8" 8)) 1)
(check-eq? (last-output (run "3,9,7,9,10,9,4,9,99,-1,8" 8)) 0)
(check-eq? (last-output (run "3,3,1108,-1,8,3,4,3,99" 8)) 1)
(check-eq? (last-output (run "3,3,1107,-1,8,3,4,3,99" 8)) 0)
(check-eq? (last-output (run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0)) 0)
(check-eq? (last-output (run "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0)) 0)

(let ([str "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"])
  (check-eq? (last-output (run str 0)) 999)
  (check-eq? (last-output (run str 8)) 1000)
  (check-eq? (last-output (run str 10)) 1001))

;; 2
(check-eq? (last-output (run (file->string "05.rktd") 5)) 10376124)