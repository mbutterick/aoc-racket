#lang br
(require racket/file rackunit racket/generator)

(define (string->ints str) (map string->number (string-split str #px",\\s*")))
(define (string->regs str) (list->vector (string->ints str)))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (run str . inputs)
  (define regs (string->regs str))
  (define (reg-ref ptr) (vector-ref regs ptr))
  (define (reg-set! ptr val) (vector-set! regs (reg-ref ptr) val))
  (define ip (open-input-string (string-join (map ~a inputs) " ")))
  (let/ec terminate
    (let loop ([ptr 0])
      (match-define (list opcode mode-1 mode-2 mode-3)
        (match (for/list ([c (in-string (~r (reg-ref ptr) #:min-width 5 #:pad-string "0"))])
                 (string->number (string c)))
          [(list d4 d3 d2 d1 d0) (cons (+ (* 10 d1) d0)
                                       (for/list ([mode-val (list d2 d3 d4)]
                                                  [offset '(1 2 3)])
                                         (λ (ptr) ((if (zero? mode-val) reg-ref values) (reg-ref (+ ptr offset))))))]))
      (define next-ptr
        (match opcode
          [(or 1 2 7 8) ; 4-arity: add & multiply & compare
           (reg-set! (+ ptr 3) ((match opcode
                                  [1 +]
                                  [2 *]
                                  [7 (binarize <)]
                                  [8 (binarize =)]) (mode-1 ptr) (mode-2 ptr)))
           (+ ptr 4)]
          [(or 3 4) ; 2-arity: input & output
           (match opcode
             [3 (parameterize ([current-input-port ip])
                  (reg-set! (+ ptr 1) (read)))]
             [4 (println (mode-1 ptr))])
           (+ ptr 2)]
          [(or 5 6) ; 3-arity: jump
           (if ((match opcode
                  [5 not]
                  [6 values]) (zero? (mode-1 ptr)))
               (mode-2 ptr)
               (+ ptr 3))]
          [99 (terminate)]
          [_ (error 'unknown-opcode)]))
      (loop next-ptr))))

(define-syntax-rule (last-output func)
  (last (map string->number (string-split (with-output-to-string (λ () func))))))

(define (thruster-signal program phases)
  (for/fold ([input 0])
            ([phase (in-list (if (string? phases) (string->ints phases) phases))])
    (last-output (run program phase input))))

(check-eq? (thruster-signal "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" "4,3,2,1,0") 43210)
(check-eq? (thruster-signal "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0" "0,1,2,3,4") 54321)
(check-eq? (thruster-signal "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" "1,0,4,3,2") 65210)

;; 1
(check-eq?
 (apply max (for/list ([phases (in-permutations '(0 1 2 3 4))])
              (thruster-signal (file->string "07.rktd") phases)))
 87138)