#lang br
(require racket/file rackunit racket/generator)

(define (string->ints str) (map string->number (string-split str #px",\\s*")))
(define (string->regs str) (list->vector (string->ints str)))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (make-amp-generator program)
  (generator ()
            (define regs (string->regs program))
            (define (reg-ref ptr) (vector-ref regs ptr))
            (define (reg-set! ptr val) (vector-set! regs (reg-ref ptr) val))
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
                       [3 (reg-set! (+ ptr 1) (yield))]
                       [4 (yield (mode-1 ptr))])
                     (+ ptr 2)]
                    [(or 5 6) ; 3-arity: jump
                     (if ((match opcode
                            [5 not]
                            [6 values]) (zero? (mode-1 ptr)))
                         (mode-2 ptr)
                         (+ ptr 3))]
                    [99 (terminate)]
                    [_ (error "unknown-opcode" opcode)]))
                (loop next-ptr)))))

(define (make-amp-generators program phases)
  (for/list ([phase (in-list (if (string? phases) (string->ints phases) phases))])
            (define thd (make-amp-generator program))
            (thd)
            (thd phase)
            thd))

(define (thruster-signal program phases)
  (for/fold ([input 0])
            ([amp (in-list (make-amp-generators program phases))])
    (amp input)))


(check-eq? (thruster-signal "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" "4,3,2,1,0") 43210)
(check-eq? (thruster-signal "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0" "0,1,2,3,4") 54321)
(check-eq? (thruster-signal "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" "1,0,4,3,2") 65210)

(define amp-program (file->string "07.rktd"))

;; 1
(check-eq?
 (apply max (for/list ([phases (in-permutations '(0 1 2 3 4))])
                      (thruster-signal amp-program phases)))
 87138)

(define (thruster-signal-cycle program phases)
  (for/fold ([input 0])
            ([amp (in-cycle (make-amp-generators program phases))]
             #:break (eq? (generator-state amp) 'done))
    (match (values->list (amp input))
      [(list res) res]
      [_ input])))

(check-eq?
 (thruster-signal-cycle "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" "9,8,7,6,5")
 139629729)

(check-eq?
 (thruster-signal-cycle "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" "9,7,8,5,6")
 18216)

;; 2
(check-eq?
 (apply max (for/list ([phases (in-permutations '(5 6 7 8 9))])
                      (thruster-signal-cycle amp-program phases)))
 17279674)