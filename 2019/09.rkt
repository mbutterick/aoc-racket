#lang br
(require racket/file rackunit)

(define (string->ints str) (map string->number (string-split str #px",\\s*")))
(define (string->regs str) (list->vector (string->ints str)))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (make-runner program [calling-thd (current-thread)])
  (thread (λ ()
            (define regs (string->regs program))
            (define (grow-regs! len)
              (define newvec (make-vector (add1 len) 0))
              (vector-copy! newvec 0 regs)
              (set! regs newvec))
            (define (reg-ref ptr)
              (cond
                [(< ptr (vector-length regs)) (vector-ref regs ptr)]
                [else
                 (grow-regs! (add1 ptr))
                 (reg-ref ptr)]))
            (define (reg-set! ptr val)
              (cond
                [(< (reg-ref ptr) (vector-length regs)) (vector-set! regs (reg-ref ptr) val)]
                [else
                 (grow-regs! (add1 (reg-ref ptr)))
                 (reg-set! ptr val)]))
            (define relative-base 0)
            (let/ec terminate
              (let loop ([ptr 0])
                (define inst (for/list ([c (in-string (~r (reg-ref ptr) #:min-width 5 #:pad-string "0"))])
                               (string->number (string c))))
                (match-define (list opcode mode-1 mode-2 mode-3)
                  (match #R inst
                    [(list d4 d3 d2 d1 d0)
                     (cons (+ (* 10 d1) d0)
                           (for/list ([mode-val (list d2 d3 d4)]
                                      [offset '(1 2 3)])
                             (match mode-val
                               ;; position
                               [0 (λ (ptr) (reg-ref (reg-ref (+ ptr offset))))]
                               ;; immediate
                               [1 (λ (ptr) (reg-ref (+ ptr offset)))]
                               ;; relative
                               [2 (λ (ptr) (reg-ref (+ relative-base (reg-ref (+ ptr offset)))))])))]))
                (define next-ptr
                  (match #R opcode
                    [(or 1 2 7 8) ; 4-arity: add & multiply & compare
                     (reg-set! (+ ptr 3) ((match opcode
                                            [1 +]
                                            [2 *]
                                            [7 (binarize <)]
                                            [8 (binarize =)]) (mode-1 ptr) (mode-2 ptr)))
                     (+ ptr 4)]
                    [(or 3 4 9) ; 2-arity: input & output
                     (match opcode
                       [3 (reg-set! (mode-1 ptr) (thread-receive))]
                       [4 (thread-send calling-thd (mode-1 ptr))]
                       [9 (set! relative-base (+ relative-base (mode-1 ptr)))])
                     (+ ptr 2)]
                    [(or 5 6) ; 3-arity: jump
                     (if ((match opcode
                            [5 not]
                            [6 values]) (zero? (mode-1 ptr)))
                         (mode-2 ptr)
                         (+ ptr 3))]
                    [99 (thread-send calling-thd 'done) (terminate)]
                    [_ (error "unknown-opcode" opcode)]))
                (loop next-ptr))))))


(define (run program-or-thread)
  (define thd (match program-or-thread
                [(? thread? thd) thd]
                [program (make-runner program)]))
  (let loop ([acc null])
    (match (thread-receive)
      ['done (reverse acc)]
      [msg (loop (cons msg acc))])))


(check-equal?
 (run "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
 '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))

(check-equal?
 (run "1102,34915192,34915192,7,4,7,99,0")
 '(1219070632396864))

(check-equal?
 (run "104,1125899906842624,99")
 '(1125899906842624))


(define t1 (make-runner (file->string "09.rktd")))
(thread-send t1 1)
(run t1)
