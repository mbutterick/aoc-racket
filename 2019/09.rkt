#lang br
(require racket/file rackunit)

(define (string->ints str) (map string->number (string-split str #px",\\s*")))
(define (string->regs str) (list->vector (string->ints str)))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (make-runner program [calling-thd (current-thread)])
  (thread (λ ()
            (define regs (string->regs program))
            (define (maybe-enlarge-regs ptr)
              (unless (< ptr (vector-length regs))
                (define newvec (make-vector (add1 ptr) 0))
                (vector-copy! newvec 0 regs)
                (set! regs newvec))
              regs)
            (define (reg-ref ptr) (vector-ref (maybe-enlarge-regs ptr) ptr))
            (define (reg-set! ptr val) (vector-set! (maybe-enlarge-regs ptr) ptr val))
            (define relative-base 0)
            (let/ec terminate
              (let loop ([ptr 0])
                (define inst (for/list ([c (in-string (~r (reg-ref ptr) #:min-width 5 #:pad-string "0"))])
                                       (string->number (string c))))
                (match-define (list opcode mode-1 mode-2 mode-3)
                  (match inst
                    [(list d4 d3 d2 d1 d0)
                     (cons (+ (* 10 d1) d0)
                           (for/list ([mode-val (list d2 d3 d4)]
                                      [offset '(1 2 3)])
                                     (λ (ptr)
                                       (define ptr-adjusted (+ ptr offset))
                                       (match mode-val
                                         [0 (reg-ref ptr-adjusted)] ; position
                                         [1 ptr-adjusted] ; immediate
                                         [2 (+ relative-base (reg-ref ptr-adjusted))]))))])) ; relative
                (define next-ptr
                  (match opcode
                    [(or 1 2 7 8) ; 4-arity: add & multiply & compare
                     (reg-set! (mode-3 ptr) ((match opcode
                                               [1 +]
                                               [2 *]
                                               [7 (binarize <)]
                                               [8 (binarize =)]) (reg-ref (mode-1 ptr)) (reg-ref (mode-2 ptr))))
                     (+ ptr 4)]
                    [(or 3 4 9) ; 2-arity: input & output
                     (match opcode
                       [3 (reg-set! (mode-1 ptr) (thread-receive))]
                       [4 (thread-send calling-thd (reg-ref (mode-1 ptr)))]
                       [9 (set! relative-base (+ relative-base (reg-ref (mode-1 ptr))))])
                     (+ ptr 2)]
                    [(or 5 6) ; 3-arity: jump
                     (if ((match opcode
                            [5 not]
                            [6 values]) (zero? (reg-ref (mode-1 ptr))))
                         (reg-ref (mode-2 ptr))
                         (+ ptr 3))]
                    [99 (thread-send calling-thd 'done) (terminate)]
                    [_ (error "unknown opcode" opcode)]))
                (loop next-ptr))))))


(define (run program-or-thread)
  (define thd (match program-or-thread
                [(? thread? thd) thd]
                [program (make-runner program)]))
  (for/list ([val (in-producer thread-receive 'done)])
            val))


(check-equal?
 (run "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
 '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))


(check-equal?
 (run "1102,34915192,34915192,7,4,7,99,0")
 '(1219070632396864))

(check-equal?
 (run "104,1125899906842624,99")
 '(1125899906842624))

;; 1
(define t1 (make-runner (file->string "09.rktd")))
(thread-send t1 1)
(check-equal?
 (run t1)
 '(3601950151))

;; 2
(define t2 (make-runner (file->string "09.rktd")))
(thread-send t2 2)
(check-equal?
 (run t2)
 '(64236))