#lang br
(require racket/file rackunit racket/set)

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
                (define-values (opcode resolve)
                  (match inst
                    [(list d4 d3 d2 d1 d0)
                     (values (+ (* 10 d1) d0)
                             (λ (ptr offset)
                               (define parameter-value (match offset [1 d2] [2 d3] [3 d4]))
                               (define ptr-resolver (match parameter-value
                                                      [0 reg-ref] ; position
                                                      [1 values] ; immediate
                                                      [2 (compose1 (λ (ptr) (+ relative-base ptr)) reg-ref)])); relative
                               (ptr-resolver (+ ptr offset))))])) 
                (define next-ptr
                  (match opcode
                    [(or 1 2 7 8) ; 4-arity: add & multiply & compare
                     (reg-set! (resolve ptr 3) ((match opcode
                                                  [1 +]
                                                  [2 *]
                                                  [7 (binarize <)]
                                                  [8 (binarize =)]) (reg-ref (resolve ptr 1)) (reg-ref (resolve ptr 2))))
                     (+ ptr 4)]
                    [(or 3 4 9) ; 2-arity: input & output
                     (match opcode
                       [3 (thread-send calling-thd 'wants-input)
                        (reg-set! (resolve ptr 1) (thread-receive))]
                       [4 (thread-send calling-thd (reg-ref (resolve ptr 1)))]
                       [9 (set! relative-base (+ relative-base (reg-ref (resolve ptr 1))))])
                     (+ ptr 2)]
                    [(or 5 6) ; 3-arity: jump
                     (if ((match opcode
                            [5 not]
                            [6 values]) (zero? (reg-ref (resolve ptr 1))))
                         (reg-ref (resolve ptr 2))
                         (+ ptr 3))]
                    [99 (terminate)]
                    [_ (error "unknown opcode" opcode)]))
                (loop next-ptr))))))


(define th (make-runner (file->string "25.rktd")))
(let loop ([buf null])
  (match (thread-receive)
    [(? integer? int)
     (display (string (integer->char int)))
     (loop buf)]
    ['wants-input
     (let ([buf (if (null? buf)
                    (append (map char->integer (string->list (read-line))) (list 10))
                    buf)])
       (thread-send th (car buf))
       (loop (cdr buf)))]))
                  
