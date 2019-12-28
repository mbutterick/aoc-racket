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
                       [3 (reg-set! (resolve ptr 1) (thread-receive))]
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

(define beam-rec (make-hasheq))
(define (in-beam? x y)
  (hash-ref! beam-rec (+ (* 10000 x) y)
             (λ ()
               (define th (make-runner (file->string "19.rktd")))
               (thread-send th x)
               (thread-send th y)
               (not (zero? (thread-receive))))))

;; 1
(check-eq?
 (for*/sum ([x (in-range 50)]
            [y (in-range 50)]
            #:when (in-beam? x y))
           1)
 129)

(define (in-big-square? x y)
  (and
   (in-beam? x y)
   (in-beam? (+ x 99) y)
   (in-beam? x (+ y 99))))

;; 2
;; discovered by trial & error:
;; 2100 too low
;; 2200 ok
(check-eq?
 (for*/first ([xy-sum (in-naturals 2100)]
              [x (in-range xy-sum)]
              [y (in-value (- xy-sum x))]
              #:when (in-big-square? x y))
             (+ (* 10000 x) y))
 14040699)