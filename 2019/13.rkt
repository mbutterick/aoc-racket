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
                       [3 (thread-send calling-thd 'wants-joystick) (reg-set! (resolve ptr 1) (thread-receive))]
                       [4 (thread-send calling-thd (reg-ref (resolve ptr 1)))]
                       [9 (set! relative-base (+ relative-base (reg-ref (resolve ptr 1))))])
                     (+ ptr 2)]
                    [(or 5 6) ; 3-arity: jump
                     (if ((match opcode
                            [5 not]
                            [6 values]) (zero? (reg-ref (resolve ptr 1))))
                         (reg-ref (resolve ptr 2))
                         (+ ptr 3))]
                    [99 (thread-send calling-thd 'done) (terminate)]
                    [_ (error "unknown opcode" opcode)]))
                (loop next-ptr))))))


(define (get-sprites x)
  (define thd (match x
                [(? thread?) x]
                [_ (make-runner x)]))
  (for/list ([out (in-producer thread-receive 'done)])
    out))

(require racket/sequence)
(define (sprites->pixels sprs)
  (for/hash ([xyz (in-slice 3 sprs)])
    (values (list (first xyz) (second xyz)) (third xyz))))

;; 1
(check-eq?
 (count (curry = 2) (hash-values (sprites->pixels (get-sprites (file->string "13.rktd")))))
 304)

(require racket/hash)
(define str (regexp-replace #px"^\\d" (file->string "13.rktd") "2"))
(define (breakout)
  (define ball-sprite 4)
  (define paddle-sprite 3)
  (define score-magic-code '(-1 0))
  (define t (make-runner str))
  (define screen-pixels (make-hash))
  (define (find-loc which) (for/first ([(k v) (in-hash screen-pixels)]
                                       #:when (= which v))
                             k))
  (let loop ([acc null])
    (match (thread-receive)
      ['wants-joystick
       (hash-union! screen-pixels (sprites->pixels (reverse acc)) #:combine (λ (v0 v1) v1))
       (match-define (list bx _) (find-loc ball-sprite))
       (match-define (list px _) (find-loc paddle-sprite))
       (thread-send t (cond
                        [(< px bx) 1]
                        [(> px bx) -1]
                        [else 0]))
       (loop null)]
      ['done (hash-ref (sprites->pixels (reverse acc)) score-magic-code)]
      [val (loop (cons val acc))])))

;; 2
(check-eq? (breakout) 14747)