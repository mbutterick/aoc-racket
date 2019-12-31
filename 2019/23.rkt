#lang br
(require racket/file rackunit racket/async-channel)

(define (string->ints str) (map string->number (string-split str #px",\\s*")))
(define (string->regs str) (list->vector (string->ints str)))

(define ((binarize proc) x y) (if (proc x y) 1 0))

(define (make-runner program inch ouch)
  (thread (λ ()
            (define regs (string->regs (file->string program)))
            (define (maybe-enlarge-regs ptr)
              (unless (< ptr (vector-length regs))
                (define newvec (make-vector (add1 ptr) 0))
                (vector-copy! newvec 0 regs)
                (set! regs newvec))
              regs)
            (define (reg-ref ptr) (vector-ref (maybe-enlarge-regs ptr) ptr))
            (define (reg-set! ptr val) (vector-set! (maybe-enlarge-regs ptr) ptr val))
            (define relative-base 0)
            (define address #false)
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
                       [3 (async-channel-put ouch 'wants-input)
                        (define val (async-channel-get inch))
                        (unless address (set! address val))
                        (reg-set! (resolve ptr 1) val)]
                       [4 (async-channel-put ouch (reg-ref (resolve ptr 1)))]
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

(define packet-address first)
(define packet-x second)
(define packet-y third)

(define (solve break-cond)
  (define nics (for/list ([i (in-range 50)])
                 (define inch (make-async-channel))
                 (define ouch (make-async-channel))
                 (define th (make-runner "23.rktd" inch ouch))
                 (async-channel-get ouch)
                 (async-channel-put inch i)
                 (list inch ouch i)))
  (for/fold ([packets null]
             [nat #false]
             [idle-vals null]
             #:result (packet-y nat))
            ([nicrec (in-cycle nics)]
             #:break (break-cond nat idle-vals))
    (define (network-idle?) (and (null? packets) #false))
    (cond
      [(network-idle?)
       (match-define (list inch _ _) (car nics))
       (async-channel-put inch (packet-x nat))
       (async-channel-put inch (packet-y nat))
       (values packets nat (cons (packet-y nat) idle-vals))]
      [else
       (match-define (list inch ouch nic-address) nicrec)
       (match (async-channel-get ouch)
         ['wants-input
          (define-values (ps other-ps) (partition (λ (p) (= nic-address (packet-address p))) packets))
          (match (reverse ps)
            [(? null?) (async-channel-put inch -1)]
            [ps (for ([p (in-list ps)])
                  (match-define (list _ x y) p)
                  (async-channel-put inch x)
                  (async-channel-put inch y))])
          (values other-ps nat idle-vals)]
         [dest
          (define next-packet (list dest (async-channel-get ouch) (async-channel-get ouch)))
          (if (eq? dest 255)
              (values packets next-packet idle-vals)
              (values (cons next-packet packets) nat idle-vals))])])))

;; 1
(check-eq? (solve (λ (nat idle-vals) nat)) 21089)
