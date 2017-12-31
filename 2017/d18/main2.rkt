#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★★)

(define-macro (#%mb (STARS) TOKS ...)
  #`(#%module-begin
     (time (STARS (vector (λ () TOKS) ...)))))

(define cur-program (make-parameter #f))
(define cur-receiver (make-parameter #f))

(provide snd set add mul mod rcv jgz)
(define-macro (value VAL) #'(let ([val 'VAL]) (if (number? val) val (hash-ref! (cur-program) val 0))))
(define-macro (set REG VAL) #'(hash-set! (cur-program) 'REG (value VAL)))
(define-macro (add REG VAL) #'(hash-update! (cur-program) 'REG (λ (val) (+ val (value VAL))) 0))
(define-macro (mul REG VAL) #'(hash-update! (cur-program) 'REG (λ (val) (* val (value VAL))) 0))
(define-macro (mod REG VAL) #'(hash-update! (cur-program) 'REG (λ (val) (modulo val (value VAL))) 0))
(define-macro (snd VAL) #'(begin
                            (hash-update! (cur-program) 'sends add1 0)
                            (hash-update! (cur-receiver) 'msgs (λ (vals) (append vals (list (value VAL)))) null)))
(define-macro (rcv REG) #'(let ([msgs (hash-ref! (cur-program) 'msgs null)])
                            (when (empty? msgs)
                              (hash-set! (cur-program) 'waiting #t)
                              (raise 'waiting))
                            (hash-set! (cur-program) 'waiting #f)
                            (hash-update! (cur-program) 'msgs cdr)
                            (hash-set! (cur-program) 'REG (car msgs))))
(define-macro (jgz X Y) #'(when (positive? (value X)) (raise (value Y))))

(define (do-inst p rcvr insts)
  (hash-update! p 'offset (λ (offset)
                            (+ offset
                               (with-handlers ([(curry eq? 'waiting) (const 0)]
                                               [integer? values])
                                 (parameterize ([cur-program p]
                                                [cur-receiver rcvr])
                                   (define inst (vector-ref insts offset))
                                   (inst))
                                 1)))))

(define (★★ insts)
  (define p0 (make-hasheq '((p . 0))))
  (define p1 (make-hasheq '((p . 1))))
  (define (offset-valid? p) (<= 0 (hash-ref! p 'offset 0) (sub1 (vector-length insts))))
  (define (waiting? p) (hash-ref! p 'waiting #f))
  (for ([i (in-naturals)]
        #:break (or (and (not (offset-valid? p0)) (not (offset-valid? p1)))
                    (and (waiting? p0) (waiting? p1))))
    (when (offset-valid? p0) (do-inst p0 p1 insts))
    (when (offset-valid? p1) (do-inst p1 p0 insts)))
  (hash-ref p1 'sends))
