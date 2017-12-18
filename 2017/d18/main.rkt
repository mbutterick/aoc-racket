#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) TOKS ...)
  #`(#%module-begin
     (time (STARS (vector (λ () TOKS) ...)))))

(define regs (make-hasheq))
(define last-sound-played (make-parameter #f))
(struct offset-signal (val))
(struct end-signal (val))

(provide snd set add mul mod rcv jgz)
(define-macro (value VAL) #'(let ([val 'VAL]) (if (number? val) val (hash-ref regs val))))
(define-macro (snd REG) #'(last-sound-played (hash-ref regs 'REG)))
(define-macro (set REG VAL) #'(hash-set! regs 'REG (value VAL)))
(define-macro (add REG VAL) #'(hash-update! regs 'REG (λ (val) (+ val (value VAL))) 0))
(define-macro (mul REG VAL) #'(hash-update! regs 'REG (λ (val) (* val (value VAL))) 0))
(define-macro (mod REG VAL) #'(hash-update! regs 'REG (λ (val) (modulo val (value VAL))) 0))
(define-macro (rcv REG) #'(unless (zero? (hash-ref regs 'REG)) (raise (last-sound-played))))
(define-macro (jgz REG VAL) #'(when (positive? (hash-ref regs 'REG)) (raise (offset-signal (value VAL)))))

(define (★ insts)
  (with-handlers ([number? values])
    (for/fold ([offset 0])
              ([i (in-naturals)])
      (with-handlers ([offset-signal? (λ (os) (+ (offset-signal-val os) offset))])
        (define proc (vector-ref insts offset))
        (proc)
        (add1 offset)))))
  