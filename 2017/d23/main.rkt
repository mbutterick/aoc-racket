#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) TOKS ...)
  #`(#%module-begin
     (time (STARS (vector (λ () TOKS) ...)))))

(define regs (make-hasheq))
(struct offset-signal (val))

(provide set mul sub jnz)
(define-macro (value VAL) #'(let ([val 'VAL]) (if (number? val) val (hash-ref! regs val 0))))
(define-macro (set REG VAL) #'(hash-set! regs 'REG (value VAL)))
(define-macro (sub REG VAL) #'(hash-update! regs 'REG (λ (val) (- val (value VAL))) 0))
(define-macro (mul REG VAL) #'(begin (hash-update! regs 'mul-count add1 0)
                                     (hash-update! regs 'REG (λ (val) (* val (value VAL))) 0)))
(define-macro (jnz REG VAL) #'(when (not (zero? (value REG))) (raise (offset-signal (value VAL)))))

(define (★ insts)
  (for/fold ([offset 0]
             #:result (hash-ref regs 'mul-count))
            ([i (in-naturals)]
             #:break (not (<= 0 offset (sub1 (vector-length insts)))))
    (with-handlers ([offset-signal? (λ (os) (+ (offset-signal-val os) offset))])
      (define proc (vector-ref insts offset))
      (proc)
      (add1 offset))))