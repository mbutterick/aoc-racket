#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) TOKS ...)
  #`(#%module-begin
     (time (STARS (vector (λ () TOKS) ...)))))

(define regs (make-hasheq))

(provide set mul sub jnz)
(define-macro (value VAL) #'(let ([val 'VAL]) (if (number? val) val (hash-ref! regs val 0))))
(define-macro (set REG VAL) #'(hash-set! regs 'REG (value VAL)))
(define-macro (sub REG VAL) #'(hash-update! regs 'REG (λ (val) (- val (value VAL))) 0))
(define-macro (mul REG VAL) #'(begin (hash-update! regs 'mul-count add1 0)
                                     (hash-update! regs 'REG (λ (val) (* val (value VAL))) 0)))
(define-macro (jnz REG VAL) #'(when (not (zero? (value REG))) (raise (value VAL))))

(define (★ insts [final-key 'mul-count])
  (for/fold ([offset 0]
             #:result (hash-ref regs final-key))
            ([i (in-naturals)]
             #:break (not (<= 0 offset (sub1 (vector-length insts)))))
    (with-handlers ([integer? (λ (num) (+ num offset))])
      (define inst (vector-ref insts offset))
      (inst)
      (add1 offset))))

(define (★★ insts)
  (hash-set! regs 'a 1)
  (★ insts 'h))