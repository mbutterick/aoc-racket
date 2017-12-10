#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]))

(define-macro (#%mb (STARS) (TOK ...) ...)
  #`(#%module-begin
     (inst TOK ...) ...
     (if (eq? 'STARS '★) (max-arg vals) max-seen)))

(define vals (make-hasheq))
(define (get-val key) (hash-ref! vals key 0))
(define (max-arg vals) (argmax cdr (hash->list vals)))
(define max-seen 0)
(define (set-val! key updater)
  (hash-update! vals key (λ (val)
                           (define new-val (updater val))
                           (set! max-seen (max max-seen new-val))
                           new-val) 0))

(provide >= <= < > ==)
(define-macro-cases cmp
  [(_ ==) #'=]
  [(_ !=) #'(negate =)]
  [(_ OTHER) #'OTHER])

(define-macro-cases updater
  [(_ dec) #'-]
  [(_ inc) #'+])

(provide if)
(define-macro (inst TARGET UPDATE-OP UPDATE-VAL if SRC CMP VAL)
  #'(when ((cmp CMP) (get-val 'SRC) VAL)
      (set-val! 'TARGET (λ (val) ((updater UPDATE-OP) val UPDATE-VAL)))))
