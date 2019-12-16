#lang br
(require racket/file rackunit)

(define ore? number?)

(define (make-reactor reax-output proc-or-ore [t (current-thread)])
  (let ([r (thread (λ ()
                     (define reax 0)
                     (define reax-formula (match proc-or-ore
                                            [(? ore? ore) ore]
                                            [proc proc]))
                     (let loop ([supply 0])
                       (match (thread-receive)
                         ['ore
                          (thread-send t (match reax-formula
                                           [(? ore? ore) (* ore reax)]
                                           [_ 0]))
                          (loop supply)]
                         ['reset
                          (set! reax 0)
                          (loop 0)]
                         [amt (let inner ([supply supply])
                                (cond
                                  [(< supply amt)
                                   (set! reax (add1 reax))
                                   (unless (ore? reax-formula)
                                     (reax-formula))
                                   (inner (+ supply reax-output))]
                                  [else (loop (- supply amt))]))]))))])
    (λ (arg)
      (thread-send r arg)
      (when (eq? arg 'ore)
        (thread-receive)))))


(define A (make-reactor 10 10))
#|(A 7)
(A 7)
(A 7)
(A 7)
(check-eq? (A 'ore) 30)
(A 'reset)
|#

(define B (make-reactor 1 1))
#|(B 7)
(B 7)
(B 7)
(B 7)
(check-eq? (B 'ore) 28)
(B 'reset)
|#

(define C (make-reactor 1 (λ () (A 7) (B 1))))
(define D (make-reactor 1 (λ () (A 7) (C 1))))
(define E (make-reactor 1 (λ () (A 7) (D 1))))
(define FUEL (make-reactor 1 (λ () (A 7) (E 1))))
(FUEL 1)
(A 'ore)
(A 'ore)
(A 'ore)