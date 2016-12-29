#lang br/quicklang ;; http://adventofcode.com/2016/day/23
(provide read-syntax
         (rename-out [mb #%module-begin])
         cpy inc dec jnz tgl)

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (zero? (string-length str))))
            (format-datum '(~a) str)))))

(define-macro (mb INST ...)
  #'(#%module-begin
     (report 'INST) ...
     (define insts (vector INST ...))
     (define regs (make-hash '((a . 0)(b . 0)(c . 0)(d . 0))))
     (println (solve insts regs))
     (hash-set! regs 'c 1)
     (println (solve insts regs))))

(define current-toggles (make-parameter empty))
(define current-ptr (make-parameter #f))

(define (toggled?) (member (current-ptr) (current-toggles)))

(define (solve insts regs)
  (let loop ([ptr 0])
    (current-ptr ptr)
    (if (>= ptr (vector-length insts))
        regs
        (loop (+ ptr (let ([move ((vector-ref insts ptr) regs)])
                       (if (void? move) 1 move)))))))

(define-macro (make-tgl-base X)
  #'(λ(regs)
      (current-toggles
       ((if (member X (current-toggles))
            remove
            cons) X (current-toggles)))))

(define-macro (tgl X)
  #'(λ(regs)
      ((if (toggled?)
          (make-inc-base 'X)
          (make-tgl-base 'X)) regs)))

(define-macro (make-cpy-base X Y)
  #'(λ(regs)
      (define val (if (number? X) X (hash-ref regs X)))
      (hash-set! regs Y val)))

(define-macro (cpy X Y)
  #'(λ(regs)
      ((if (toggled?)
        (make-jnz-base 'X 'Y)
        (make-cpy-base 'X 'Y)) regs)))

(define-macro (make-dec-base X) #'(λ(regs) (hash-update! regs X sub1)))
(define-macro (make-inc-base X) #'(λ(regs) (hash-update! regs X add1)))

(define-macro (inc X) #'(λ(regs)
                          ((if (toggled?)
                              (make-dec-base 'X)
                              (make-inc-base 'X)) regs)))

(define-macro (dec X) #'(λ(regs)
                          ((if (toggled?)
                            (make-inc-base 'X)
                            (make-dec-base 'X)) regs)))


(define-macro (make-jnz-base X Y)
  #'(λ(regs)
      (when (not (zero? (if (number? X) X (hash-ref regs X))))
        Y)))

(define-macro (jnz X Y)
  #'(λ(regs)
      ((if (toggled?)
        (make-cpy-base 'X 'Y)
        (make-jnz-base 'X 'Y)) regs)))
