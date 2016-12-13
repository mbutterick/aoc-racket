#lang br/quicklang
;; http://adventofcode.com/2016/day/12
(provide read-syntax
         (rename-out [mb #%module-begin])
         cpy inc dec jnz)

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (zero? (string-length str))))
            (format-datum '(~a) str)))))

(define-macro (mb . INSTS)
  #'(#%module-begin
     (define insts (vector . INSTS))
     (define regs (make-hash '((a . 0)(b . 0)(c . 0)(d . 0))))
     (println (solve insts regs))
     (hash-set! regs 'c 1)
     (println (solve insts regs))))

(define (solve insts regs)
  (let loop ([ptr 0])
    (if (>= ptr (vector-length insts))
        regs
        (loop (+ ptr (let ([move ((vector-ref insts ptr) regs)])
                       (if (void? move) 1 move)))))))

(define-macro (cpy X Y)
  #'(λ(regs)
      (define val (if (number? 'X) 'X (hash-ref regs 'X)))
      (hash-set! regs 'Y val)))

(define-macro (inc X) #'(λ(regs) (hash-update! regs 'X add1)))

(define-macro (dec X) #'(λ(regs) (hash-update! regs 'X sub1)))

(define-macro (jnz X Y)
  #'(λ(regs)
      (when (not (zero? (if (number? 'X) 'X (hash-ref regs 'X))))
        Y)))
