#lang sugar/debug br/quicklang
(require "../helper.rkt")

(provide read-syntax)
(define (read-syntax path port)
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read port)])
                          datum))))

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb STARS JMP ...)
  #'(#%module-begin (escape 'STARS (list->vector '(JMP ...)))))

(define (escape stars vec)
  (let/ec exit
    (for/fold ([pos 0])
              ([i (in-naturals)])
      (unless (<= 0 pos (sub1 (vector-length vec)))
        (exit i))
      (define jmp (vector-ref vec pos))
      (vector-set! vec pos (if (and (eq? stars '★★) (>= jmp 3))
                               (sub1 jmp)
                               (add1 jmp)))
      (+ pos jmp))))

