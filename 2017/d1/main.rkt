#lang br/quicklang
(require "../helper.rkt")

(provide read-syntax)
(define (read-syntax path port)
  (strip-context #`(module mod "main.rkt" #,@(port->datums port))))

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb STARS NUMBER ...)
  #'(#%module-begin (captcha-sum 'STARS NUMBER) ...))

(define (captcha-sum stars num)
  (define digits (number->digits num))
  (define offset (if (eq? stars '★) -1 (quotient (length digits) 2)))
  (for/sum ([digit (in-list digits)]
            [other-digit (in-list (shift-cycle digits offset))]
            #:when (= digit other-digit))
    digit))