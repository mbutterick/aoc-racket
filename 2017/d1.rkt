#lang br/quicklang
(require sugar/list)

(module+ reader
  (require syntax/strip-context)
  (provide read-syntax)
  (define (read-syntax path port)
    (strip-context #`(module day1 aoc-racket/2017/d1
                       #,@(for/list ([datum (in-port read port)])
                            datum)))))

(provide (rename-out [mb #%module-begin]))
(define-macro (mb SELECTOR CAPTCHA-STR ...)
  #'(#%module-begin (captcha-sum 'SELECTOR CAPTCHA-STR) ...))

(define (captcha-sum offset-sig num)
  (define digits (for/list ([c (in-string (number->string num))])
                 (string->number (string c))))
  (define offset (if (eq? offset-sig '★) -1 (/ (length digits) 2)))
  (for/sum ([first (in-list digits)]
            [second (in-list (shift-cycle digits offset))]
            #:when (= first second))
    first))