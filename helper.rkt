#lang at-exp racket/base
(require scribble/manual)
(provide (all-defined-out))
(require (for-syntax racket/base racket/syntax) racket/runtime-path)
(provide (for-syntax #%datum))

(define (aoc-title which)
  (define which-str (number->string which))
  (define day-x (format "day-~a" which-str))
  (define day-prefix (format "~a-" day-x))
  @title[#:style manual-doc-style]{Day @which-str})

(define-syntax (link-rp stx)
  (syntax-case stx ()
    [(_ where text-args ...)
     (with-syntax ([rp-name (generate-temporary)])
       #'(begin
           (require racket/runtime-path)
           (define-runtime-path rp-name where)
           @(link (path->string rp-name) text-args ...)))]))