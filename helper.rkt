#lang at-exp racket/base
(require scribble/manual)
(provide (all-defined-out))
(require (for-syntax racket/base racket/syntax) racket/runtime-path)
(provide (for-syntax #%datum))

(define (aoc-title which)
  (define which-str (number->string which))
  @title[#:style manual-doc-style]{@link[@string-append["http://adventofcode.com/day/" @which-str]]{Day @which-str}})

(define-syntax (link-rp stx)
  (syntax-case stx ()
    [(_ where text-args ...)
     (with-syntax ([rp-name (generate-temporary)])
       #'(begin
           (require racket/runtime-path)
           (define-runtime-path rp-name (expand-user-path where))
           @(link (path->string rp-name) text-args ...)))]))