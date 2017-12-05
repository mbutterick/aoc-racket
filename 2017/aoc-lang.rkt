#lang s-exp syntax/module-reader
#:read read
#:read-syntax read-syntax
#:language `(submod ,aoc-lang expander) 
(require racket/runtime-path)
(define-runtime-path aoc-lang "aoc-lang.rkt")

(module expander br/quicklang
  (require "helper.rkt")
  (provide (except-out (all-from-out br/quicklang "helper.rkt") #%module-begin)
           (rename-out [#%mb #%module-begin]))

  (define-macro (#%mb . ARGS)
    (with-pattern ([MODULE-PATH (syntax-source caller-stx)])
      #'(#%module-begin
         (provide read-syntax)
         (define (read-syntax path port)
           (strip-context #`(module mod MODULE-PATH
                              #,@(for/list ([line (in-lines port)])
                                   (for/list ([datums (in-port read (open-input-string line))])
                                     datums)))))
         . ARGS))))
