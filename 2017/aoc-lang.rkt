#|
#lang s-exp syntax/module-reader
#:read read
#:read-syntax read-syntax
#:language `(submod ,aoc-lang expander) 
(require racket/runtime-path)
(define-runtime-path aoc-lang "aoc-lang.rkt")
|#

#lang br/quicklang
(require "helper.rkt")
(provide (except-out (all-from-out br/quicklang "helper.rkt") read-syntax #%module-begin)
         (rename-out [my-rs read-syntax] [my-mb #%module-begin]))

(define (my-rs path port)
  (define datums (for/list ([datum (in-port (curry read-syntax path) port)])
                   datum))
  (strip-context (with-pattern ([THIS-FILE (syntax-source #'here)]
                                [DATUMS datums])
                   (syntax/loc (car datums) (module puzzle-lang THIS-FILE
                                              . DATUMS)))))

(define-macro (my-mb . ARGS)
  (with-pattern ([MOD-PATH (syntax-source caller-stx)])
    #'(#%module-begin
       (provide read-syntax)
       (define (read-syntax path port)
         (strip-context #`(module mod MOD-PATH
                            #,@(for/list ([line (in-lines port)])
                                 (for/list ([datums (in-port read (open-input-string line))])
                                   datums)))))
       . ARGS)))
