#lang reader "../aoc-lang.rkt"
(require (for-syntax racket/string racket/sequence) racket/dict)
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS) (DEPTH: RANGE) ...)
  (with-pattern ([(DEPTH ...)
                  (for/list ([id (in-syntax #'(DEPTH: ...))])
                    (string->number (string-trim (symbol->string (syntax->datum id)) ":")))])
    #'(#%module-begin (time (STARS '(DEPTH ...) '(RANGE ...))))))

(define (caught? depth range [delay 0])
  (zero? (modulo (+ depth delay) (* 2 (sub1 range)))))

(define (★ ds rs)
  (for/sum ([d (in-list ds)]
            [r (in-list rs)]
            #:when (caught? d r))
    (* d r)))

(define (★★ ds rs)
  (for/first ([delay (in-naturals)]
              #:unless (for/or ([d (in-list ds)]
                                [r (in-list rs)])
                         (caught? d r delay)))
    delay))
