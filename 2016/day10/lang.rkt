#lang br/quicklang
(require racket/string (for-syntax syntax/strip-context))
;; http://adventofcode.com/2016/day/10

(provide read-syntax)
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (equal? str "")))
            (format-datum '~a str)))))

(define-macro (mb ARG ...)
  #'(#%module-begin
     ARG ...
     ))
(provide (rename-out [mb #%module-begin]))

(define bots (make-hash))
(define outputs (make-hash))

(define chip-comparison-key '(17 61))
(define-macro (value VAL goes to TYPE NUM)
  (with-pattern ([HASH-NAME (replace-context #'here (suffix-id #'TYPE "s"))])
    #'(begin
        (hash-update! HASH-NAME NUM (λ (val) (cons VAL val)) empty)
        #;(displayln (format "bots: ~v" bots))
        #;(displayln (format "outputs: ~v" outputs)))))
(provide value)

(define (bot-low bot)
  (define botvals (hash-ref bots bot))
  (define minval (car (sort botvals <)))
  (hash-set! bots bot (remove minval botvals))
  minval)

(define (bot-high bot)
  (define botvals (hash-ref bots bot))
  (define maxval (car (sort botvals >)))
  (hash-set! bots bot (remove maxval botvals))
  maxval)

; bot BOT gives low to LOW-TYPE LOW-NUM and high to HIGH-TYPE HIGH-NUM
(define-macro (bot BOT gives low to LOW-TYPE LOW-NUM and high to HIGH-TYPE HIGH-NUM)
  #'(module+ main
      (when (equal? (sort (hash-ref bots BOT) <) (sort chip-comparison-key <))
        (displayln (format "answer: bot ~a holds ~a chips" BOT chip-comparison-key)))
      (value (bot-low BOT) goes to LOW-TYPE LOW-NUM)
      (value (bot-high BOT) goes to HIGH-TYPE HIGH-NUM)))
(provide bot and)