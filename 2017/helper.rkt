#lang br
(require syntax/strip-context sugar)
(provide (all-defined-out) (all-from-out syntax/strip-context sugar))

(define ★ '★) (define ★★ '★★)

(define (port->datums port)
  (for/list ([datum (in-port read port)])
    datum))

(define (number->digits num)
  (for/list ([c (in-string (number->string num))])
    (string->number (string c))))

(define (dirname path)
  (define-values (dir name _) (split-path path))
  dir)

(define (=* . xs)
  (or (< (length xs) 2) (apply = xs)))

(define (app x . args) (apply x args))