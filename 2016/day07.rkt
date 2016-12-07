#lang br/quicklang

(module+ reader (provide read-syntax))
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "day07.rkt"
       #,@(for/list ([ip (in-lines port)]
                     #:when (not (equal? ip "")))
            ip))))

(provide (rename-out [mb #%module-begin]))
(define-macro (mb . IPS)
  #'(#%module-begin
     (length (filter supports-tls? (list . IPS)))
     (map supports-tls? (list . IPS))))

(define (supports-tls? x)
  (and
   (let ([maybe-abbas (regexp-match* #px"(\\w)(\\w)\\2\\1" x)])
     (ormap (λ(ma)
              (not (equal? (substring ma 0 1) (substring ma 1 2)))) maybe-abbas))
   (let ([maybe-bad-abbas (regexp-match* #px"\\[.*((\\w)(\\w)\\3\\2).*\\]" x #:match-select cadr)])
     (andmap (λ(ma)
               (equal? (substring ma 0 1) (substring ma 1 2))) maybe-bad-abbas))))
