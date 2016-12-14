#lang br/quicklang

(provide read-syntax)
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([ip (in-lines port)]
                     #:when (not (equal? ip "")))
            ip))))

(provide (rename-out [mb #%module-begin]))
(define-macro (mb . IPS)
  #'(#%module-begin
     (define ips (list . IPS))
     (displayln (format "part a: ~a" (length (filter supports-tls? ips))))
     (displayln (format "part b: ~a" (length (filter supports-ssl? ips))))))

(define bracketed-pattern #rx"\\[.*?\\]")

(define (bracketed-parts str)
  (regexp-match* bracketed-pattern str))

(define (unbracketed-parts str)
  (string-split str bracketed-pattern))

(define (has-abba? str)
  (for*/or ([idx (in-range (string-length str))]
            [substr (in-list (regexp-match* #px"^\\w\\w\\w\\w" str idx))]
            #:when substr)
    (define cs (string->list substr))
    (and
     (char=? (first cs) (fourth cs))
     (char=? (second cs) (third cs))
     (not (char=? (first cs) (second cs))))))

(define (supports-tls? str)
  (and
   (ormap has-abba? (unbracketed-parts str))
   (andmap (negate has-abba?) (bracketed-parts str))))

(define (find-abas str)
  (for*/list ([idx (in-range (string-length str))]
              [substr (in-list (regexp-match* #px"^\\w\\w\\w" str idx))]
              [cs (in-value (string->list substr))]
              #:when (and
                      substr
                      (char=? (first cs) (third cs))
                      (not (char=? (first cs) (second cs)))))
    substr))

(define (aba->bab aba) (format "~a~a~a" (substring aba 1 2)
                               (substring aba 0 1)
                               (substring aba 1 2)))

(define (supports-ssl? str)
  (define abas (append-map find-abas (unbracketed-parts str)))
  (for*/or ([bab (in-list (map aba->bab abas))]
            [str (in-list (bracketed-parts str))])
    (regexp-match bab str)))
