#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]))

(define (read-syntax path port)
  (strip-context #`(module mod "main.rkt"
                     #,@(port->lines port))))

(define-macro (#%mb STARS-LINE SEXP-LINE ...)
  #`(#%module-begin
     (if (eq? (process-line STARS-LINE) '★)
         (score (process-line SEXP-LINE))
         (process-line SEXP-LINE #t))
     ...))

(define (process-line line [garbage #f])
  (define gchars 0)
  (let* ([line (string-trim line)]
         [line (regexp-replace* #rx"!." line "")]
         [line (regexp-replace* #rx"<.*?>" line
                                (λ (m) (set! gchars (+ gchars (string-length m) -2)) ""))]
         [line (regexp-replace* #rx"," line "")])
    (if garbage gchars (read (open-input-string line)))))
  
(define (score tree [start 0])
  (+ (add1 start) (for/sum ([x (in-list tree)])
                    (score x (add1 start)))))
