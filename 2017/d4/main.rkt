#lang br/quicklang
(require "../helper.rkt")

(provide read-syntax)
(define (read-syntax path port)
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([line (in-lines port)])
                          (with-input-from-string line (λ ()
                                                         (for/list ([datums (in-port)])
                                                           datums)))))))

(provide (rename-out [#%mb #%module-begin]))
(define-macro (#%mb (STARS) (WORD ...) ...)
  #'(#%module-begin
     (for/sum ([ws (in-list '((WORD ...) ...))]
               #:when (no-duplicates? ws #:anagrams? (eq? 'STARS '★★)))
       1)))

(define (sort-chars word)
  (sort (string->list (symbol->string word)) char<?))

(define (no-duplicates? ws #:anagrams? [anagrams #f])
  (let ([ws (if anagrams (map sort-chars ws) ws)])
    (= (length ws) (length (remove-duplicates ws)))))


