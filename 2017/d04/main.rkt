#lang reader "../aoc-lang.rkt"

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
