#lang br/quicklang

(define (read-syntax path port)
  (define moveset-strs (string-split (port->string port)))
  (define moveset-datums
    (for*/list ([msstr (in-list moveset-strs)])
               `(moveset ,@(regexp-match* #rx"." msstr))))
  (strip-bindings
   #`(module day01-mod "lang.rkt"
       #,@moveset-datums)))
(provide read-syntax)

(define-macro moveset #'list)
(provide moveset)

(define-macro (mb . MOVESETS)
  #'(#%module-begin
     (void (solve (list . MOVESETS)))))
(provide (rename-out [mb #%module-begin]))

(define (do-moveset button moveset)
  (for/fold ([button button])
            ([move (in-list moveset)])
    (vector-ref
     (case move
       [("U") '#(1 2 3 1 2 3 4 5 6)]
       [("L") '#(1 1 2 4 4 5 7 7 8)]
       [("R") '#(2 3 3 5 6 6 8 9 9)]
       [("D") '#(4 5 6 7 8 9 7 8 9)])
     (sub1 button))))

(define starting-button 5)
(define (solve mss)
  (for/fold ([button starting-button])
            ([ms (in-list mss)])
    (define result (do-moveset button ms))
    (display (string-upcase (number->string result 16)))
    result))