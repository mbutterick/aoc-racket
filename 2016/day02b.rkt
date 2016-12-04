#lang br/quicklang
(module+ reader
  (provide read-syntax)
  (define (read-syntax path port)
    (define moveset-strs (string-split (port->string port)))
    (define moveset-datums
      (for*/list ([msstr (in-list moveset-strs)])
                 `(moveset ,@(regexp-match* #rx"." msstr))))
    (strip-bindings
     #`(module day01-mod "day02b.rkt"
         #,@moveset-datums))))

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
       [("U") '#(1 2 1 4 5 2 3 4 9 6 7 8 #xb)]
       [("L") '#(1 2 2 3 5 5 6 7 8 #xa #xa #xb #xd)]
       [("R") '#(1 3 4 4 6 7 8 9 9 #xb #xc #xc #xd)]
       [("D") '#(3 6 7 8 5 #xa #xb #xc 9 #xa #xd #xc #xd)])
     (sub1 button))))

(define starting-button 5)
(define (solve mss)
  (for/fold ([button starting-button])
            ([ms (in-list mss)])
    (define result (do-moveset button ms))
    (display (string-upcase (number->string result 16)))
    result))