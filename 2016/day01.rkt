#lang br/quicklang
(require racket/file)
(module+ reader
  (provide read-syntax)
  (define (read-syntax path port)
    (strip-bindings
     #`(module day01-mod "day01.rkt"
         #,@(for/list ([t (in-list (string-split (port->string port) ", "))])
                      `(turn ,@(string-split (string-trim t) #px"(?<=[LR])")))))))

(define-macro (mb . TURNS)
  #'(#%module-begin
     (solve . TURNS)))
(provide (rename-out [mb #%module-begin]))

(define (solve . turns)
  (define loc
    (let loop ([loc 0]
               [dir 1]
               [turns turns])
      (if (empty? turns)
          loc
          (let* ([turn (car turns)]
                 [rotation (car turn)]
                 [dist (cdr turn)]
                 [new-dir (* dir rotation)])
            (loop (+ loc (* new-dir dist)) new-dir (cdr turns))))))
  (+ (abs (imag-part loc)) (abs (real-part loc))))

(define-macro (turn DIR DIST)
  (with-pattern ([NEW-DIR (syntax-case #'DIR ()
                            ["L" #'+i]
                            ["R" #'-i])])
    #'(cons NEW-DIR (string->number DIST))))
(provide turn)