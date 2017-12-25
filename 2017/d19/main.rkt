#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]) ★ ★★)

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read (open-input-string (car lines)))])
                          datum)
                     #,@(cdr lines))))

(define-macro (#%mb STARS LINE ...)
  #'(#%module-begin
     (time (STARS (list LINE ...)))))

(define (traverse lines #:count-steps [count-steps? #f])
  (define chars (for*/hasheqv ([(line lidx) (in-indexed lines)]
                               [(c cidx) (in-indexed (string->list line))]
                               #:unless (char=? c #\space))
                  (values (+ lidx (* +i cidx)) c)))
  (define start (for/first ([k (in-hash-keys chars)]
                            #:when (zero? (real-part k)))
                  k))
  (define down 1) (define up -1) (define left -i) (define right +i)
  (let loop ([here start][dir down][path null])
    (define here-char (hash-ref chars here #f))
    (cond
      [here-char
       (define next-dir (if (char=? here-char #\+)
                            (if (hash-has-key? chars (+ here (* dir right))) (* dir right) (* dir left))
                            dir))
       (loop (+ here next-dir) next-dir (cons here-char path))]
      [count-steps? (length path)]
      [else (list->string (filter char-alphabetic? (reverse path)))]))) 

(define (★ lines) (traverse lines))

(define (★★ lines) (traverse lines #:count-steps #t))
                                   
