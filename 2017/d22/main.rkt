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

(define (infect lines bursts #:weak-mode [weak-mode? #f])
  (define chars (make-hasheqv))
  (define origin-adjust (/ (sub1 (string-length (car lines))) 2))
  (for* ([(line lidx) (in-indexed lines)]
         [(c cidx) (in-indexed (string->list line))])
    (define key (+ (- lidx origin-adjust) (* +i (- cidx origin-adjust))))
    (hash-set! chars key c))
  
  (define turn-left +i) (define turn-right -i)
  
  (define-macro (define-state ID CHAR)
    (with-pattern ([ID? (suffix-id #'ID "?")]
                   [SET-ID (prefix-id "set-" #'ID)])
      #'(begin (define ID CHAR)
               (define (SET-ID x) (hash-set! chars x ID))
               (define (ID? x) (eqv? (hash-ref! chars x #\.) ID)))))
  (define-state clean #\.)
  (define-state infected #\#)
  (define-state weakened #\W)
  (define-state flagged #\F)
  
  (for/fold ([here 0]
             [dir -1]
             [infections 0]
             #:result infections)
            ([burst (in-range bursts)])
    (match here
      [(? infected?)
       (define next-dir (* dir turn-right))
       ((if weak-mode? set-flagged set-clean) here)
       (values (+ here next-dir) next-dir infections)]
      [(? clean?)
       (define next-dir (* dir turn-left))
       ((if weak-mode? set-weakened set-infected) here)
       (values (+ here next-dir) next-dir ((if weak-mode? values add1) infections))]
      [(? weakened?)
       (define next-dir dir)
       (set-infected here)
       (values (+ here next-dir) next-dir (add1 infections))]
      [(? flagged?)
       (define next-dir (- dir))
       (set-clean here)
       (values (+ here next-dir) next-dir infections)])))

(define (★ lines) (infect lines 10000))

(define (★★ lines) (infect lines 10000000 #:weak-mode #t))
                                   
