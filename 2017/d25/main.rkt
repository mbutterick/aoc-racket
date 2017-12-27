#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]) ★ ★★)

(define (read-syntax path port)
  (define lines (for/list ([ln (in-lines port)])
                  (string-trim ln #px"\\W")))
  (strip-context #`(module mod "main.rkt"
                     #,(for/list ([datum (in-port read (open-input-string (car lines)))])
                         datum)
                     #,(list
                        (string->symbol (last (string-split (second lines))))
                        (string->number (car (take-right (string-split (third lines)) 2))))
                     #,@(for/list ([state-line-group (in-list (filter-split (cdddr lines) (λ (ln) (equal? ln ""))))])
                          (for/list ([state-line (in-list state-line-group)])
                            (read (open-input-string (last (string-split state-line)))))))))

(define-macro (#%mb (STARS) (STATE STEPS) (TOK ...) ...)
  #`(#%module-begin
     (time (STARS STATE STEPS (TOK ...) ...))))

(define-macro (★ STATE STEPS (TOK ...) ...)
  #'(begin
      (define-state TOK ...) ...
      (run STATE STEPS)))

(define tape (make-hasheqv))

(define (read-tape pos) (hash-ref! tape pos 0))
(define (write-tape pos val) (hash-set! tape pos val))
(define (change-pos pos dir) (+ pos (if (eq? dir 'left) -1 1)))

(define-macro (define-state STATE 0 VAL0 DIR0 THEN0 1 VAL1 DIR1 THEN1)
  #'(define (STATE pos)
      (case (read-tape pos)
        [(0)
         (write-tape pos VAL0)
         (values (change-pos pos 'DIR0) THEN0)]
        [(1)
         (write-tape pos VAL1)
         (values (change-pos pos 'DIR1) THEN1)])))

(define (run state steps)
  (for/fold ([pos 0]
             [state state]
             #:result (apply + (hash-values tape)))
            ([step (in-range steps)])
    (state pos)))
