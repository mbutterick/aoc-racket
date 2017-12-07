#lang reader "../aoc-lang.rkt"
(require sugar/cache)
(provide (rename-out [#%mb #%module-begin]))
(define current-stars (make-parameter #f))
(define-macro (#%mb (STARS) (NAME . TOKS) ...)
  #`(#%module-begin
     (current-stars 'STARS)
     (define names (list 'NAME ...))
     (handle (length names) NAME . TOKS) ...))

(define (weights-equal? subsyms)
  (or (null? subsyms)
      (andmap (λ (ss) (= ((car subsyms) 'wt) (ss 'wt))) (cdr subsyms))))

(define (unique-member ss subsyms)
  (= 1 (length (filter (λ (subsym) (= (ss 'wt) (subsym 'wt))) subsyms))))

(define (exceptional-subsym subsyms)
  (for/first ([ss (in-list subsyms)] #:when (unique-member ss subsyms)) ss))

(define (typical-subsym subsyms)
  (for/first ([ss (in-list subsyms)] #:unless (unique-member ss subsyms)) ss))

(define-macro-cases handle
  [(M TARGET-LEN SYM (NUM) -> SUBSYM ...)
   #'(begin (define (SYM [x #f])
              (match x
                ['wt (+ NUM (SUBSYM 'wt) ...)]
                [(? number?)
                 (define target-weight x)
                 (define subsyms (list SUBSYM ...))
                 (if (weights-equal? subsyms)
                     (displayln (format "~a is bad: needs to be ~a" 'SYM (- NUM (- (SYM 'wt) target-weight))))
                     (let ([next-target-weight ((typical-subsym subsyms) 'wt)])
                       ((exceptional-subsym subsyms) next-target-weight)))]
                [else (cons 'SYM (append (SUBSYM) ...))]))
            (module+ main (when (= TARGET-LEN (length (SYM)))
                            (case (current-stars)
                              [(★) 'SYM]
                              [else (SYM 0)]))))]
  [(M TARGET-LEN SYM (NUM)) #'(M TARGET-LEN SYM (NUM) ->)])