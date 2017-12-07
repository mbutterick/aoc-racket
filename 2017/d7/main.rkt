#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]))

(define current-stars (make-parameter #f))
(define current-target-len (make-parameter #f))

(define-macro (#%mb (STARS) (NAME . TOKS) ...)
  #`(#%module-begin
     (current-stars 'STARS)
     (current-target-len (length '(NAME ...)))
     (handle NAME . TOKS) ...))

(define (weights= . xs) (apply =* (map wt xs)))

(define (unique-weight x xs)
  (= 1 (length (filter values (map (curry weights= x) xs)))))

(define (unbalanced-subsym subsyms)
  (findf (curryr unique-weight subsyms) subsyms))

(define (balanced-subsym subsyms)
  (findf (negate (curryr unique-weight subsyms)) subsyms))

(struct prog (sym wt) #:transparent)

(define/caching (wt sym)
  (apply + (map prog-wt (flatten (sym)))))

(define-macro-cases handle
  [(M SYM (NUM) -> . SUBSYMS)
   #'(begin (define/caching (SYM [target-weight #f])
              (define subsyms (list . SUBSYMS))
              (if target-weight
                  (if (apply weights= subsyms)
                      (displayln (format "~a is bad: needs to be ~a" 'SYM (- NUM (- (wt SYM) target-weight))))
                      ((unbalanced-subsym subsyms) (wt (balanced-subsym subsyms))))
                  (cons (prog 'SYM NUM) (map app subsyms))))
            (module+ main
              (when (= (current-target-len) (length (flatten (SYM))))
                (if (eq? (current-stars) '★) 'SYM (SYM 'find-bad-weight)))))]
  [(M SYM (NUM)) #'(M SYM (NUM) ->)])