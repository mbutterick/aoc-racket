#lang typed/racket
(require typed/rackunit
         (for-syntax racket/file racket/string))
(provide (all-defined-out))

(define-syntax (convert-input-to-wire-functions stx)
  (syntax-case stx ()
    [(_)
     (let* ([input-strings (file->lines "../day07-input.txt")]
            [wire-strings (map (λ(str) (format "(wire ~a)" str)) input-strings)]
            [wire-datums (map (compose1 read open-input-string) wire-strings)])
       (datum->syntax stx `(begin ,@wire-datums)))]))

(define-syntax (wire stx)
  (syntax-case stx (->)
    [(_ arg -> wire-name)
     #'(define (wire-name) : Integer (evaluate-arg arg))]
    [(_ 16bit-op arg -> wire-name)
     #'(define (wire-name) : Integer (16bit-op (evaluate-arg arg)))]
    [(_ arg1 16bit-op arg2 -> wire-name)
     #'(define (wire-name) : Integer (16bit-op (evaluate-arg arg1) (evaluate-arg arg2)))]
    [(_ expr) #'(begin expr)]
    [else #'(void)]))

(convert-input-to-wire-functions)

(define-type Wire-Cache (HashTable (-> Integer) Integer))

(: wire-value-cache Wire-Cache)
(define wire-value-cache (make-hash))

(: evaluate-arg (-> (U Integer (-> Integer)) Integer))
(define (evaluate-arg x)
  (cond
    [(procedure? x) (hash-ref! wire-value-cache x x)]
    [else x]))

(: 16bitize (-> Integer Integer))
(define (16bitize x)
  (define 16bit-max (expt 2 16))
  (define r (modulo x 16bit-max))
  (cond
    [(negative? r) (16bitize (+ 16bit-max r))]
    [else r]))

;;bg: edits
(define-syntax-rule (define-16bit id [dom ...] proc)
  (define (id [dom : Integer] ...) : Integer (16bitize (proc dom ...))))

;;bg: added arity decl
(define-16bit AND [x y] bitwise-and)
(define-16bit OR [x y] bitwise-ior)
(define-16bit LSHIFT [x y] arithmetic-shift)
(define-16bit RSHIFT [x y] (λ ([x : Integer] [y : Integer]) (arithmetic-shift x (- y))))
(define-16bit NOT [x] bitwise-not)

(: q1 (-> Integer))
(define (q1) (a))

(compile-enforce-module-constants #f)

(: q2 (-> Integer))
(define (q2)
  (define first-a-val (a))
  (set! b (λ () first-a-val))
  (set! wire-value-cache (ann (make-hash) Wire-Cache))
  (a))


(module+ test
  (check-equal? (q1) 46065)
  (check-equal? (q2) 14134))

