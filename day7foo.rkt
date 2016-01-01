#lang racket

(require racket rackunit
                (for-syntax racket/base racket/file racket/string))


 (define-syntax (convert-input-to-wire-functions stx)
         (syntax-case stx ()
           [(_)
            (let* ([input-strings (file->lines "day7-input.txt")]
                   [wire-strings (map (λ(str) (format "(wire ~a)" str)) input-strings)]
                   [wire-datums (map (compose1 read open-input-string) wire-strings)])
              (datum->syntax stx `(begin ,@wire-datums)))]))
       
       (define-syntax (wire stx)
         (syntax-case stx (->)
           [(_ arg -> id)
            #'(define (id) (get-val arg))]
           [(_ op arg -> id)
            #'(define (id) (op (get-val arg)))]
           [(_ arg1 op arg2 -> id)
            #'(define (id) (op (get-val arg1) (get-val arg2)))]
           [(_ expr) #'(begin expr)]
           [else #'(void)]))
       
       (convert-input-to-wire-functions)
       (require sugar/debug)
       (define wire-value-cache (make-hash))
       (define (get-val x)
         (report x)
         (if (number? x) #t #f))
       
       (define (16bitize x)
         (define 16bit-max (expt 2 16))
         (define r (modulo x 16bit-max))
         (if (negative? r)
             (16bitize (+ 16bit-max r))
             r))
       
       (define-syntax-rule (define-16bit id proc) (define id (compose1 16bitize proc)))
       (define-16bit AND bitwise-and)
       (define-16bit OR bitwise-ior)
       (define-16bit LSHIFT arithmetic-shift)
       (define-16bit RSHIFT (λ(x y) (arithmetic-shift x (- y))))
       (define-16bit NOT bitwise-not)