#lang typed/racket
(require typed/rackunit
         (for-syntax racket/file racket/string sugar/debug))
(provide (all-defined-out))

(require/typed racket/base
  (hash-set*! (-> (HashTable Symbol Integer) Symbol Integer Symbol Integer Void)))

(define-syntax (convert-input-to-instruction-functions stx)
  (syntax-case stx ()
    [(_)
     (let* ([input-strings (file->lines "../day23-input.txt")]
            [inst-strings (map (λ(str) (format "(λ(_) (inst ~a))" (string-replace str "," ""))) input-strings)] ;;bg; removed thunk*
            [inst-datums (map (compose1 read open-input-string) inst-strings)])
       (datum->syntax stx `(define instructions : (Listof (-> Integer Integer)) (list ,@inst-datums))))]))

(: registers (HashTable Symbol Integer))
(define registers (make-hash '((a . 0)(b . 0))))

(define default-offset 1)

(define-syntax-rule (define-reg-updater id thunk)
  (define (id (reg : Symbol)) : Integer
    (hash-update! registers reg thunk)
    default-offset))

(define-reg-updater tpl (λ([val : Integer]) (* 3 val)))
(define-reg-updater inc (λ([val : Integer]) (add1 val)))
(define-reg-updater hlf (λ([val : Integer]) (cast (/ val 2) Integer)))

(: jmpf (-> Symbol Integer (-> Integer Any) Integer))
(define (jmpf reg num pred)
  (if (pred (hash-ref registers reg (λ () -1))) num 1))

(define-syntax (inst stx)
  (syntax-case stx (jmp jio jie)
    [(_ jio reg num)
     #'(jmpf 'reg num (curry = 1))]
    [(_ jie reg num)
     #'(jmpf 'reg num even?)]
    [(_ jmp num)
     #'num]
    [(_ op reg)
     #'(op 'reg)]))

(convert-input-to-instruction-functions)

(: q1 (-> Integer))
(define (q1)
  (let eval-instruction : Integer ([idx 0])
    (if (>= idx (length instructions))
        (hash-ref registers 'b)
        (let* ([inst (list-ref instructions idx)]
               [jump-offset (inst -1)] ;;bg
               [next-idx (+ jump-offset idx)])
          (eval-instruction next-idx)))))

(: q2 (-> Integer))
(define (q2)
  (hash-set*! registers 'a 1 'b 0)
  (q1))

(module+ test
  (check-equal? (q1) 184)
  (check-equal? (q2) 231))


