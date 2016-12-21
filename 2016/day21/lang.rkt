#lang br/quicklang ;; http://adventofcode.com/2016/day/21
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   (let ([lines (string-split (port->string port) "\n")])
     #`(module mod "lang.rkt"
         #,(car lines)
         #,@(for/list ([args (in-list (map string-split (cdr lines)))])
              `(inst ,@(map (λ(arg) (or (string->number arg) arg)) args)))))))

(define-macro (mb CODE . INSTS)
  #'(#%module-begin
     (define (explode str) (regexp-match* #rx"." str))
     (define (scramble code)
       (apply string-append
              (vector->list
               (for/fold ([code (apply vector (explode code))])
                         ([proc (in-list (list . INSTS))])
                 (proc code)))))
     (scramble CODE)
     (for*/first ([uplist (in-permutations (explode "fbgdceah"))]
                  [up (in-value (string-join uplist ""))]
                  #:when (equal? (scramble up) "fbgdceah"))
       up)))

(define-macro-cases inst
  [(_ "swap" "position" X "with" "position" Y) #'(swap-position X Y)]
  [(_ "swap" "letter" X "with" "letter" Y) #'(swap-letter X Y)]
  [(_ "reverse" "positions" X "through" Y) #'(reverse-letters X Y)]
  [(_ "rotate" DIR NUM _) #'(rotate DIR NUM)]
  [(_ "move" "position" X "to" "position" Y) #'(move X Y)]
  [(_ "rotate" "based" "on" "position" "of" "letter" X) #'(rotate-letter X)])
(provide inst)

(define (swap-position xidx yidx)
  (λ(v)
    (define tmp (vector-ref v xidx))
    (vector-set*! v xidx (vector-ref v yidx) yidx tmp)
    v))

(define (swap-letter x y)
  (λ(v) ((swap-position (vector-member x v) (vector-member y v)) v)))

(define (reverse-letters xidx yidx)
  (λ(v)
    (define letter-idxs (range xidx (add1 yidx)))
    (define letters
      (for/list ([idx (in-list letter-idxs)])
        (vector-ref v idx)))
    (for ([idx (in-list (reverse letter-idxs))]
          [letter (in-list letters)])
      (vector-set! v idx letter))
    v))

(require sugar/list)
(define (rotate dir num)
  (λ(v)
    (list->vector
     ((if (equal? "left" dir)
          shift-left-cycle
          shift-cycle) (vector->list v) num))))

(define (rotate-letter x)
  (λ(v)
    (define xidx (vector-member x v))
    (define rotval (+ 1 xidx (if (>= xidx 4) 1 0)))
    ((rotate "right" rotval) v)))

(define (move xidx yidx)
  (λ(v)
    (define xs (vector->list v))
    (define-values (head tail) (split-at xs xidx))
    (define x (car tail))
    (define new-xs (append head (cdr tail)))
    (define-values (newhead newtail) (split-at new-xs yidx))
    (list->vector (append newhead (list x) newtail))))
    

