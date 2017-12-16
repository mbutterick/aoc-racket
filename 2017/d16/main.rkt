#lang reader "../aoc-lang.rkt"
(provide (rename-out [#%mb #%module-begin]) ★ ★★)

(define-macro (#%mb (STARS SIZE) (MOVE ...) ...)
  #`(#%module-begin
     (time (STARS SIZE '(MOVE ...)) ...)))

(define (spin ps num)
  (define ps-len (vector-length ps))
  (for/vector ([idx (in-range ps-len)])
    (vector-ref ps (modulo (- idx num) ps-len))))

(define (exchange ps left right)
  (define new-vec (vector-copy ps))
  (vector-set*! new-vec
                left (vector-ref ps right)
                right (vector-ref ps left))
  new-vec)
  
(define (partner ps left right)
  (exchange ps (vector-member left ps) (vector-member right ps)))

(define (starting-vec size)
  (list->vector (take '(a b c d e f g h i j k l m n o p q r s t) size)))

(define (thunkify move)
  (define move-str (symbol->string move))
  (cond
    [(regexp-match #px"(?<=^s)\\d+(?=$)" move-str)
     => (λ (m)
          (define num (string->number (car m)))
          (λ (ps) (spin ps num)))]
    [(regexp-match #px"(?<=^x)\\d+/\\d+(?=$)" move-str)
     => (λ (m)
          (define nums (map string->number (string-split (car m) "/")))
          (λ (ps) (apply exchange ps nums)))]
    [(regexp-match #px"(?<=^p)\\w/\\w(?=$)" move-str)
     => (λ (m)
          (define syms (map string->symbol (string-split (car m) "/")))
          (λ (ps) (apply partner ps syms)))]
    [else (error move-str)]))

(define (dance vec moves [rounds 1])
  (for*/fold ([vec vec])
             ([round (in-range rounds)]
              [move-thunk (in-list (map thunkify moves))])
    (move-thunk vec)))

(define (★ size moves [rounds 1])
  (string-append* (map ~a (vector->list (dance (starting-vec size) moves rounds)))))

(define (★★ size moves)
  (define cycle-length (for/fold ([vecs (list (starting-vec size))]
                                  #:result (length (cdr vecs)))
                                 ([i (in-naturals)]
                                  #:break (member (car vecs) (cdr vecs)))
                         (cons (dance (car vecs) moves) vecs)))
  (define one-billion (expt 10 9))
  (define iterations-needed (modulo one-billion cycle-length))
  (★ size moves iterations-needed))

