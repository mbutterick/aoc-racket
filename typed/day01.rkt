#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(define up-char #\()
(define down-char #\))

(: make-matcher (-> Char (-> String Natural)))
(define (make-matcher c)
  (λ((str : String)) (length (regexp-match* (regexp (format "\\~a" c)) str))))
(define get-ups (make-matcher up-char))
(define get-downs (make-matcher down-char))
(: get-destination (-> String Integer))
(define (get-destination str) (- (get-ups str) (get-downs str)))

(: q1 (-> String Integer))
(define (q1 str)
  (get-destination str))

(: elevator-string->ints (-> String (Listof Integer)))
(define (elevator-string->ints str)
  (for/list : (Listof Integer)
            ([c (in-string str)])
            (if (equal? c up-char)
                1
                -1)))

(: q1-alt (-> String Integer))
(define (q1-alt str)
  (apply + (elevator-string->ints str)))

(: in-basement? (-> (Listof Integer) Boolean))
(define (in-basement? movements)
  (negative? (apply + movements)))  

(: q2 (-> String Integer))
(define (q2 str)
  (define relative-movements
    (for/fold : (Listof Integer)
              ([movements-so-far : (Listof Integer) empty])
              ([c (in-string str)]
               ;#:break (in-basement? movements-so-far)
               )
      (if (in-basement? movements-so-far)
        movements-so-far
        (cons (get-destination (~a c)) movements-so-far))))
  (length relative-movements))

(: q2-for/first (-> String (U #f Integer)))
(define (q2-for/first str)
  (define basement-position
    (let ([ints (elevator-string->ints str)])
      (for/or : (U #f Integer) ;;bg first=>or
                 ([idx (in-range (length ints))]
                  #:when (negative? (apply + (take ints idx))))
                 idx)))
  basement-position)

(: q2-for/or (-> String (U #f Integer)))
(define (q2-for/or str)
  (define basement-position
    (let ([ints (elevator-string->ints str)]) 
      (for/or : (U #f Integer)
              ([idx (in-range (length ints))])
              (and (negative? (apply + (take ints idx))) idx))))
  basement-position)


(module+ test
  (define input-str (file->string "../day01-input.txt"))
  (check-equal? (q1 input-str) 74)
  (check-equal? (q1-alt input-str) 74)
  (check-equal? (q2 input-str) 1795)
  (check-equal? (q2-for/first input-str) 1795)
  (check-equal? (q2-for/or input-str) 1795))


