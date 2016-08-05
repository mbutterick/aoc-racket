#lang typed/racket
(require typed/rackunit typed/json)
(provide (all-defined-out))
(: string->jsexpr (-> String JSExpr))
(define (string->jsexpr str)
  (define bg (read-json (open-input-string str)))
  (if (eof-object? bg)
    (error 'day12:eof)
    bg))

(define-type JSTree JSExpr) ;(Rec T (U JSExpr (Listof T))))

(: flatten-jsexpr (-> JSExpr (Listof Any)))
(define (flatten-jsexpr jsexpr)
  (flatten
   (let loop : Any ([x : Any jsexpr])
     (cond
       [(list? x)
        (map loop x)]
       [(hash? x)
        (loop (flatten (hash->list x)))]
       [else x]))))

(: q1 (-> String Number))
(define (q1 input-str)
  (define json-items (flatten-jsexpr (string->jsexpr input-str)))
  (apply + (filter number? json-items)))

(: flatten-jsexpr-2 (-> JSExpr (Listof Any)))
(define (flatten-jsexpr-2 jsexpr)
  (flatten
   (let loop : Any ([x : Any jsexpr])
     (cond
       [(list? x)
        (map loop x)]
       [(hash? x)
        (if (member "red" (hash-values x))
            empty
            (loop (flatten (hash->list x))))]
       [else x]))))

(: q2 (-> String Number))
(define (q2 input-str)
  (define json-items (flatten-jsexpr-2 (string->jsexpr input-str)))
  (apply + (filter number? json-items)))

(module+ test
  (define input-str (file->string "../day12-input.txt"))
  (check-equal? (q1 input-str) 191164)
  (check-equal? (q2 input-str) 87842))

