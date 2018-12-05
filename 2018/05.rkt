#lang debug br
(require racket/file racket/set)

(define fp (open-input-file "05.txt"))

(define fpbs (port->bytes fp))

(define-macro (for/mutable-list (SEQ ...) . BODY)
  #'(let loop ([mprs null][xs (reverse (for/list (SEQ ...) . BODY))])
      (if (empty? xs)
          mprs
          (loop (mcons (car xs) mprs) (cdr xs)))))

(define (reactive-pair? mprs)
  (= 32 (abs (- (mcar (mcdr mprs)) (mcar (mcdr (mcdr mprs)))))))

(define (react [fpbs fpbs])
  (define mprs0 (mcons #f (for/mutable-list ([b (in-bytes fpbs)])
                                            b)))
  (let loop ([mprs mprs0][found? #false])
    (cond
      [(or (empty? mprs)
           (empty? (mcdr mprs))
           (empty? (mcdr (mcdr mprs))))
       (if found? (loop mprs0 #false) mprs0)]
      [(reactive-pair? mprs)
       (set-mcdr! mprs (mcdr (mcdr (mcdr mprs))))
       (loop (mcdr mprs) #true)]
      [else (loop (mcdr mprs) found?)]))
  (for/sum ([val (in-mlist (mcdr mprs0))])
           1))

(define (★)
  (react fpbs))

(define possible-units
  (map char->integer (remove-duplicates (map char-upcase (map integer->char (bytes->list fpbs))) char=?)))

(define (remove-unit fbps unit)
  (regexp-replace* (regexp (format "[~a~a]" (integer->char unit) (integer->char (+ unit 32)))) fbps ""))

(define (★★)
  (apply min (for/list ([unit (in-list possible-units)])
                       (react (remove-unit fpbs unit)))))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 10564)
  (check-equal? (time (★★)) 6336))