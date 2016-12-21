#lang br/quicklang ;; http://adventofcode.com/2016/day/20
(provide read-syntax
         cons
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([line (in-list (string-split (port->string port) "\n"))])
            `(cons ,@(map string->number (string-split line "-")))))))

(define-macro (mb . RANGE-PAIRS)
  #'(#%module-begin
     (define range-pairs (sort (list . RANGE-PAIRS) < #:key car))
     (solve-a range-pairs)
     (solve-b range-pairs)))

(define (solve-a range-pairs)
  (for/first ([left (in-list range-pairs)]
              [right (in-list (cdr range-pairs))]
              #:when (> (- (car right) (cdr left)) 1))
    (add1 (cdr left))))

(define (find-overlap n ranges)
  (and (pair? ranges)
       (for/first ([r (in-list ranges)]
                   #:when (<= (car r) n (cdr r)))
         r)))
       
(define (solve-b range-pairs)
  (define rps (for/fold ([rps empty])
                        ([rp (in-list (append range-pairs '((4294967295 . 4294967295))))])
                (define next-rps rps)
                (define left (or
                              (let ([result (find-overlap (car rp) rps)])
                                (and result (set! next-rps (remove result next-rps)) (car result)))
                              (car rp)))
                (define right (or
                               (let ([result (find-overlap (cdr rp) rps)])
                                 (and result (set! next-rps (remove result next-rps)) (cdr result)))
                               (cdr rp)))
                (cons (cons left right) next-rps)))
  (define sorted-rps (sort rps < #:key car))
  (for/sum ([left (in-list sorted-rps)]
            [right (in-list (cdr sorted-rps))]
            #:when (> (- (car right) (cdr left)) 1))
    (- (car right) (cdr left) 1)))