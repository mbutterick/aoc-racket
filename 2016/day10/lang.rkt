#lang br/quicklang
;; http://adventofcode.com/2016/day/10
(provide read-syntax)
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (equal? str "")))
            `(handle ,@(string-split str))))))

(define chip-comparison-key '(17 61))
(define-macro (mb . ARGS)
  #'(#%module-begin
     (begin . ARGS)
     (displayln
      (for/first ([(k v) (in-hash gates)]
                  #:when (equal? (sort (apply eval-gate k) <)
                                 (sort chip-comparison-key <)))
        k))
     (displayln (for/product ([i (in-range 3)])
                  (gate-low "output" (~a i))))))
(provide (rename-out [mb #%module-begin]))

(define gates (make-hash))

(define-macro-cases handle
  [(_ "value" VAL "goes" "to" TYPE NUM)
   #'(hash-update! gates (list TYPE NUM) (λ (val) (cons (λ () (let ([v VAL])
                                                                (if (string? v)
                                                                    (string->number v)
                                                                    v))) val)) empty)]
  [(_ "bot" BOT "gives" "low" "to" LOW-TYPE LOW-NUM "and" "high" "to" HIGH-TYPE HIGH-NUM)
   #'(begin
       (handle "value" (gate-low "bot" BOT) "goes" "to" LOW-TYPE LOW-NUM)
       (handle "value" (gate-high "bot" BOT) "goes" "to" HIGH-TYPE HIGH-NUM))])
(provide handle)

(require sugar/cache)
(define/caching (eval-gate type num)
  (for/list ([proc (in-list (hash-ref gates (list type num)))])
    (proc)))

(define (gate-low type num) (car (sort (eval-gate type num) <)))
(define (gate-high type num) (car (sort (eval-gate type num) >)))
