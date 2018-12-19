#lang debug br
(require racket/file)

(define (★)
  (match-define (list player-count marble-count)
    (map string->number (regexp-match* #px"\\d+" (file->string "09.txt"))))
  (define scores (make-hasheqv))
  (let loop ([marble 1] [pos 0] [circle (list 0)])     
    (cond
      [(> marble marble-count) (argmax cdr (hash->list scores))]
      [(zero? (modulo marble 23))
       (define len (length circle))
       (define deletion-pos (modulo (+ (- pos 7) len) len))
       (define-values (head tail) (split-at circle deletion-pos))
       (define player (modulo marble player-count))
       (hash-update! scores player (λ (sc) (+ marble (car tail) sc)) 0)
       (loop (add1 marble) deletion-pos (append head (cdr tail)))]
      [else
       (define next-pos (add1 (modulo (add1 pos) (length circle))))
       (define-values (head tail) (split-at circle next-pos))
       (loop (add1 marble) next-pos (append head (list marble) tail))])))

#;(define (★★)
    )
#;(★★)

#;(module+ test
    (require rackunit)
    (check-equal? (time (★)) 437654)
    (check-equal? (time (★★)) 566))