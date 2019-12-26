#lang br
(require racket/file rackunit)

(define (stack cards)
  (reverse cards))

(define (cut n cards)
  (define-values (head tail)
    (split-at cards (match n
                      [(? positive?) n]
                      [_ (- (length cards) (abs n))])))
  (append tail head))

(define (increment n cards)
  (define len (length cards))
  (define vec (make-vector len))
  (for ([(card idx) (in-indexed cards)])
    (vector-set! vec (modulo (* idx n) len) card))
  (vector->list vec))

(define (run program [deck-size 10])
  (for/fold ([cards (range deck-size)])
            ([ln (in-list (string-split program "\n"))])
    (match (string-split ln)
      [(list "cut" (app string->number num)) (cut num cards)]
      [(list _ _ "increment" (app string->number num)) (increment num cards)]
      [_ (stack cards)])))

(check-equal?
 (run "deal with increment 7
deal into new stack
deal into new stack")
 '(0 3 6 9 2 5 8 1 4 7))

(check-equal?
 (run "cut 6
deal with increment 7
deal into new stack")
 '(3 0 7 4 1 8 5 2 9 6))

(check-equal?
 (run "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1")
 '(9 2 5 8 1 4 7 0 3 6))

;; 1
(check-eq? (index-of (run (file->string "22.rktd") 10007) 2019) 4086)
