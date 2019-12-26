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


;; 2
#|
The idea is that the numbers are too large to shuffle the deck directly.
Unlike the previous question, this one asks what card is at a certain position.
So we want to go in reverse: just trace that position backwards through each operation
and figure out where we are at the start.
Also, we are likely going to discover a cycle in the transformations
(that is, card 2020 returns to position 2020 after a while)
and we can use this cycle length to reduce the number of times we have to execute the (un)shuffling.
("Likely" because otherwise this puzzle cannot be computed in reasonable time.)
|#

(define (unstack-pos cardlen pos) (- (sub1 cardlen) pos))

(check-eq? (unstack-pos 10 0) 9)
(check-eq? (unstack-pos 10 9) 0)

(define (uncut-pos cardlen pos n)
  (define cut-pos (match n
                      [(? positive?) n]
                      [_ (- cardlen (abs n))]))
  (cond
    [(<= (- cardlen cut-pos) pos) (- pos (- cardlen cut-pos))]
    [else (+ pos cut-pos)]))

(check-eq? (uncut-pos 10 7 3) 0)
(check-eq? (uncut-pos 10 0 3) 3)
(check-eq? (uncut-pos 10 7 -4) 3)
(check-eq? (uncut-pos 10 0 -4) 6)

(define (unincrement-pos cardlen pos inc)
  42)