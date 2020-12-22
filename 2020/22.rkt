#lang br
(require racket/file rackunit racket/set)

(struct result (who cards) #:transparent)

(match-define (list p1s p2s)
  (let* ([str  (file->string "22.rktd")]
         [strs (string-split str "\n\n")])
    (map (λ (str) (cdr (map string->number (string-split str "\n")))) strs)))

(define (play-game p1s p2s [recursive #false])
  (define game-states (mutable-set))
  (let/ec exit
    (for/fold ([p1s p1s] [p2s p2s]
               #:result (if (empty? p1s) (result 'p2 p2s) (result 'p1 p1s)))
              ([i (in-naturals)]
               #:break (or (empty? p1s) (empty? p2s)))
      (match-define (list (cons p1 p1s-rest) (cons p2 p2s-rest)) (list p1s p2s))
      (define (p1-wins) (values (append p1s-rest (list p1 p2)) p2s-rest))
      (define (p2-wins) (values p1s-rest (append p2s-rest (list p2 p1))))
      (cond
        [(and recursive
              (let ([game-state (cons p1s p2s)])
                (when (set-member? game-states game-state)
                  (exit (result 'p1 p1s)))
                (set-add! game-states game-state))
              (<= p1 (length p1s-rest)) (<= p2 (length p2s-rest)))
         (match (play-game (take p1s-rest p1) (take p2s-rest p2) #true)
           [(result 'p1 _) (p1-wins)]
           [_ (p2-wins)])]
        [(> p1 p2) (p1-wins)]
        [else (p2-wins)]))))

(define (winner->score winner)
  (for/sum ([(val i) (in-indexed (reverse (result-cards winner)))])
    (* val (add1 i))))

(check-equal? (winner->score (play-game p1s p2s)) 30138)

(check-equal? (winner->score (play-game p1s p2s #true)) 31587)