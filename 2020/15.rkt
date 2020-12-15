#lang br
(require racket/file rackunit)

(define (solve str max-turns)
  (define nums (map string->number (string-split str ",")))
  (define previously-seen (make-hasheq))
  (for ([(num turn) (in-indexed (drop-right nums 1))])
    (hash-set! previously-seen num (add1 turn)))
  (for/fold ([last-seen (last nums)])
            ([turn (in-range (add1 (length nums)) (add1 max-turns))])
    (begin0
      (match (hash-ref previously-seen last-seen #false)
        [#false 0]
        [where-previously-seen (- (sub1 turn) where-previously-seen)])
      (hash-set! previously-seen last-seen (sub1 turn)))))

(check-equal? (solve "11,0,1,10,5,19" 2020) 870)
(check-equal? (solve "11,0,1,10,5,19" 30000000) 9136)