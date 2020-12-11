#lang br
(require racket/file rackunit)

(define grid
  (for*/hasheqv ([(row ridx) (in-indexed (file->lines "11.rktd"))]
                 [(col cidx) (in-indexed row)])
    (values (make-rectangular cidx ridx) col)))

(define (seat-count grid)
  (count (λ (val) (char=? val #\#)) (hash-values grid)))

(define offsets '(-1-1i -1 -1+i +i 1+i 1 1-i -i))

(define (visible-occupied-seats grid k [adjacent-only? #false])
  (for/sum ([offset (in-list offsets)])
    (let loop ([k (+ k offset)] [sum 0])
      (match (hash-ref grid k #false)
        [#false sum]
        [#\# 1]
        [#\L 0]
        [_ (if adjacent-only?
               sum
               (loop (+ k offset) sum))]))))

(define (adjacent-occupied-seats grid k) (visible-occupied-seats grid k #true))

(define (iterate grid seat-counter threshold)
  (for/hasheqv ([(k v) (in-hash grid)])
    (match v
      [#\L #:when (zero? (seat-counter grid k)) (values k #\#)]
      [#\# #:when (<= threshold (seat-counter grid k)) (values k #\L)]
      [_ (values k v)])))

(define (solve grid iteration-proc)
  (let loop ([grid grid])
    (define next-grid (iteration-proc grid))
    (if (eqv? (seat-count grid) (seat-count next-grid))
        (seat-count grid)
        (solve next-grid iteration-proc))))

(check-equal? (solve grid (λ (grid) (iterate grid adjacent-occupied-seats 4))) 2489)

(check-equal? (solve grid (λ (grid) (iterate grid visible-occupied-seats 5))) 2180)