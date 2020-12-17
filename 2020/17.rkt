#lang br
(require racket/file rackunit)

(define grid
  (for*/hash ([(row ridx) (in-indexed (file->lines "17.rktd"))]
              [(col cidx) (in-indexed row)])
    (values (list 0 (make-rectangular cidx ridx)) col)))

(define (seat-count grid)
  (count (λ (val) (char=? val #\#)) (hash-values grid)))

(define offsets-3d (for*/list ([z '(1 0 -1)]
                               [xy '(-1-1i -1 -1+i +i 1+i 1 1-i -i 0)]
                               #:unless (and (zero? z) (zero? xy)))
                     (list z xy)))

(define offsets-4d
  (for*/list ([offset (cons '(0 0) offsets-3d)]
              [zadj '(+i 0 -i)]
              [nextval (in-value
                        (list (+ zadj (first offset)) (second offset)))]
              #:unless (equal? nextval '(0 0)))
    nextval))

(define (adjacent-occupied-seats grid offsets k)
  (for/sum ([offset (in-list offsets)])
    (let loop ([k (map + k offset)] [sum 0])
      (match (hash-ref grid k #false)
        [#\# 1]
        [_ 0]))))

(define (grow grid offsets)
  (define newgrid (make-hash))
  (for* ([k (in-hash-keys grid)]
         [offset (cons '(0 0) offsets)])
    (define newk (map + k offset))
    (hash-ref! newgrid newk (hash-ref grid newk #\.)))
  newgrid)

(define (iterate grid offsets)
  (for/hash ([(k v) (in-hash (grow grid offsets))])
    (match v
      [#\# (values k (if (<= 2 (adjacent-occupied-seats grid offsets k) 3) #\# #\.))]
      [_ (values k (if (= (adjacent-occupied-seats grid offsets k) 3) #\# #\.))])))

(define (count-active grid)
  (count (λ (val) (char=? val #\#)) (hash-values grid)))

(define (solve offsets)
  (for/fold ([grid grid]
             #:result (count-active grid))
            ([i 6])
    (iterate grid offsets)))

(check-equal? (solve offsets-3d) 395)

;; warning: slow
#;(check-equal? (solve offsets-4d) 2296)