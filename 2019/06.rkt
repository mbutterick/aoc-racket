#lang br
(require racket/file rackunit graph racket/dict)

(define (string->orbits str)
  (for/list ([ln (in-list (string-split str))])
    (map string->symbol (string-split ln ")"))))

(define orbit-recs (string->orbits (file->string "06.rktd")))

(define (count-orbits orbit-recs)
  (match/values (bfs (undirected-graph orbit-recs) 'COM)
                [(dists _) (apply + (hash-values dists))]))

;; 1
(check-eq? (count-orbits orbit-recs) 261306)

(define (count-transfers orbit-recs)
  (match/values (bellman-ford (undirected-graph orbit-recs) 'YOU)
                [(dists _) (- (hash-ref dists 'SAN) 2)]))

;; 2
(check-eq? (count-transfers orbit-recs) 382)


;; alternate solution, using only lists

(define orbit-dict (for/list ([rec (in-list orbit-recs)])
                     (cons (second rec) (first rec))))

(define orbit-chains
  ;; path from each object back to 'COM
  (for/list ([orbiter (in-dict-keys orbit-dict)])
    (let loop ([chain (list orbiter)])
      (match (assq (car chain) orbit-dict)
        [#false (reverse chain)]
        [(app cdr dest) (loop (cons dest chain))]))))

;; 1
;; orbit count is one less than the length of each chain
(check-eq? (apply + (map sub1 (map length orbit-chains))) 261306)

(define you-chain (assq 'YOU orbit-chains))
(define san-chain (assq 'SAN orbit-chains))
(define common-tail (take-common-prefix (reverse you-chain) (reverse san-chain)))

;; 2
;; answer path is two less than the length of path between YOU and SAN
(check-eq?
 (- (+ (- (length you-chain) (length common-tail))
       (- (length san-chain) (length common-tail)))
    2)
 382)