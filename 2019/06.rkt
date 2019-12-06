#lang br
(require racket/file rackunit graph)

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