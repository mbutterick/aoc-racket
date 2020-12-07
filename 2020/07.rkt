#lang br
(require racket/file graph rackunit)

(struct $bag (weight name))

(define parsed-baglists
  (for/list ([ln (file->lines "07.rktd")])
    (for/list ([str (string-split ln #px" bags?[,.]?")])
      (define pcs (string-split str))
      ($bag
       (and (>= (length pcs) 3) (string->number (car (take-right pcs 3))))
       (string-join (take-right pcs 2))))))

(define g1 (unweighted-graph/directed
            (for*/list ([baglist parsed-baglists]
                        [bag (cdr baglist)])
              (list ($bag-name bag) ($bag-name (car baglist))))))

(define-values (distances _) (dijkstra g1 "shiny gold"))

(check-equal? (count exact-positive-integer? (hash-values distances)) 222)

(define g2 (weighted-graph/directed
            (for*/list ([baglist parsed-baglists]
                        [bag (cdr baglist)])
              (match-define ($bag weight name) bag)
              (list weight ($bag-name (car baglist)) name))))

(define (bag-count name)
  (+ 1 (for*/sum ([name2 (get-neighbors g2 name)]
                  [weight (in-value (edge-weight g2 name name2))]
                  #:when (exact-positive-integer? weight))
         (* weight (bag-count name2)))))

(check-equal? (sub1 (bag-count "shiny gold")) 13264)


