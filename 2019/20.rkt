#lang br
(require racket/file rackunit racket/set graph)

(define str (file->string "20.rktd"))

(define (find-cells pred)
  (for*/hasheqv ([(row ridx) (in-indexed (string-split str "\n"))]
                 [(c cidx) (in-indexed row)]
                 #:when (pred c))
    (values (make-rectangular cidx ridx) c)))

(define passage-locs (find-cells (λ (c) (char=? c #\.))))
(define letter-locs (find-cells char-alphabetic?))

(define g (unweighted-graph/undirected null))

(for* ([loc (in-hash-keys passage-locs)]
       [delta (list 1 -1 +i -i)])
  (when (hash-has-key? passage-locs (+ loc delta))
    (add-edge! g loc (+ loc delta)))
  (when (hash-has-key? letter-locs (+ loc delta))
    (add-edge! g loc (string->symbol
                      (apply string ((if (member delta (list -1 -i))
                                         reverse
                                         values) (for/list ([d (list delta (* 2 delta))])
                                                   (hash-ref letter-locs (+ loc d)))))))))
         
(for ([v (in-vertices g)]
      #:when (and (symbol? v) (not (memq v '(AA ZZ)))))
  (apply add-edge! g (get-neighbors g v)))


;; 1
(check-eq? (- (length (fewest-vertices-path g 'AA 'ZZ)) 3) 638)