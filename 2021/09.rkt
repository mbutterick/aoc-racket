#lang br
(require racket/file rackunit)

(define depths
  (for*/hasheqv ([(line lidx) (in-indexed (file->lines "09.rktd"))]
                 [(depth cidx) (in-indexed line)])
    (values (make-rectangular cidx lidx) (string->number (string depth)))))

(define (neighbors loc)
  (for*/list ([delta '(1 -1)]
              [ifactor '(1 +i)])
    (+ loc (* delta ifactor))))

(define (low? loc)
  (< (hash-ref depths loc)
     (apply min (filter-map (λ (li) (hash-ref depths li #f)) (neighbors loc)))))

(define low-locs (for/list ([(loc depth) (in-hash depths)]
                            #:when (low? loc))
                   loc))

(check-equal? (apply + (map (λ (lp) (add1 (hash-ref depths lp))) low-locs)) 539)

(define (higher-adjacent loc)
  (for/list ([neighbor (neighbors loc)]
             #:when (and
                     (hash-has-key? depths neighbor)
                     (not (= (hash-ref depths neighbor) 9))
                     (< (hash-ref depths loc) (hash-ref depths neighbor))))
    neighbor))

(define (basin-at loc)
  (remove-duplicates (cons loc (append-map basin-at (higher-adjacent loc)))))

(define basin-sizes (map length (map basin-at low-locs)))
(check-equal? (apply * (take (sort basin-sizes >) 3)) 736920)