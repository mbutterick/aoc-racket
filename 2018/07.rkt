#lang debug br
(require graph)

#|
Track the progress through the steps with a dag,
but track the state of the prerequisites with another dag (in the reverse direction)
|#

(define dag (directed-graph null))
(define prereqs (weighted-graph/directed null))
(for ([ln (in-lines (open-input-file "07.txt"))])
  (match-define (list left right) (regexp-match* #rx"(?<=[Ss]tep )." ln))
  (add-directed-edge! dag left right)
  (add-directed-edge! prereqs right left +inf.0))

(define (activate-prereq! v1 v2)
  ;; a prereq is "met" if the edge weight is zero
  (add-directed-edge! prereqs v2 v1 0))

(define (prereqs-met? v)
  ;; check if all v's edges in the prereq graph are zero
  (andmap zero? (map (λ (n) (edge-weight prereqs v n)) (get-neighbors prereqs v))))

(define (find-available g)
  (filter prereqs-met? (get-vertices g)))

(define (★)
  (let loop ([vs-available (find-available prereqs)] [visited null])
    (cond
      [(= (length visited) (length (get-vertices dag)))
       (apply string-append (reverse visited))]
      [else
       (match (sort vs-available string<?)
         [(list this-v others ...)
          (for-each (λ (n) (activate-prereq! this-v n)) (get-neighbors dag this-v))
          (define this-visited (cons this-v visited))
          (loop (filter-not (λ (v) (member v this-visited)) (find-available prereqs))
                this-visited)])])))

#;(define (★★)
    )
#;(★★)

(module+ test
  (require rackunit)
  (check-equal? (time (★)) "GNJOCHKSWTFMXLYDZABIREPVUQ")
  #;(check-equal? (time (★★)) 566))