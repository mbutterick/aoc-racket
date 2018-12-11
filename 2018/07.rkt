#lang debug br
(require graph)

#|
Track the progress through the steps with a dag,
but track the state of the prerequisites with another dag (in the reverse direction)
|#

(define dag (directed-graph null))
(define prereqs (weighted-graph/directed null))

(define (init-graphs!)
  (for ([ln (in-lines (open-input-file "07.txt"))])
    (match-define (list (list left) (list right))
      (map string->list (regexp-match* #rx"(?<=[Ss]tep )." ln)))
    (add-directed-edge! dag left right)
    (add-directed-edge! prereqs right left +inf.0)))

(define (meet-prereq! v1 v2)
  ;; a prereq is "met" if the edge weight is zero
  (add-directed-edge! prereqs v2 v1 0))

(define (prereqs-met? v)
  ;; check if all v's edges in the prereq graph are zero
  (zero? (for/sum ([n (in-list (get-neighbors prereqs v))])
           (edge-weight prereqs v n))))

(define (find-available g)
  (filter prereqs-met? (get-vertices g)))

(define (★)
  (init-graphs!)
  (let loop ([vs-available (find-available prereqs)] [done null])
    (cond
      [(= (length done) (length (get-vertices dag))) (list->string (reverse done))]
      [else
       (match-define (list this-v others ...) (sort vs-available char<?))
       (for ([n (in-list (get-neighbors dag this-v))])
         (meet-prereq! this-v n))
       (define this-visited (cons this-v done))
       (loop (filter-not (λ (v) (memv v this-visited)) (find-available prereqs))
             this-visited)])))

(define (worker-done? worker)
  (match-define (cons char time) worker)
  (= time (+ 60 (- (char->integer char) 65))))

(define (★★)
  (init-graphs!)
  (define worker-count 5)
  (let loop ([workers null][done null][steps 0])
    (cond
      [(= (length done) (length (get-vertices dag))) (sub1 steps)]
      [else
       (define-values (done-ws working-ws) (partition worker-done? workers))
       (define done-vs (map car done-ws))
       (for* ([v (in-list done-vs)]
              [n (in-list (get-neighbors dag v))])
         (meet-prereq! v n))
       (define next-done (append done-vs done))
       (define updated-ws (for/list ([w (in-list working-ws)])
                            (match-define (cons v time) w)
                            (cons v (add1 time))))
       (define vs-available (for/list ([v (in-list (find-available prereqs))]
                                       #:unless (memv v (append next-done (map car updated-ws))))
                              v))
       (define new-vs (take (sort vs-available char<?)
                            (min (length vs-available) (- worker-count (length working-ws)))))
       (define new-workers (map (λ (v) (cons v 0)) new-vs))
       (loop (append new-workers updated-ws) next-done (add1 steps))])))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) "GNJOCHKSWTFMXLYDZABIREPVUQ")
  (check-equal? (time (★★)) 886))