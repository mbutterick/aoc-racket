#lang br
(require racket/file graph rackunit)

(define lines (file->lines "15.rktd"))

(define grid (for*/list ([(line ridx) (in-indexed lines)]
                         [(num cidx) (in-indexed (map string->number (regexp-match* #rx"." line)))])
               (cons (make-rectangular cidx ridx) num)))

(define data (make-hash grid))

(define (row-length data) (add1 (apply max (map imag-part (hash-keys data)))))
(define (col-length data) (add1 (apply max (map real-part (hash-keys data)))))

(define (make-grid-graph data)
  (define rows (row-length data))
  (define cols (col-length data))
  (define g (weighted-graph/directed null))
  (for ([k (in-hash-keys data)])
    (add-vertex! g k))
  (define (edges-between pt0 pt1)
    (add-directed-edge! g pt0 pt1 (hash-ref data pt1))
    (add-directed-edge! g pt1 pt0 (hash-ref data pt0)))
  (for ([v (in-vertices g)])
    (unless (= (real-part v) (sub1 cols))
      (edges-between v (add1 v)))
    (unless (= (imag-part v) (sub1 rows))
      (edges-between v (+ +i v))))
  g)

(define (preds->path preds dest)
  (for/fold ([path (list dest)])
            ([i (in-naturals)]
             #:break (zero? (car path)))
    (cons (hash-ref preds (car path)) path)))

(define (solve data)
  (define g (make-grid-graph data))
  (define-values (paths preds) (dijkstra g 0))
  (define dest (make-rectangular (sub1 (col-length data)) (sub1 (row-length data))))
  (hash-ref paths dest))
(check-equal? (solve data) 487)

(define big-data (make-hash))
(define rows (row-length data))
(define cols (col-length data))

(for* ([(k v) (in-hash data)]
       [rowrepeat 5]
       [colrepeat 5])
  (define next-val (+ v colrepeat rowrepeat))
  (hash-set! big-data (+ k (make-rectangular (+ (* colrepeat cols)) (+ (* rowrepeat rows))))
             (cond
                 [(> next-val 9) (- next-val 9)]
                 [else next-val])))

;; graph library is too slow to solve big grid
;; needs dynamic programming
#;(solve big-data)