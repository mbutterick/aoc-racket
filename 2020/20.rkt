#lang br
(require racket/file rackunit)

(define edge-flipped
  (let ()
    (define cache (make-hasheq))
    (define bit-count 10)
    (define (edge-flipper x)
      (for/sum ([(b i)
                 (in-indexed (for/list ([b (in-range (sub1 bit-count) -1 -1)])
                               (bitwise-bit-set? x b)))]
                #:when b)
        (arithmetic-shift 1 i)))
    (λ (x) (hash-ref! cache x (λ () (edge-flipper x))))))

(struct tile (num edges) #:transparent)

(define (tilestr->edgevals tilestr)
  (define vec (list->vector (string->list (string-replace tilestr "\n" ""))))
  (for/list ([validxs (list (range 10) ; top
                            (range 9 100 10) ; right
                            (range 99 89 -1) ; bottom
                            (range 90 -1 -10) ; left
                            )])
    (for/sum ([idx (in-list validxs)]
              [i (in-naturals)]
              #:when (char=? (vector-ref vec idx) #\#))
      (arithmetic-shift 1 i))))

(define tiles
  (let* ([recs (string-split (file->string "20.rktd") "Tile")]
         [recs (map string-trim recs)])
    (for/list ([rec recs])
      (match-define (list numstr tilestr) (string-split rec ":"))
      (tile (string->number numstr)
            (tilestr->edgevals tilestr)))))

(define (matching-edges tile)
  (for/list ([edge (in-list (tile-edges tile))])
    (for/first ([other-tile (in-list (remove tile tiles))]
                #:when
                (for/or ([other-edge (in-list (tile-edges other-tile))])
                  (or (eq? edge other-edge) (eq? edge (edge-flipped other-edge)))))
      other-tile)))

(define (corner-tile? tile)
  (= 2 (length (filter values (matching-edges tile)))))

(check-equal? (apply * (map tile-num (filter corner-tile? tiles))) 15405893262491)
