#lang br
(require racket/file rackunit racket/set)

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

(struct tile (num edges css) #:transparent #:mutable)

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

(define tilestrs (make-hash))

(define tiles
  (let* ([recs (string-split (file->string "20.rktd") "Tile")]
         [recs (map string-trim recs)])
    (for/list ([rec recs])
      (match-define (list numstr tilestr) (string-split rec ":"))
      (define num (string->number numstr))
      (tile num (tilestr->edgevals tilestr) (map string->list (string-split tilestr "\n"))))))

(define (matching-tiles tile [tiles tiles])
  (for/list ([edge (in-list (tile-edges tile))])
    (for/first ([other-tile (in-list (remove tile tiles))]
                #:when
                (for/or ([other-edge (in-list (tile-edges other-tile))])
                  (or (eq? edge other-edge) (eq? edge (edge-flipped other-edge)))))
      other-tile)))

(define (matching-edge-count tile count)
  (= count (length (filter values (matching-tiles tile)))))
(define (edge-tile? tile) (matching-edge-count tile 3))
(define (corner-tile? tile) (matching-edge-count tile 2))

(define corners (filter corner-tile? tiles))
(check-equal? (apply * (map tile-num corners)) 15405893262491)

(define first-corner (car corners))
(define tile-grid (make-hasheqv))

(define (flip-css css) (map reverse css))
(define (flip t) (tile (tile-num t) (tile-edges t) (flip-css (tile-css t))))
(define (rotate-css css) (map reverse (apply map list css)))
(define (rotate t) (tile (tile-num t) (tile-edges t) (rotate-css (tile-css t))))

(define (top tile) (first (tile-css tile)))
(define (bottom tile) (last (tile-css tile)))
(define (left tile) (map first (tile-css tile)))
(define (right tile) (map last (tile-css tile)))

(define (joiner tile left-proc right-proc)
  (for*/first ([other-tile tiles]
               #:unless (eq? (tile-num tile) (tile-num other-tile))
               [flip-proc (list values flip)]
               [rotate-proc (list values rotate (compose1 rotate rotate) (compose1 rotate rotate rotate))]
               [adjusted-tile (in-value (rotate-proc (flip-proc other-tile)))]
               #:when (equal? (left-proc tile) (right-proc adjusted-tile)))
    adjusted-tile))

(define (insert-joiners! coord)
  (define tile (hash-ref tile-grid coord))
  (for ([edge-proc (list top left bottom right)]
        [opp-edge-proc (list bottom right top left)]
        [dir (list +i -1 -i 1)])
    (match (joiner tile edge-proc opp-edge-proc)
      [#false (void)]
      [joiner (hash-set! tile-grid (+ coord dir) joiner)])))

(hash-set! tile-grid 0 first-corner)
(for* ([imag (in-range 12)]
       [real (in-range 0 -12 -1)])
  (insert-joiners! (+ real (* imag +i))))

(define (trim-edge css)
  (drop-right (cdr (map (λ (cs) (drop-right (cdr cs) 1)) css)) 1))

(define actual-image
  (append*
   (for/list ([imag (in-range 11 -1 -1)])
     (apply map append
            (for/list ([real (in-range -11 1)])
              (trim-edge (tile-css (hash-ref tile-grid (+ real (* imag +i))))))))))

(define row-width (length (car actual-image)))
(define (css->vec css) (list->vector (apply append css)))

(define sea-monster-offsets (list 0
                                  (- row-width 18)
                                  (- row-width 13)
                                  (- row-width 12)
                                  (- row-width 7)
                                  (- row-width 6)
                                  (- row-width 1)
                                  row-width
                                  (+ row-width 1)
                                  (- (* 2 row-width) 2)
                                  (- (* 2 row-width) 5)
                                  (- (* 2 row-width) 8)
                                  (- (* 2 row-width) 11)
                                  (- (* 2 row-width) 14)
                                  (- (* 2 row-width) 17)))

(define (sea-monster-at? vec idx)
  (for/and ([offset (in-list sea-monster-offsets)])
    (define pos (+ idx offset))
    (and
     (< pos (vector-length vec))
     (char=? (vector-ref vec pos) #\#))))

(define vec (css->vec (rotate-css (rotate-css (rotate-css actual-image)))))
(define sea-monster-idxs
  (for/list ([idx (in-range (vector-length vec))]
             #:when (sea-monster-at? vec idx))
    idx))

(define sea-monster-parts
  (set->list
   (for*/set ([idx sea-monster-idxs]
              [offset sea-monster-offsets])
     (+ idx offset))))

(check-equal? (- (count (λ (c) (char=? #\# c)) (apply append actual-image)) (length sea-monster-parts)) 2133)
