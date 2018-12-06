#lang debug br

#|
Start with the area of the grid bounded by the locations.
Calculate the neighborhoods within this grid.
Any location that has a grid neighborhood that includes an edge piece
will grow to infinity (because adjacent pieces outside the grid will also be closest to that location)
So ignore those. And search the others for the answer.
|#

(define locs (for/list ([ln (in-lines (open-input-file "06.txt"))])
               (match (map string->number (string-split ln ", "))
                 [(list x y) (cons x y)])))

(define (bounding-box locs)
  (match locs
    [(list (cons xs ys) ...)
     (list (cons (apply min xs) (apply max ys))
           (cons (apply max xs) (apply min ys)))]))

(define bbox (bounding-box locs))

(define (manhattan-dist loca locb)
  (match (list loca locb)
    [(list (cons xa ya) (cons xb yb))
     (+ (abs (- xa xb)) (abs (- ya yb)))]))

(define (closest-to pt locs)
  (match (sort (map (λ (loc) (cons (manhattan-dist pt loc) loc)) locs) < #:key car)
    [(list* (cons score _) (cons score2 _) _) #:when (= score score2) #false] ; because tie 
    [(cons (cons _ loc) _) loc]))

(define (finite-neighborhoods locs)
  (define closest-table (make-hash))
  (match-define (list (cons xmin ymax) (cons xmax ymin)) bbox)
  (for* ([x (in-range xmin (add1 xmax))]
         [y (in-range ymin (add1 ymax))])
    (define pt (cons x y))
    (define loc (closest-to pt locs))
    (when loc
      (if (or (= x xmin) (= x xmax) (= y ymin) (= y ymax)) ; on edge, therefore infinite
          (hash-set! closest-table loc #f) ; invalidate loc
          (hash-update! closest-table loc (λ (val) (and val (cons pt val))) null))))
  (filter values (hash-values closest-table)))

(define (★)
  (apply max (map length (finite-neighborhoods locs))))

(define (region locs target-dist)
  (match-define (list (cons xmin ymax) (cons xmax ymin)) bbox)
  (for*/list ([x (in-range xmin (add1 xmax))]
              [y (in-range ymin (add1 ymax))]
              [pt (in-value (cons x y))]
              [total-dist (in-value (for/sum ([loc (in-list locs)])
                                      (manhattan-dist pt loc)))]
              #:when (< total-dist target-dist))
    pt))

(define (★★)
  (length (region locs 10000)))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 6047)
  (check-equal? (time (★★)) 46320))