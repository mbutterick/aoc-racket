#lang br
(require racket/file rackunit racket/set racket/dict)

(define (line->wire ln)
  ;; convert string to a wire
  (for/list ([tok (in-list (string-split ln ","))])
    (match (regexp-match #px"^(\\w)(\\d+)$" tok)
      [(list _ dir numstr)
       (cons (string->symbol dir) (string->number numstr))])))

(define (wire->locs wire)
  ;; list of every grid location visited by a wire
  ;; use complex numbers to model x, y locations
  (for*/fold ([locs (list 0)]
              #:result (reverse locs))
             ([(dir dist) (in-dict wire)]
              [complex-unit (in-value (match dir
                                       ['L -1]
                                       ['R 1]
                                       ['U +i]
                                       ['D -i]))]
              [i (in-range dist)])
    (cons (+ (car locs) complex-unit) locs))) 

(define test-w1 (line->wire "R8,U5,L5,D3"))
(check-equal?
 (wire->locs test-w1)
 '(0 1 2 3 4 5 6 7 8 8+1i 8+2i 8+3i 8+4i 8+5i 7+5i 6+5i 5+5i 4+5i 3+5i 3+4i 3+3i 3+2i))

(define test-w2 (line->wire "U7,R6,D4,L4"))
(check-equal?
 (wire->locs test-w2)
 '(0 0+1i 0+2i 0+3i 0+4i 0+5i 0+6i 0+7i 1+7i 2+7i 3+7i 4+7i 5+7i 6+7i 6+6i 6+5i 6+4i 6+3i 5+3i 4+3i 3+3i 2+3i))

(define (find-intersections w1 w2)
  ;; intersections are any common points, except 0
  (filter-not zero?
              (set->list
               (set-intersect (list->set (wire->locs w1))
                              (list->set (wire->locs w2))))))

(define (manhattan-dist num)
  (+ (abs (real-part num)) (abs (imag-part num)))) 

(define (closest-intersection w1 w2)
  (apply min (map manhattan-dist (find-intersections w1 w2))))

(check-eq?
 (closest-intersection
  (line->wire "R8,U5,L5,D3")
  (line->wire "U7,R6,D4,L4"))
 6)

(check-eq?
 (closest-intersection
  (line->wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
  (line->wire "U62,R66,U55,R34,D71,R55,D58,R83"))
 159)

(check-eq?
 (closest-intersection
  (line->wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
  (line->wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
 135)

;; 1
(define wires (map line->wire (file->lines "03.rktd")))
(check-eq?
 (apply closest-intersection wires)
 227)

(define (earliest-intersection w1 w2)
  (apply min
         (for/list ([int (in-list (find-intersections w1 w2))])
           (+ (index-of (wire->locs w1) int)
              (index-of (wire->locs w2) int)))))

(check-eq?
 (earliest-intersection
  (line->wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
  (line->wire "U62,R66,U55,R34,D71,R55,D58,R83"))
 610)

(check-eq?
 (earliest-intersection
  (line->wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
  (line->wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
 410)

;; 2
(check-eq?
 (apply earliest-intersection wires)
 20286)
