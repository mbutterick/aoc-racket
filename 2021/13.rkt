#lang br
(require racket/file sugar rackunit racket/set)

(define-values (point-strs fold-strs) (splitf-at (file->lines "13.rktd") non-empty-string?))

(define points
  (for/seteqv ([str point-strs])
    (apply make-rectangular (map string->number (string-split (string-replace str "," " "))))))

(define creases
  (for/list ([str fold-strs]
             #:when (non-empty-string? str))
    (match (string-split str "=")
      [(list "fold along y" numstr) (* +i (string->number numstr))]
      [(list "fold along x" numstr) (string->number numstr)])))

(define (do-crease points [stop-at +inf.0])
  (for/fold ([points points])
            ([crease creases]
             [i (in-range stop-at)])
    (for/seteqv ([pt points])
      (define imag-diff (- (imag-part pt) (imag-part crease)))
      (define real-diff (- (real-part pt) (real-part crease)))
      (cond
        [(and (zero? (real-part crease)) (positive? imag-diff))
         (+ (real-part pt) (* +i (- (imag-part crease) imag-diff)))]
        [(and (zero? (imag-part crease)) (positive? real-diff))
         (+ (- (real-part crease) real-diff) (* +i (imag-part pt)))]
        [else pt]))))

(check-equal? (set-count (do-crease points 1)) 706)

(define matrix (do-crease points))
(for-each displayln
          (map string-join
               (for/list ([row (add1 (apply max (map imag-part (set->list matrix))))])
                 (for/list ([col (add1 (apply max (map real-part (set->list matrix))))])
                   (if (set-member? matrix (make-rectangular col row)) "X" " ")))))
;; draws word LRFJBJEH

