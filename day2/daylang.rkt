#lang racket
(require rackunit)
(provide read-syntax)

(define (get-dimensions str)
  (map string->number (regexp-match* #px"\\d+" str)))

(define (get-faces dimensions)
  (define half-faces
    (for*/list ([first-pos (in-range (sub1 (length dimensions)))]
                            [second-pos (in-range (add1 first-pos) (length dimensions))])
                           (list (list-ref dimensions first-pos) (list-ref dimensions second-pos))))
  (append* (make-list 2 half-faces)))

(define (str->paper str)
  (define dimensions (get-dimensions str))
  (define faces (get-faces dimensions))
  (define areas (map (curry apply *) faces))
  (define smallest-area (apply min areas))
  (apply + smallest-area areas))

(define (str->ribbon str)
  (define dimensions (get-dimensions str))
  (define faces (get-faces dimensions))
  (define perimeters (map (λ(face) (* 2 (apply + face))) faces))
  (define smallest-perimeter (apply min perimeters))
  (define volume (apply * dimensions))
  (+ smallest-perimeter volume))
                 
(check-equal? (str->paper "2x3x4") 58)
(check-equal? (str->paper "1x1x10") 43)

(check-equal? (str->ribbon "2x3x4") 34)
(check-equal? (str->ribbon "1x1x10") 14)

(define (read-syntax source-path-string in-port)
  (with-syntax ([source-str (string-trim (port->string in-port))]
                [str->paper str->paper]
                [str->ribbon str->ribbon])
    #'(module _ racket
        (define package-strs (string-split 'source-str))
        (apply + (map str->paper package-strs))
        (apply + (map str->ribbon package-strs)))))