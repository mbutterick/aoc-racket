#lang br
(require racket/file rackunit racket/sequence)

(define pixel-data (for/list ([c (in-string (file->string "08.rktd"))])
                     (string->number (string c))))
(define img-width 25)
(define img-height 6)

(define layers (for/list ([layer (in-slice (* img-width img-height) pixel-data)])
                 layer))

(define ((digit-count digit) layer)
  (for/sum ([x (in-list layer)]
            #:when (= x digit))
    1)) 

(define least-zero-layer (argmin (digit-count 0) layers))

;; 1
(check-eq?
 (* ((digit-count 1) least-zero-layer) ((digit-count 2) least-zero-layer))
 2286)

(define (sum-pixels . ps)
  (for/first ([p (in-list ps)]
              #:when (< p 2))
    p))

(define (layer->string layer)
  (string-join (for/list ([row (in-slice img-width layer)])
                 (string-join (for/list ([digit (in-list row)])
                                (if (zero? digit) " " "X")) "")) "\n"))

;; 2
(check-equal? (layer->string (apply map sum-pixels layers))
              " XX    XX XXXX X    XXX  \nX  X    X    X X    X  X \nX       X   X  X    X  X \nX       X  X   X    XXX  \nX  X X  X X    X    X    \n XX   XX  XXXX XXXX X    ")