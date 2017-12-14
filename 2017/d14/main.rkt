#lang reader "../aoc-lang.rkt"
(require racket/sequence)
(provide (rename-out [#%mb #%module-begin]))

(define-macro (#%mb (STARS) (STR) ...)
  #`(#%module-begin
     ((if (eq? 'STARS '★) one-star (λ (x) #f)) (format "~a" 'STR)) ...))

(define (one-star str)
  (for*/sum ([i (in-range 128)]
             [digit (in-list (kh->ints (knot-hash (format "~a-~a" str i))))])
    digit))

(define (kh->ints kh)
  (for*/list ([c (in-string kh)]
              [num (in-value (string->number (string c) 16))]
              [c (in-string (~r num #:base 2 #:min-width 4 #:pad-string "0"))])
    (string->number (string c))))

(define (knot-hash seed-str [range-in 256])
  (define ascii-chars (map char->integer (string->list seed-str)))
  (define nums (reverse-segments range-in (append ascii-chars '(17 31 73 47 23)) #:reps 64))
  (define dense-hash (for/list ([vals (in-slice 16 nums)])
                       (apply bitwise-xor vals)))
  (string-append* (for/list ([num (in-list dense-hash)])
                    (~r num #:base 16 #:min-width 2 #:pad-string "0"))))

(define (reverse-segments range-in lens #:reps [reps 1])
  (define vec (list->vector (range range-in)))
  (for*/fold ([current-position 0]
              [skip-size 0])
             ([rep (in-range reps)]
              [len (in-list lens)])
    (define posns (for/list ([i (in-range len)])
                    (modulo (+ current-position i) range-in)))
    (for ([val (in-list (map (curry vector-ref vec) posns))]
          [posn (in-list (reverse posns))])
      (vector-set! vec posn val))
    (values (+ current-position len skip-size) (add1 skip-size)))
  (vector->list vec))