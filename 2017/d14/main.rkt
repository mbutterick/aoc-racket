#lang reader "../aoc-lang.rkt"
(require racket/sequence sugar/list)
(provide (rename-out [#%mb #%module-begin]))

(define-macro (#%mb (STARS) (STR) ...)
  #`(#%module-begin
     (time ((if (eq? 'STARS '★) one-star two-star) (format "~a" 'STR))) ...))

(define (knot-hashes str)
  (for/list ([i (in-range 128)])
    (knot-hash (format "~a-~a" str i))))

(define (one-star str)
  (for*/sum ([kh (in-list (knot-hashes str))]
             [int (in-list (kh->ints kh))])
    int))

(define (two-star str)
  (define vec (for*/vector ([kh (in-list (knot-hashes str))]
                            [int (in-list (kh->ints kh))])
                (if (= int 1) 'used 'empty)))
  (define (at-left-edge? idx) (= (modulo idx 128) 0))
  (define (at-right-edge? idx) (= (modulo idx 128) 127))
  (for/fold ([region 0])
            ([(val idx) (in-indexed vec)]
             #:unless (number? val))
    (let loop ([idx idx])
      (cond
        [(and (<= 0 idx (sub1 (vector-length vec))) (eq? (vector-ref vec idx) 'used))
         (vector-set! vec idx region)
         (unless (at-left-edge? idx) (loop (sub1 idx)))
         (unless (at-right-edge? idx) (loop (add1 idx)))
         (loop (+ idx 128))
         (loop (- idx 128))
         (add1 region)]
        [else region]))))

(define (kh->ints kh)
  (for*/list ([c (in-string kh)]
              [num (in-value (string->number (string c) 16))]
              [c (in-string (~r num #:base 2 #:min-width 4 #:pad-string "0"))])
    (if (char=? c #\1) 1 0)))

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
    (for ([val (in-list (map (λ (posn) (vector-ref vec posn)) posns))]
          [posn (in-list (reverse posns))])
      (vector-set! vec posn val))
    (values (+ current-position len skip-size) (add1 skip-size)))
  (vector->list vec))