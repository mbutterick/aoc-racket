#lang br/quicklang
(require "../helper.rkt" racket/sequence)
(provide read-syntax (rename-out [#%mb #%module-begin]))

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read (open-input-string (car lines)))])
                                  datum)
                     #,@(cdr lines))))

(define-macro (#%mb STARS RANGE-IN STR)
  #`(#%module-begin
     (time ((if (eq? 'STARS '★) one-star two-star) RANGE-IN STR))))

(define (one-star range-in str)
  (define lens (with-input-from-string (string-replace str "," " ")
                                       (λ () (for/list ([len (in-port)])
                                                       len))))
  (define nums (reverse-segments range-in lens))
  (* (first nums) (second nums)))

(define (two-star range-in str)
  (define ascii-chars (map char->integer (string->list str)))
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