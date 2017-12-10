#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]))

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read (open-input-string (car lines)))])
                          datum)
                     #,@(cdr lines))))

(define-macro (#%mb STARS RANGE STR)
  #`(#%module-begin
     (set-knot-range! RANGE)
     ((if (eq? 'STARS '★) one-star two-star) STR)))

(define (one-star str)
  (for ([len (in-port read (open-input-string (string-replace str "," " ")))])
    (reverse-segment len))
  (* (vector-ref vec 0) (vector-ref vec 1)))

(define (two-star str)
  (define ascii-chars (map char->integer (string->list str)))
  (for* ([round (in-range 64)]
         [len (in-list (append ascii-chars '(17 31 73 47 23)))])
    (reverse-segment len))
  (define dense-hash (for/list ([i (in-range 16)])
                       (apply bitwise-xor (for/list ([v (in-vector vec (* i 16) (* (add1 i) 16))])
                                            v))))
  (string-append* (for/list ([num (in-list dense-hash)])
                         (~r num #:base 16 #:min-width 2 #:pad-string "0"))))

(define vec #f)
(define current-position (make-parameter 0))
(define skip-size (make-parameter 0))
(define (set-knot-range! int) (set! vec (list->vector (range int))))

(define (reverse-segment len)
  (define posns (for/list ([i (in-range len)])
                  (modulo (+ (current-position) i) (vector-length vec))))
  (for ([val (in-list (map (curry vector-ref vec) posns))]
        [posn (in-list (reverse posns))])
    (vector-set! vec posn val))
  (current-position (+ (current-position) len (skip-size)))
  (skip-size (add1 (skip-size))))