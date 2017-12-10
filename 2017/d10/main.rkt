#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]))

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read (open-input-string (car lines)))])
                          datum)
                     #,@(cdr lines))))

(define vec #f)
(define (set-knot-range! int) (set! vec (list->vector (range int))))

(define-macro (#%mb STARS RANGE STR)
  #`(#%module-begin
     (set-knot-range! RANGE)
     ((if (eq? 'STARS '★) one-star two-star) STR)))

(define (one-star str)
  (define lens (for/list ([len (in-port read (open-input-string (string-replace str "," " ")))])
                 len))
  (reverse-segments lens)
  (* (vector-ref vec 0) (vector-ref vec 1)))

(define (two-star str)
  (define ascii-chars (map char->integer (string->list str)))
  (reverse-segments (append ascii-chars '(17 31 73 47 23)) #:count 64)
  (define dense-hash (for/list ([i (in-range 16)])
                       (apply bitwise-xor (for/list ([v (in-vector vec (* i 16) (* (add1 i) 16))])
                                            v))))
  (string-append* (for/list ([num (in-list dense-hash)])
                    (~r num #:base 16 #:min-width 2 #:pad-string "0"))))

(define (reverse-segments lens #:count [count 1])
  (for*/fold ([current-position 0]
              [skip-size 0])
             ([i (in-range count)]
              [len (in-list lens)])
    (define posns (for/list ([i (in-range len)])
                    (modulo (+ current-position i) (vector-length vec))))
    (for ([val (in-list (map (curry vector-ref vec) posns))]
          [posn (in-list (reverse posns))])
      (vector-set! vec posn val))
    (values (+ current-position len skip-size) (add1 skip-size))))