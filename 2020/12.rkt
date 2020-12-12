#lang br
(require racket/file rackunit racket/dict)

(define insts
  (for/list ([ln (file->lines "12.rktd")])
    (match-define (list _ name dist) (regexp-match  #px"(\\D+)(\\d+)" ln))
    (cons (string->symbol name) (string->number dist))))

(define left-turn +i)
(define right-turn -i)
(define north +i)
(define east (* north right-turn))
(define south (* east right-turn))
(define west (* south right-turn))

(define (manhattan-dist pos) (+ (abs (real-part pos)) (abs (imag-part pos))))

(define (solve insts [use-waypoint? #false])
  (for/fold ([pos 0]
             [facing 1]
             [waypoint 10+i]
             #:result (manhattan-dist pos))
            ([(name dist) (in-dict insts)])
    (case name
      [(F)
       (define ref-point (if use-waypoint? waypoint facing))
       (values (+ pos (* ref-point dist)) facing waypoint)]
      [(N E S W)
       (define direction (match name
                           ['S south]
                           ['N north]
                           ['E east]
                           ['W west]))
       (define next-pos (if use-waypoint? pos (+ pos (* direction dist))))
       (values next-pos facing (+ waypoint (* direction dist)))]
      [(L R)
       (define rotation
         (expt (if (eq? name 'L) left-turn right-turn) (/ dist 90)))
       (values pos (* facing rotation) (* waypoint rotation))])))

(check-equal? (solve insts) 1496)
(check-equal? (solve insts #t) 63843)