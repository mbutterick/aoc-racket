#lang br
(require racket/file rackunit)

(define (make-octopodes)
  (define octopodes (make-hasheqv))
  (for* ([(line lidx) (in-indexed (file->lines "11.rktd"))]
         [(depth cidx) (in-indexed line)])
    (hash-set! octopodes (make-rectangular cidx lidx) (string->number (string depth))))
  octopodes)

(define octopodes (make-octopodes))

(define (neighbors loc)
  (remove loc
          (for*/list ([rd '(-1 0 1)]
                      [id '(-1 0 1)])
            (+ loc (make-rectangular rd id)))))

(define (increment! loc)
  (cond
    [(number? (hash-ref octopodes loc #false))
     (define next-val
       (match (modulo (add1 (hash-ref octopodes loc)) 10)
         [0 'flashed]
         [val val]))
     (hash-set! octopodes loc next-val)
     (when (eq? next-val 'flashed)
       (for-each increment! (neighbors loc)))]))

(define (find-flashes-and-reset)
  (for/list ([(loc val) (in-hash octopodes)]
             #:when (eq? val 'flashed))
    (hash-set! octopodes loc 0)
    loc))

(define (part-1)
  (for/fold ([flashes null]
             #:result (length flashes))
            ([step (in-range 100)])
    (for-each increment! (hash-keys octopodes))
    (append (find-flashes-and-reset) flashes)))

(check-equal? (part-1) 1705)

(set! octopodes (make-octopodes))

(define (part-2)
  (for/or ([step (in-naturals)])
    (for-each increment! (hash-keys octopodes))
    (and (= (length (find-flashes-and-reset)) (length (hash-keys octopodes))) (add1 step))))

(check-equal? (part-2) 265)
