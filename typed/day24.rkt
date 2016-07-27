#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(: groups (-> (Listof Integer) Integer Integer (Listof (Listof Integer))))
(define (groups packages len goal-weight)
  (cond
    [(= len 0) empty]
    [(= len 1) (map (ann list (-> Integer (Listof Integer))) (filter (ann (curry = goal-weight) (-> Integer Boolean)) packages))] ;;bg OMG
    [else
     (append*
      (for/list ([x (in-list packages)])
                : (Listof (Listof (Listof Integer)))
                (define later-packages (cdr (or (member x packages) (error 'bg))))
                (append-map (λ([ss : (Listof Integer)]) (define new-group (cons x ss))
                              (if (= goal-weight (weight new-group))
                                  (list new-group)
                                  empty))
                            (groups later-packages (sub1 len) (- goal-weight x)))))]))

(: weight (-> (Listof Integer) Integer))
(define (weight group) (apply + group))

(: quantum-entanglement (-> (Listof Integer) Integer))
(define (quantum-entanglement group) (apply * group))

(: remove-group (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define (remove-group group packages)
  (filter (λ([p : Integer]) (not (member p group))) packages))

(: has-solution? (-> (Listof Integer) (Listof Integer) Boolean))
(define (has-solution? group packages)
  (define target-weight (weight group))
  (define remaining-packages (remove-group group packages))
  (for/or : Boolean ([len (in-range (length remaining-packages))]
              #:when (not (empty?
                           (groups remaining-packages len target-weight))))
             #t))

(: find-three-group-solution (-> (Listof Integer) Integer (U #f Integer)))
(define (find-three-group-solution all-packages target-weight)
  (for/or : (U #f Integer) ([len (in-range (length all-packages))]) ;;bg cannot do for*/or
    (let loop : (U #f Integer) (
               [groups ;in-list
                        ((inst sort (Listof Integer) Integer)
                         (groups all-packages len target-weight)
                         #:key quantum-entanglement <)])
      (cond
       [(null? groups)
        #f]
       [(has-solution? (car groups) all-packages)
        (quantum-entanglement (car groups))]
       [else
        (loop (cdr groups))]))))
      ;         #:when (has-solution? group all-packages))
      ;        (quantum-entanglement group)))

(: q1 (-> String (U #f Integer)))
(define (q1 input-str)
  (define all-packages (map string->integer (string-split input-str)))
  (define target-weight (cast (/ (weight all-packages) 3) Integer))
  (find-three-group-solution all-packages target-weight))

;;bg
(: string->integer (-> String Integer))
(define (string->integer s)
  (cast (string->number s) Integer))

(: q2 (-> String (U #f Integer)))
(define (q2 input-str)
  (define all-packages (map string->integer (string-split input-str)))
  (define target-weight (cast (/ (weight all-packages) 4) Integer))
  (for/or : (U #f Integer) ([len (in-range (length all-packages))]) ;;bg cannot do for*/or
    (let loop : (U #f Integer) (
               [groups ((inst sort (Listof Integer) Integer)
                                 (groups all-packages len target-weight)
                                 #:key quantum-entanglement <)])
      (cond
       [(null? groups)
        #f]
       [(find-three-group-solution
                       (remove-group (car groups) all-packages) target-weight)
        (quantum-entanglement (car groups))]
       [else
        (loop (cdr groups))]))))

(module+ test
  (define input-str (file->string "../day24-input.txt"))
  (check-equal? (q1 input-str) 10439961859)
  (check-equal? (q2 input-str) 72050269))


