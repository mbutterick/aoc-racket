#lang br
(require racket/file rackunit racket/sequence)

(define (parse-input str)
  (filter-map string->number (string-split str #rx"=|,|\n|>|<")))

(struct $moon (pos vel) #:transparent #:mutable)

(define (str->moons str)
  (for/list ([trip (in-slice 3 (parse-input str))])
    ($moon trip (list 0 0 0))))

(define (update-pair-velocity! m0 m1)
  (define-values (new-m0-vel new-m1-vel)
    (for/lists (gm0 gm1)
               ([m0-pos-item (in-list ($moon-pos m0))]
                [m0-vel-item (in-list ($moon-vel m0))]
                [m1-pos-item (in-list ($moon-pos m1))]
                [m1-vel-item (in-list ($moon-vel m1))])
      (define-values (adj-m0 adj-m1)
        (cond
          [(< m0-pos-item m1-pos-item) (values 1 -1)]
          [(= m0-pos-item m1-pos-item) (values 0 0)]
          [else (values -1 1)]))
      (values (+ adj-m0 m0-vel-item) (+ adj-m1 m1-vel-item))))
  (set-$moon-vel! m0 new-m0-vel)
  (set-$moon-vel! m1 new-m1-vel))

(define (update-position! m)
  (set-$moon-pos! m (map + ($moon-pos m) ($moon-vel m))))

(define (update-gravity! moons)
  (let loop ([moons moons])
    (unless (= 1 (length moons))
      (match-define (cons m0 ms) moons)
      (for ([m (in-list ms)])
        (update-pair-velocity! m0 m))
      (loop ms))))

(define (step! moons [count 1])
  (for ([i (in-range count)])
    (update-gravity! moons)
    (for-each update-position! moons))
  moons)

(define (total-energy moons)
  (for/sum ([moon (in-list moons)])
    (apply * (for/list ([field-proc (list $moon-pos $moon-vel)])
               (for/sum ([val (in-list (field-proc moon))])
                 (abs val))))))

(check-eq?
 (total-energy (step! (str->moons "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>") 10))
 179)

(check-eq?
 (total-energy (step! (str->moons "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>") 100))
 1940)

;; 1
(check-eq?
 (total-energy (step! (str->moons (file->string "12.rktd")) 1000))
 9876)

;; 2
(define (shift-origin! moons pos)
  (for ([moon (in-list moons)])
    (set-$moon-pos! moon (map - ($moon-pos moon) pos))))
        
(define (period str)
  (for*/list ([which-moon (list first second third fourth)]
              [which-dim (list first second third)])
    (define moons (str->moons str))
    (shift-origin! moons ($moon-pos (which-moon moons)))
    (let loop ([count 1])
      (step! moons)
      (if (and (= 0 (which-dim ($moon-pos (which-moon moons))))
               (= 0 (which-dim ($moon-vel (which-moon moons)))))
          count
          (loop (add1 count))))))

(check-eq?
 (apply lcm (period "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"))
 2772)

(check-eq?
 (apply lcm
   (period "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>"))
   4686774924)



