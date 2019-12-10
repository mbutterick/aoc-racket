#lang br
(require racket/file rackunit racket/math )

(define (string->asteroids str)
  (for*/list ([(row ridx) (in-indexed (string-split str))]
              [(col cidx) (in-indexed row)]
              #:when (char=? #\# col))
    (+ cidx (* +i ridx))))

(define ((count-visible roids) roid-origin)
  (length (remove-duplicates (for/list ([roid (in-list roids)]
                                        #:unless (= roid-origin roid))
                               (angle (- roid roid-origin))))))

(define (find-best roids)
  (argmax (count-visible roids) roids))

(check-equal? (find-best (string->asteroids "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")) 5+8i)

(check-equal? (find-best (string->asteroids "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")) 1+2i)

(check-equal? (find-best (string->asteroids ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")) 6+3i)

(check-equal? (find-best (string->asteroids ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")) 11+13i)

;; 1
(define roids (string->asteroids (file->string "10.rktd")))
(define best-roid (find-best roids))
(check-eq? ((count-visible roids) best-roid) 214)

(define (radians roid)
  (match (+ pi (angle (* -i (- roid best-roid))))
    [(== (* pi 2)) 0]
    [res res]))

(check-= (radians 8) 0 0.1)
(check-= (radians 16+16i) (/ pi 2) 0.1)
(check-= (radians 8+24i) pi 0.1)
(check-= (radians +16i) (* 1.5 pi) 0.1)

;; 2
(define roids-by-dist (sort (remove best-roid roids) < #:key magnitude))
(define radian-groups (group-by radians roids-by-dist))
(define radian-groups-by-angle
  (sort radian-groups < #:key (compose1 radians car)))
(check-eq?
 (match (list-ref radian-groups-by-angle (sub1 200))
   [(cons c _) (+ (* 100 (real-part c)) (imag-part c))])
 502)