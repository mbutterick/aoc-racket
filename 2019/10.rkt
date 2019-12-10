#lang br
(require racket/file rackunit)

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
(check-eq?
 ((count-visible roids) (find-best roids))
 214)
