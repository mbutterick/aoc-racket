#lang info
(define collection "aoc-racket")
(define scribblings '(("aoc-racket.scrbl" (multi-page))))
(define deps '("base" "scribble-lib" "sugar" "rackunit-lib" "math-lib" "beautiful-racket-lib"))
(define test-omit-paths (list #rx"rkt$"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc" "rackunit-doc" "at-exp-lib" "math-doc"))