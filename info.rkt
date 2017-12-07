#lang info
(define collection "aoc-racket")
(define scribblings '(("aoc-racket.scrbl" (multi-page))))
(define deps '("graph"
               "base" "scribble-lib" ("sugar" #:version "0.3") "rackunit-lib" "math-lib" "beautiful-racket-lib"))
(define test-omit-paths (list #rx"rkt$"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc" "rackunit-doc" "at-exp-lib" "math-doc"))