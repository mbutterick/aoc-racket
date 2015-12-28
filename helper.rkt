#lang at-exp racket/base
(require scribble/manual)
(provide (all-defined-out))

(define (aoc-title which)
  (define which-str (number->string which))
@title[#:style manual-doc-style]{@link[@string-append["http://adventofcode.com/day/" @which-str]]{Day @which-str}})

(define (input)
  @link["input.txt"]{input})