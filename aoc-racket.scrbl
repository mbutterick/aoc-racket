#lang scribble/manual
@(require (for-label racket rackunit sugar/list))

@title{Advent of Code solutions & explanations}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[aoc-racket]

@italic{Dedicated to curious characters everywhere, especially those learning Racket.}

@link["http://adventofcode.com"]{Advent of Code} is a series of programming puzzles designed by @link["http://was.tl"]{Eric Wastl}.

I find that programming puzzles are a good way of learning something new about a programming language, or learning how to do certain things better. Documenting these solutions helped me nail down some discoveries.

Thank you to Eric Wastl. If you like Advent of Code, please @link["http://adventofcode.com/about"]{pay him for it}.

You can install this package (if you haven't already) with

@tt{raco pkg install aoc-racket}

@local-table-of-contents[]

@include-section[(submod "day1.rkt" doc)]
@include-section[(submod "day2.rkt" doc)]
@include-section[(submod "day3.rkt" doc)]
@include-section[(submod "day4.rkt" doc)]
@include-section[(submod "day5.rkt" doc)]
@include-section[(submod "day6.rkt" doc)]
@include-section[(submod "day7.rkt" doc)]
@include-section[(submod "day8.rkt" doc)]
@include-section[(submod "day9.rkt" doc)]
@include-section[(submod "day10.rkt" doc)]
@include-section[(submod "day11.rkt" doc)]
@include-section[(submod "day12.rkt" doc)]
@include-section[(submod "day13.rkt" doc)]
@include-section[(submod "day14.rkt" doc)]
@include-section[(submod "day15.rkt" doc)]
@include-section[(submod "day16.rkt" doc)]
@include-section[(submod "day17.rkt" doc)]
@include-section[(submod "day18.rkt" doc)]