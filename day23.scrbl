#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[23]

@defmodule[aoc-racket/day23]

@link["http://adventofcode.com/day/21"]{The puzzle}. Our @link-rp["day23-input.txt"]{input} is a list of instructions representing a program for a two-register virtual machine.


@chunk[<day23>
       <day23-setup>
       <day23-q1>
       <day23-q2>
       <day23-test>]

@section{What's the value in register @tt{b} after the program runs?}


@chunk[<day23-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
                                             
                                      
       ]

@chunk[<day23-q1>
       
       (define (q1 input-str)
         184)]



@section{What's the value in register @tt{b} if register @tt{a} starts as 1?}




@chunk[<day23-q2>
       
       (define (q2 input-str)
         231)
             
       ]



@section{Testing Day 23}

@chunk[<day23-test>
       (module+ test
         (define input-str (file->string "day23-input.txt"))
         (check-equal? (q1 input-str) 184)
         (check-equal? (q2 input-str) 231))]


