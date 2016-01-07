#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[25]

@defmodule[aoc-racket/day25]

@link["http://adventofcode.com/day/25"]{The puzzle}. Our @link-rp["day25-input.txt"]{input} is a row and column number for a grid.


@chunk[<day25>
       <day25-setup>
       <day25-q1>
       <day25-test>]

@section{What code do you give the machine?}

The puzzle involves a series of codes in a grid. The first code is @racket[20151125] and subsequent codes are computed by multiplying by @racket[252533] and calculating the remainder after dividing by @racket[33554393]. Then the codes are put into a grid in diagonal fashion, in the order shown:

@tt{
  |  1    2    3    4    5    6
 @(linebreak)
 ++++++++++++++++++++++   @(linebreak)
 
 1 |  1   3   6  10  15  21   @(linebreak)
 
 2 |  2   5   9  14  20   @(linebreak)
 
 3 |  4   8  13  19   @(linebreak)
 
 4 |  7  12  18   @(linebreak)
 
 5 | 11  17   @(linebreak)
 
 6 | 16}

The grid is described as infinite. But this doesn't play a role in the puzzle, since our job is to find the number at the finite coordinate given in our input. So we need a function for finding the @italic{n}th code in the series, and a function for finding which @italic{n} resides at a given grid location. Short & sweet.


@chunk[<day25-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       ]


@chunk[<day25-q1>
       
       (define first-code 20151125)
       
       (define (next-code code)
         (modulo (* code 252533) 33554393))
       
       (define (nth-code n)
         (for/fold ([code-so-far first-code])
                   ([i (in-range (sub1 n))])
           (next-code code-so-far)))
       
       (define (rc->n row col)
         (define first-col-val (add1 (apply + (range row))))
         (define col-offset-val (apply + (range (add1 row) (+ row col))))
         (+ first-col-val col-offset-val))
       
       (define (q1 input-str)
         (match-define (list _ row col)
           (map string->number
                (regexp-match #px"row (\\d+), column (\\d+)" input-str)))
         (nth-code (rc->n row col)))
                  
       ]



@section{Testing Day 25}

@chunk[<day25-test>
       (module+ test
         (define input-str (file->string "day25-input.txt"))
         (check-equal? (q1 input-str) 19980801))]


