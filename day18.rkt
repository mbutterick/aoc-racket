#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[18]

@defmodule[aoc-racket/day18]

@link["http://adventofcode.com/day/18"]{The puzzle}. Our @link-rp["day18-input.txt"]{input} is a 100 × 100 grid describing the initial state of a matrix of lights.


@chunk[<day18>
       <day18-setup>
       <day18-q1>
       <day18-q2>
       <day18-test>]

@isection{How many lights are on after 100 iterations of the light-switching rules?}

There are two rules for incrementing the state of the lighting grid:

@itemlist[
 @item{A light that's on stays on when 2 or 3 adjacent lights are also on, and otherwise turns off.}
 @item{A light that's off turns on if exactly 3 adjacent lights are also on.}
 ]

These rules are equivalent to an implementation of Conway's @link["https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"]{Game of Life} (recall that we implemented another Conway algorithm in @secref{Day_10}).

To model our lighting grid, we'll reuse our technique from @secref{Day_6} of using a single vector and translating between Cartesian coordinates and vector indexes. A lit bulb will be represented by @racket[1], an unlit bulb by, you guessed it, @racket[0].

The heavy lifting is in the @racket[iterate-grid] function, which steps through each bulb, looks at the eight adjacent bulbs in the previous grid, and determines whether the bulb should be on or off.

As we think about that function, we might notice that life will be easier if we don't have to make special accommodations for bulbs at the edges of the grid, which ordinarily don't have eight adjacent bulbs. So what we'll do is add a margin of bulbs around the perimeter, and leave them in the off position. That way, all the bulbs in our original grid will have an eight-bulb neighborhood. So instead of modeling our grid with a 100 × 100 = 10,000 unit vector, we'll use a 102 × 102 = 10,404 unit vector.

After that, it's just a matter of loading our input data into a grid, running @racket[iterate-grid] 100 times — a great job for @iracket[for/fold] — and counting the activated bulbs in the resulting grid.

@chunk[<day18-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define grid-side 102)
       
       (define (rowcol->idx row col) (+ (* grid-side row) col))
       (define (idx->rowcol idx) (quotient/remainder idx grid-side))
       
       (define (count-lit grid) (apply + (vector->list grid)))
       
       (define bulb-on 1)
       (define bulb-off 0)
       
       (define (input->grid str)
         (define grid-vec (make-vector (* grid-side grid-side) bulb-off))
         (for* ([(bulb-row bulb-row-idx) (in-indexed (string-split str))]
                [(bulb bulb-col-idx) (in-indexed (regexp-match* #rx"." bulb-row))])
               (vector-set! grid-vec (rowcol->idx (add1 bulb-row-idx) (add1 bulb-col-idx))
                            (if (equal? bulb "#") bulb-on bulb-off)))
         grid-vec)
       
       (define (bulb+adjacents grid grid-idx)
         (define-values (row col) (idx->rowcol grid-idx))
         (for*/vector ([r (in-range (sub1 row) (+ row 2))]
                       [c (in-range (sub1 col) (+ col 2))])
                      (vector-ref grid (rowcol->idx r c))))
       
       (define (iterate-grid grid)
         (for*/vector ([row (in-range grid-side)]
                       [col (in-range grid-side)])
                      (cond
                        [(or (= row 0) (= col 0)
                             (= row (sub1 grid-side))
                             (= col (sub1 grid-side)))
                         bulb-off]
                        [else
                         (define bulb-idx (rowcol->idx row col))
                         (define bulb (vector-ref grid bulb-idx))
                         (define lit-neighbors
                           (- (count-lit (bulb+adjacents grid bulb-idx)) bulb))
                         (cond
                           [(= bulb-on bulb) (if (<= 2 lit-neighbors 3) bulb-on bulb-off)]
                           [(= 3 lit-neighbors) bulb-on]
                           [else bulb-off])])))
                                            
                                            
       ]

@chunk[<day18-q1>
       (define (q1 input-str)
         (define initial-grid (input->grid input-str))
         (define iterations 100)
         (define final-grid (for/fold ([grid-so-far initial-grid])
                                      ([i (in-range iterations)])
                              (iterate-grid grid-so-far)))
         (count-lit final-grid))]



@section{How many lights are on after 100 iterations, if the corner bulbs are always lit?}

Same as above, except we turn on the four corner bulbs after parsing the input, and after every iteration.

@chunk[<day18-q2>

       (define (light-corners grid)
         (vector-set*! grid (rowcol->idx 1 1) bulb-on
                       (rowcol->idx 1 100) bulb-on
                       (rowcol->idx 100 1) bulb-on
                       (rowcol->idx 100 100) bulb-on)
         grid)

       (define (q2 input-str)
         (define initial-grid (light-corners (input->grid input-str)))
         (define iterations 100)
         (define final-grid (for/fold ([grid-so-far initial-grid])
                                      ([i (in-range iterations)])
                              (light-corners (iterate-grid grid-so-far))))
         (count-lit final-grid))
             
       ]



@section{Testing Day 18}

@chunk[<day18-test>
       (module+ test
         (define input-str (file->string "day18-input.txt"))
         (check-equal? (q1 input-str) 821)
         (check-equal? (q2 input-str) 886))]


