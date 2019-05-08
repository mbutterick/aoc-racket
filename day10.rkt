#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[10]

@defmodule[aoc-racket/day10]

@link["http://adventofcode.com/day/10"]{The puzzle}. Our @link-rp["day10-input.txt"]{input} is a short numeric key.

@chunk[<day10>
       <day10-setup>
       <day10-q1>
       <day10-q2>
       <day10-test>]

@isection{What's the length of the sequence after 40 iterations?}

The puzzle asks us to compute the @italic{look and say} sequence invented by mathematician John Conway. Each iteration of the sequence is the description of the last step if you said it in numbers. So @racket[1] becomes ``one 1'', written @racket[11]; @racket[11] becomes ``two ones'', or @racket[21], then @racket[1211], @racket[111221], and so on.

As in @secref{Day_1}, this puzzle relies on cumulative state, so we'll loop using @iracket[for/fold]. To generate the new string for each pass of the loop, we'll use @iracket[regexp-match*] to find every contiguous run of digits. Each digit run is converted into a list with the number of digits and the digit itself. Then all these lists are concatenated into a new string, and the loop repeats.

The second part of the puzzle is just going to change the number of iterations. So we'll make one function that can be used for both parts.

@chunk[<day10-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (look-and-say iterations input-key)
          (for/fold ([start input-key])
                    ([i (in-range iterations)])
            (define digit-runs (regexp-match* #px"(\\d)\\1*" start))
            (string-append*
             (map ~a
                  (append-map (λ (digit-run)
                                (list (string-length digit-run)
                                      (substring digit-run 0 1)))
                              digit-runs)))))
                                              
       ]

@chunk[<day10-q1>
       
       (define (q1 input-key)
         (string-length (look-and-say 40 input-key)))]



@section{After 50 iterations?}

We use the same @racket[look-and-say] function, but with an iteration argument of @racket[50] rather than @racket[40].

@chunk[<day10-q2>
       
       (define (q2 input-key)
         (string-length (look-and-say 50 input-key)))       ]


@section{Testing Day 10}

@chunk[<day10-test>
       (module+ test
         (define input-key (file->string "day10-input.txt"))
         (check-equal? (q1 input-key) 492982)
         (check-equal? (q2 input-key) 6989950))]


