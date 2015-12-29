#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[3]

Our @link-rp["day3/input.txt"]{input} is a string made of the characters @litchar{^v<>} that represent north, south, west, and east. Taken together, the string represents a path through an indefinitely large grid.

In essence, this a two-dimensional version of the elevator problem in @secref["day-1"].

@chunk[<day3>
       <setup>
       <test>]

@section{How many grid cells are visited?}

In the elevator problem, we modeled the parentheses that represented up and down as @racket[1] and @racket[-1]. We'll proceed the same way here, but we'll assign Cartesian coordinates to each possible move — @racket['(0 1)] for north, @racket['(-1 0)] for west, and so on.

For dual-valued data, whether to use @seclink["pairs" #:doc '(lib "scribblings/guide/guide.scrbl")]{pairs or lists} is largely a stylistic choice. How do you plan to process the data? In this case, the way we create the path is by adding the x and y coordinates of the current cell and the new move. So it ends up being convenient to model these cells as lists rather than pairs, so we can add them with a simple @racket[(map + move cell)].

Once the whole cell path is computed, the answer is found by removing duplicate cells and counting how many remain.

@chunk[<setup>
       (require racket rackunit)
       (define (string->cells str)
         (define start '(0 0))
         (define moves (for/list ([s (in-list (regexp-match* #rx"." str))])
                                 (case s
                                   [("^") '(0 1)]
                                   [("v") '(0 -1)]
                                   [("<") '(-1 0)]
                                   [(">") '(1 0)])))
           (reverse (for/fold ([cells-so-far (list start)])
                              ([move (in-list moves)])
                      (define last-cell (car cells-so-far))
                      (define next-cell (map + move last-cell))
                      (cons next-cell cells-so-far))))

       (define (q1 str)
         (length (remove-duplicates (string->cells str))))]

@section{How many grid cells are visited if ?}

@chunk[<test>
       (module+ test
         (define input-str (file->string "input.txt"))
         (check-equal? (q1 input-str) 2565)
         #;(check-equal? (q2 input-str) 2639))]
