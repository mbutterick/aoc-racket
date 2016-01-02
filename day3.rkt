#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[3]

@defmodule[aoc-racket/day3]

@link["http://adventofcode.com/day/3"]{The puzzle}. Our @link-rp["day3-input.txt"]{input} is a string made of the characters @litchar{^v<>} that represent north, south, west, and east. Taken together, the string represents a path through an indefinitely large grid.

In essence, this a two-dimensional version of the elevator problem in @secref{Day_1}.


@chunk[<day3>
       <day3-setup>
       <day3-q1>
       <day3-q1-complex>
       <day3-q2>
       <day3-test>]

@section{How many grid cells are visited?}

In the elevator problem, we modeled the parentheses that represented up and down as @racket[1] and @racket[-1]. We'll proceed the same way here, but we'll assign Cartesian coordinates to each possible move — @racket['(0 1)] for north, @racket['(-1 0)] for west, and so on.

For dual-valued data, whether to use @seclink["pairs" #:doc '(lib "scribblings/guide/guide.scrbl")]{pairs or lists} is largely a stylistic choice. Ask: what will you do with the data next? That will often suggest the most natural representation. In this case, the way we create each cell in the path is by adding the x and y coordinates of the current cell to the next move. So it ends up being convenient to model these cells as lists rather than pairs, so we can add them with a simple @racket[(map + current-cell next-move)]. (Recall that when you use @racket[map] with multiple lists, it pulls one element from each list in parallel.)

Once the whole cell path is computed, the answer is found by removing duplicate cells and counting how many remain.

@chunk[<day3-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       ]

@chunk[<day3-q1>
       (define (string->cells str)
         (define start '(0 0))
         (match-define (list east north west south) '((1 0) (0 1) (-1 0) (0 -1)))
         (define moves (for/list ([s (in-list (regexp-match* #rx"." str))])
                                 (case s
                                   [(">") east]
                                   [("^") north]
                                   [("<") west]
                                   [("v") south])))
         (for/fold ([cells-so-far (list start)])
                   ([next-move (in-list moves)])
           (define current-cell (car cells-so-far))
           (define next-cell (map + current-cell next-move))
           (cons next-cell cells-so-far)))
       
       (define (q1 str)
         (length (remove-duplicates (string->cells str))))]

@subsection{Alternate approach: complex numbers}

Rather than use Cartesian coordinates, we could rely on Racket's built-in support for complex numbers to trace the path in the complex plane. Complex numbers have a real and an imaginary part — e.g, @racket[3+4i] — and thus, represent points in a plane just as well as Cartesian coordinates. The advantage is that complex numbers are atomic values, not lists. We can add them normally, without resort to @racket[map]. (It's not essential for this problem, but math jocks might remember that complex numbers can be rotated 90 degrees counterclockwise by multiplying by @tt{+i}.)

Again, the problem has nothing to do with complex numbers inherently. Like pairs and lists, they're just another option for encoding dual-valued data.

@chunk[       <day3-q1-complex>
       (define (string->complex-cells str)
         (define start 0)
         (define east 1)
         (define moves (for/list ([s (in-list (regexp-match* #rx"." str))])
                                 (* east (expt +i (case s
                                                    [(">") 0]
                                                    [("^") 1]
                                                    [("<") 2]
                                                    [("v") 3])))))
         (for/fold ([cells-so-far (list start)])
                   ([next-move (in-list moves)])
           (define current-cell (car cells-so-far))
           (define next-cell (+ current-cell next-move))
           (cons next-cell cells-so-far)))
       
       (define (q1-complex str)
         (length (remove-duplicates (string->complex-cells str))))
       ]

@section{How many grid cells are visited if the path is split?}

By ``split'', the puzzle envisions two people starting at the origin, with one following the odd-numbered moves, and the other following the even-numbered moves. So there are two paths instead of one. The question remains the same: how many cells are visited by one path or the other?

The solution works the same as before — the only new task is to split the input into two strings, and then send them through our existing @racket[string->cells] function.

@chunk[<day3-q2>
       (define (split-odds-and-evens str)
         (define-values (odd-chars even-chars)
           (for/fold ([odds-so-far empty][evens-so-far empty])
                     ([c (in-string str)][i (in-naturals)])
             (if (even? i)
                 (values odds-so-far (cons c evens-so-far))
                 (values (cons c odds-so-far) evens-so-far))))
         (values (string-append* (map ~a (reverse odd-chars)))
                 (string-append* (map ~a (reverse even-chars)))))
       
       (define (q2 str)
         (define-values (odd-str even-str) (split-odds-and-evens str))
         (length (remove-duplicates
                  (append (string->cells odd-str) (string->cells even-str)))))
       ]

@section{Testing Day 3}

@chunk[<day3-test>
       (module+ test
         (define input-str (file->string "day3-input.txt"))
         (check-equal? (q1 input-str) 2565)
         (check-equal? (q1-complex input-str) 2565)
         (check-equal? (q2 input-str) 2639))]
