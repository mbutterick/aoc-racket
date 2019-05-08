#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[17]

@defmodule[aoc-racket/day17]

@link["http://adventofcode.com/day/17"]{The puzzle}. Our @link-rp["day17-input.txt"]{input} is a list of containers that hold the designated number of liters.


@chunk[<day17>
       <day17-setup>
       <day17-q1>
       <day17-q2>
       <day17-test>]

@section{How many combinations of containers fit exactly 150 liters?}

This is a lot like the second part of @secref{Day_15}, where we had to find cookie recipes that totaled 500 calories. This time, rather than recipes, we need to generate combinations of the containers that add up to exactly 150 liters (though we don't have to use all the containers, and multiple containers of the same size are deemed to create unique arrangements).

We do this by creating the @italic{power set} of the containers — that is, a list of all possible subsets — and counting how many meet our criterion. As with the recipe problem, our @racket[powerset] function is a simple recursive operation.

@chunk[<day17-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (powerset xs)
         (if (empty? xs)
             (list empty)
             (append-map
              (λ (s) (list (cons (car xs) s) s))
              (powerset (cdr xs)))))             
       ]

@chunk[<day17-q1>
       (define (q1 input-str)
         (define containers
           (map string->number (string-split input-str)))
         (length (filter (λ (s) (= 150 (apply + s)))
                         (powerset containers))))]



@section{How many combinations have the minimum number of containers?}

Same as above, except we find the minimum length among the winners, and then count how many other winners have that length.

@chunk[<day17-q2>
       
       (define (q2 input-str)
         (define containers
           (map string->number (string-split input-str)))
         (let* ([winners (filter (λ (s) (= 150 (apply + s)))
                                 (powerset containers))]
                [shortest (apply min (map length winners))])
           (length (filter (λ (w) (= shortest (length w))) winners))))
                                                                     
       ]



@section{Testing Day 17}

@chunk[<day17-test>
       (module+ test
         (define input-str (file->string "day17-input.txt"))
         (check-equal? (q1 input-str) 1638)
         (check-equal? (q2 input-str) 17))]


