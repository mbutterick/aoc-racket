#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[20]

@defmodule[aoc-racket/day20]

@link["http://adventofcode.com/day/20"]{The puzzle}. Our @link-rp["day20-input.txt"]{input} is a target number of presents, in this case @racket[36000000].


@chunk[<day20>
       <day20-setup>
       <day20-q1>
       <day20-q2>
       <day20-test>]

@isection{What's the first house that gets the target number of presents?}

We're asked to imagine infinite elves delivering presents to an infinite sequence of houses. (Already @link["http://practicaltypography.com/the-infinite-pixel-screen.html"]{I like} this puzzle.) The first elf delivers a present to every house equal to 10 times his number (= 10); the second elf, 20 gifts to every second house; the @italic{n}th elf, 10@italic{n} gifts to every @italic{n}th house.

Math jocks will notice that the elf behavior roughly describes a @link["https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"]{Sieve of Eratosthenes}. Each house is visited by elf @italic{n} only if @italic{n} is a divisor of the house number. (Houses that are primes are therefore only visited by the first elf.) Might there be a Racket function that finds the divisors of a number? Why, yes — it's called @iracket[divisors]. We can use it to find the numbers of the elves that visit a house, and loop through house numbers till we reach the target. (The 10-gift multiplier is arbitrary.)

@chunk[<day20-setup>
       (require racket rackunit (only-in math divisors))
       (provide (all-defined-out))
       ]

@chunk[<day20-q1>
       
       (define (q1 input-str)
         (define target-gifts (read (open-input-string input-str)))
         (define gifts-per-elf 10)
         (for/first ([house-number (in-naturals)]
                     #:when (let* ([elves (divisors house-number)]
                                   [elf-gifts
                                    (apply + (map (curry * gifts-per-elf) elves))]) 
                              (>= elf-gifts target-gifts)))
                    house-number))]



@isection{What's the first house that gets the target number of presents, if each elf delivers 11 gifts to 50 houses?}

Going with the math-jock vibe, what this condition means is that the highest-numbered house the @italic{n}th elf will visit is 50@italic{n}. So the answer for this question is like the first, but we'll @iracket[filter] the list of elves using this condition.

@chunk[<day20-q2>
       
       (define (q2 input-str)
         (define target-gifts (read (open-input-string input-str)))
         (define gifts-per-elf 11)
         (for/first ([house-number (in-naturals)]
                     #:when (let* ([elves (divisors house-number)]
                                   [elves (filter
                                           (λ(e) (<= house-number (* 50 e))) elves)]
                                   [elf-gifts
                                    (apply + (map (curry * gifts-per-elf) elves))]) 
                              (>= elf-gifts target-gifts)))
                    house-number))
                                  
       ]



@section{Testing Day 20}

@chunk[<day20-test>
       (module+ test
         (define input-str (file->string "day20-input.txt"))
         (check-equal? (q1 input-str) 831600)
         (check-equal? (q2 input-str) 884520))]


