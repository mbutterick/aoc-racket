#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[9]

@defmodule[aoc-racket/day09]

@link["http://adventofcode.com/day/9"]{The puzzle}. Our @link-rp["day09-input.txt"]{input} consists of a list of distances between fictional cities, e.g., @italic{AlphaCentauri to Straylight = 133}.

@chunk[<day09>
       <day09-setup>
       <day09-q1>
       <day09-q2>
       <day09-test>]

@isection{What's the shortest route that visits all the cities?}

This puzzle is a version of the famous @link["https://simple.wikipedia.org/wiki/Travelling_salesman_problem"]{traveling-salesman problem}. The problem is famous because there's no reasonable algorithm to solve it for arbitrarily large sets of cities. This version, however, has only eight cities. So it is possible (and easiest) to simply try all the options and see which is shortest.

The solution has two parts. First, we'll parse our input data and put the distances into a mutable hash table. One small wrinkle — the distance between city A and city B is the same whether our path takes us from A to B or B to A. So the keys for our hash will be of the form @racket[(list city-a city-b)], with the cities always in alphabetical order.

In the second part, we'll loop through every possible path between the cities with @iracket[in-permutations]. We'll split each path into pairs of cities, look up each distance between pairs, and sum them. This will give us a list of distances, and we can find the smallest with @iracket[min].

@margin-note{The reason the traveling-saleman problem is generally difficult is that the number of permutations of @racket[_n] cities is @racket[(factorial (sub1 _n))], which gets very large, very quickly.}

@chunk[<day09-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define distances (make-hash))
       
       (define (str->hash ln)
         (match-define (list _ here there dist)
           (regexp-match #px"^(\\w+) to (\\w+) = (\\d+)" ln))
         (define key (places->key here there))
         (hash-set! distances key (string->number dist)))
       
       (define (places->key here there)
         (sort (list (string-downcase here) (string-downcase there)) string<?))
       
       (define (calculate-route-distances)
         (define (pairify xs)
           (map list (drop-right xs 1) (drop xs 1)))
         (define (distance here there)
           (hash-ref distances (places->key here there) +inf.0))
         
         (define cities (remove-duplicates (append* (hash-keys distances))))
         (for/list ([route (in-permutations cities)])
                   (for/sum ([pair (in-list (pairify route))])
                            (apply distance pair))))
       ]

@chunk[<day09-q1>
       
       
       (define (q1 strs)
         (for-each str->hash strs)
         (apply min (calculate-route-distances)))]



@isection{What's the longest route?}

Exactly the same, except we look for the @iracket[max] value among the distances rather than the @racket[min].

@chunk[<day09-q2>
       
       (define (q2 strs)
         (apply max (calculate-route-distances)))       ]


@section{Testing Day 9}

@chunk[<day09-test>
       (module+ test
         (define input-strs (file->lines "day09-input.txt"))
         (check-equal? (q1 input-strs) 251)
         (check-equal? (q2 input-strs) 898))]


