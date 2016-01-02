#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[8]

@defmodule[aoc-racket/day8]

@link["http://adventofcode.com/day/8"]{The puzzle}. Our @link-rp["day8-input.txt"]{input} consists of a list of seemingly random strings within quotation marks.

@chunk[<day8>
       <day8-setup>
       <day8-q1>
       <day8-q2>
       <day8-test>]

@section{What's the difference between the literal length of the strings, and their length in memory?}

The puzzle relies the fact that within strings, certain single characters — like the backslash @litchar{\} and double-quote mark @litchar{"} — are described with more than one character. Thus, the question asks us to compare the two lengths.

The literal length of the string is trivial — use @racket[string-length]. The memory length requires interpreting a string as a Racket value, which (as seen in @secref{Day_7}) simply means using @racket[read].

@chunk[<day8-setup>
       (require racket rackunit)
       (provide (all-defined-out))
]

@chunk[<day8-q1>
       (define (memory-length str) (string-length (read (open-input-string str))))
       
       (define (q1 strs)
         (- (apply + (map string-length strs)) (apply + (map memory-length strs))))]



@section{What's the difference between the re-encoded length of the literal string, and the original length?}

This question simply comes down to — do you know how to use the string-formatting functions in your programming language?

In Racket, a string can be re-encoded with @racket[~v]. Not a very puzzling puzzle overall.


@chunk[<day8-q2>
       (define (encoded-length str) (string-length (~v str)))
       
       (define (q2 strs)
         (- (apply + (map encoded-length strs)) (apply + (map string-length strs))))       ]


@section{Testing Day 8}

@chunk[<day8-test>
       (module+ test
         (define input-strs (file->lines "day8-input.txt"))
         (check-equal? (q1 input-strs) 1333)
         (check-equal? (q2 input-strs) 2046))]


