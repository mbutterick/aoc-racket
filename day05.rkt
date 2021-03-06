#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[5]

@defmodule[aoc-racket/day05]

@link["http://adventofcode.com/day/5"]{The puzzle}. Our @link-rp["day05-input.txt"]{input} is a list of random-looking but not really random text strings.

@chunk[<day05>
       <day05-setup>
       <day05-q1>
       <day05-q2>
       <day05-test>]

@isection{How many strings are ``nice''?}

A string is ``nice'' if it meets certain criteria:

@itemlist[
 @item{Contains three vowels (= @litchar{aeiou}).}
 @item{Contains a double letter.}
 @item{Does not contain @litchar{ab}, @litchar{cd}, @litchar{pq}, or @litchar{xy}.}
 ]

This is a job for @iracket[regexp-match]. There's nothing tricky here (except for remembering that certain matching functions require the @iracket[pregexp] pattern prefix rather than @racket[regexp]).


@chunk[<day05-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       ]

@chunk[<day05-q1>
       (define (nice? str)
         (define (three-vowels? str)
           (>= (length (regexp-match* #rx"[aeiou]" str)) 3))
         (define (double-letter? str)
           (regexp-match #px"(.)\\1" str))
         (define (no-kapu? str)
           (not (regexp-match #rx"ab|cd|pq|xy" str)))
         (and (three-vowels? str)
              (double-letter? str)
              (no-kapu? str)))
       
       (define (q1 words)
         (length (filter nice? words)))
                                       
       ]

@section{How many strings are ``nice'' under new rules?}

This time a string is ``nice`` if it:

@itemlist[
@item{Contains a pair of two letters that appears twice without overlapping}
@item{Contains a letter that repeats with at least one letter in between}
          ]

Again, a test of your regexp-writing skills.

@chunk[<day05-q2>
       (define (nicer? str)
         (define (nonoverlapping-pair? str)
           (regexp-match #px"(..).*\\1" str))
         (define (separated-repeater? str)
           (regexp-match #px"(.).\\1" str))
         (and (nonoverlapping-pair? str)
              (separated-repeater? str) #t))
       
       (define (q2 words)
         (length (filter nicer? words)))]

@section{Testing Day 5}

@chunk[<day05-test>
       (module+ test
         (define input-str (file->lines "day05-input.txt"))
         (check-equal? (q1 input-str) 238)
         (check-equal? (q2 input-str) 69))]
