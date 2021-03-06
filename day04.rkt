#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)
@(require (for-label openssl/md5))

@aoc-title[4]

@defmodule[aoc-racket/day04]

@link["http://adventofcode.com/day/4"]{The puzzle}. Our @link-rp["day04-input.txt"]{input} is a string of eight characters that represents part of a key for making an MD5 hash.

@chunk[<day04>
       <day04-setup>
       <day04-q1>
       <day04-q2>
       <day04-test>]

@isection{What is the lowest-numbered MD5 hash starting with five zeroes?}

We're asked to create an MD5 hash from an input key that consists of our eight-character input joined to a decimal number. The puzzle asks us to find the lowest decimal number that, when joined to our input, produces an MD5 hash that starts with five zeroes.

Whether or not you already know what an MD5 hash is, you can search the Racket docs and will soon find the @racketmodname[openssl/md5] module and the @iracket[md5] function. Then, this puzzle is easy: starting at @racket[0], make new input keys with each integer, and stop when we find one that results in the MD5 hash we want. (The approach is similar to the second part of @secref{Day_1}.) 


@chunk[<day04-setup>
       (require racket rackunit openssl/md5)
       (provide (all-defined-out))
       ]

@chunk[<day04-q1>
       (define (q1 str)
         (for/or ([i (in-naturals)])
                 (define md5-key (string-append str (~a i)))
                 (define md5-hash (md5 (open-input-string md5-key)))
                 (and (string-prefix? md5-hash "00000") i)))
       ]

@section{How about six zeroes?}

Exactly the same, except we test for a string of six zeroes. It is likely, however, to take quite a bit longer to run, as the sixth zero essentially makes the criterion 10 times more stringent.

@chunk[<day04-q2>
       (define (q2 str)
         (for/or ([i (in-naturals)])
                 (define md5-key (string-append str (~a i)))
                 (define md5-hash (md5 (open-input-string md5-key)))
                 (and (string-prefix? md5-hash "000000") i)))]

@section{Testing Day 4}

@chunk[<day04-test>
       (module+ test
         (define input-str (file->string "day04-input.txt"))
         (check-equal? (q1 input-str) 346386)
         (check-equal? (q2 input-str) 9958218))]
