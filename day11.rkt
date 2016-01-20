#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[11]

@defmodule[aoc-racket/day11]

@link["http://adventofcode.com/day/11"]{The puzzle}. Our @link-rp["day11-input.txt"]{input} is a short alphabetic key that represents a password.

@chunk[<day11>
       <day11-setup>
       <day11-q1>
       <day11-q2>
       <day11-test>]

@isection{What's the next password that meets the criteria?}

Though the password is alphabetic, we can increment it as we would a numerical password, by changing the rightmost letter to the next letter (for instance @litchar{x} to @litchar{y}, @litchar{y} to @litchar{z}). When we reach @litchar{z}, we roll over to @litchar{a}, and ``carry over'' the surplus by incrementing the letter to the left.

Furthermore, like @secref{Day_5}, the puzzle provides certain criteria that must be met:

@itemlist[
 @item{The password must have a sequence of three consecutive letters (like @litchar{bcd}).}
  
 @item{The password may not contain @litchar{i}, @litchar{o}, or @litchar{l}.}
 
 @item{The password must contain two different, non-overlapping pairs of letters.}
 ]

As in @secref{Day_5}, we'll use @iracket[regexp-match] to implement tests for these conditions. We'll also use @iracket[regexp-replace*] to build the function that increments a password alphabetically. Then it's a simple matter of looking at passwords until we find one that works.

The @racket[increment-password] function works by using the observation that if the password ends in any number of @litchar{z}s, you have to roll them over to @litchar{a} and increment the letter to the left. Otherwise, you can just increment the last letter — which is actually the same rule, but with zero @litchar{z}s. This logic can all be captured in one regular expression — @racket[#rx"^(.*?)(.)(z*)$"].

The @racket[three-consecutive-letters?] test works by converting the letters to numbers and creating a list of the differences betweeen adjacent values. Any three consecutive letters will differ by value of @racket[1]. So if the list of differences contains the subsequence @racket['(1 1)], then the string has three consecutive letters.


@chunk[<day11-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (increment-password password)
         (define (increment-letter c)
           ((compose1 ~a integer->char add1 char->integer car string->list) c))
         
         (match-define (list _ prefix letter-to-increment trailing-zs)
           (regexp-match #rx"^(.*?)(.)(z*)$" password))
         
         (string-append* (list prefix (increment-letter letter-to-increment)
                               (regexp-replace* #rx"z" trailing-zs "a"))))
       
       (define (three-consecutive-letters? str)
         (define ints (map char->integer (string->list str)))
         (let loop ([differences (map - (cdr ints) (drop-right ints 1))])
           (if (empty? differences)
               #f
               (or (list-prefix? '(1 1) differences) (loop (cdr differences)))))) 
       
       (define (no-iol? str)
         (not (regexp-match #rx"[iol]" str)))
       
       (define (two-nonoverlapping-doubles? str)
         (regexp-match #px"(\\w)\\1.*?(\\w)\\2" str))
       
       (define (valid? password)
         (and (three-consecutive-letters? password)
              (no-iol? password)
              (two-nonoverlapping-doubles? password)))
       
       (define (find-next-valid-password starting-password)
         (define candidate-pw (increment-password starting-password))
         (if (valid? candidate-pw)
             candidate-pw
             (find-next-valid-password candidate-pw)))
                                                      
       ]

@chunk[<day11-q1>
       
       (define (q1 input-key)
         (find-next-valid-password input-key))]



@section{What's the next valid password after that?}

We take the answer to question 1 and use it as input to the same function.

@chunk[<day11-q2>
       
       (define (q2 input-key)
         (find-next-valid-password (q1 input-key)))       ]


@section{Testing Day 11}

@chunk[<day11-test>
       (module+ test
         (define input-key (file->string "day11-input.txt"))
         (check-equal? (q1 input-key) "hxbxxyzz")
         (check-equal? (q2 input-key) "hxcaabcc"))]


