#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[5]

Our @link-rp["day5-input.txt"]{input} is a list of random-looking but not really random text strings.

@chunk[<day5>
       <day5-setup>
       <day5-q1>
       <day5-q2>
       <day5-test>]

@section{How many strings are ``nice''?}

A string is ``nice'' if it meets certain criteria:

@itemlist[
 @item{Contains three vowels (= @litchar{aeiou}).}
 @item{Contains a double letter.}
 @item{Does not contain @litchar{ab}, @litchar{cd}, @litchar{pq}, or @litchar{xy}.}
 ]

This is a job for @racket[regexp-match]. There's nothing tricky here (except for remembering that certain matching functions require the @racket[pregexp] pattern prefix rather than @racket[regexp]).


@chunk[<day5-setup>
       (require racket rackunit)
       ]

@chunk[<day5-q1>
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

@chunk[<day5-q2>
       (define (nicer? str)
         (define (nonoverlapping-pair? str)
           (regexp-match #px"(..).*\\1" str))
         (define (separated-repeater? str)
           (regexp-match #px"(.).\\1" str))
         (and (nonoverlapping-pair? str)
              (separated-repeater? str) #t))
       
       (define (q2 words)
         (length (filter nicer? words)))]

@section{Testing our input}

@chunk[<day5-test>
       (module+ test
         (define input-str (file->lines "day5-input.txt"))
         (check-equal? (q1 input-str) 238)
         (check-equal? (q2 input-str) 69))]
