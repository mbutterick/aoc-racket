#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[16]

@defmodule[aoc-racket/day16]

@link["http://adventofcode.com/day/16"]{The puzzle}. Our @link-rp["day16-input.txt"]{input} is a list of 500 people named Sue, along with three attribute pairs for each. We're also provided a set of attribute pairs that identifies one of the Sues:

@tt{children: 3
 @(linebreak)
 cats: 7
 @(linebreak)
 samoyeds: 2
 @(linebreak)
 pomeranians: 3
 @(linebreak)
 akitas: 0
 @(linebreak)
 vizslas: 0
 @(linebreak)
 goldfish: 5
 @(linebreak)
 trees: 3
 @(linebreak)
 cars: 2
 @(linebreak)
 perfumes: 1} 

@chunk[<day16>
       <day16-setup>
       <day16-q1>
       <day16-q2>
       <day16-test>]

@section{Which Sue matches the attribute input?}

Our input has 10 attribute pairs, but each of our Sues only has three attribute pairs. The other values for each Sue are unknown. Thus, for each Sue in our list, we need to check if the three known attribute pairs are among in our 10 identifying pairs. Assumedly, this will be true for only one Sue, and that will be the answer.

We might be tempted to break down the attribute pairs into hash tables. But we don't even need to work that hard — we can just treat the attribute pairs as a list of strings, and match whole strings like @racket{children: 3}. And our Sues can be a list too, each holding a list of strings. Cheap & cheerful.

@chunk[<day16-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (parse-sues str)
         (for/list ([ln (in-list (string-split str "\n"))])
                   (define attr-str (second (regexp-match #px"^.*?: (.*)$" ln)))
                   (string-split attr-str ", ")))
       
       (define master-attrs (file->lines "day16-input-master-attrs.txt"))
       
       ]

@chunk[<day16-q1>
       (define (q1 input-str)
         (define sues (parse-sues input-str))
         (for/or ([(sue-attrs sue-number) (in-indexed sues)])
                 (for/and ([sue-attr (in-list sue-attrs)])
                          (and (member sue-attr master-attrs) (add1 sue-number)))))]



@section{Which Sue matches the attribute input, with the ``retroencabulator'' rules?}

Same question as before, with new rules for matching the master attributes:

@itemlist[
          
 @item{The target Sue has more @tt{cats} and @tt{trees} than indicated.}
  
 @item{The target Sue has fewer @tt{pomeranians} and @tt{goldfish} than indicated. (How many pomeranians does anyone really need?)}
 ]

Now that we're asked to compare attribute values in a deeper way, our avoidance of a hash table in question 1 looks like a false economy.

So let's compound our laziness with more laziness. Rather than upgrade to a hash table now, let's convert our strings to @italic{datums} (as saw in @secref{Day_7}). Because if we put a string like @racket{children: 3} inside parentheses like so @racket{(children: 3)} and then convert it to a datum (with @racket[read]) we'll end up with a list with a key and a value, e.g. @racket['(children: 3)]. In other words, just what we needed. (I know the plural of @italic{datum} is @italic{data}, but @italic{datums} better connotes ``more than one datum, in the Racket sense.'')

Plus, it's always fun to find a use for @racket[case] and the frequently overlooked @racket[assoc].

@chunk[<day16-q2>
       
       (define (q2 input-str)
         (define (attrs->datums attrs)
           (map (compose1 read open-input-string
                          (λ(attr) (format "(~a)" attr))) attrs))
         (define sues (for/list ([sue-attrs (parse-sues input-str)])
                                (attrs->datums sue-attrs)))
         (define master-datums (attrs->datums master-attrs)) 
         (for/or ([(sue-datums sue-number) (in-indexed sues)])
                 (for/and ([sue-datum (in-list sue-datums)])
                          (and
                           (let* ([sue-key (first sue-datum)]
                                  [sue-value (second sue-datum)]
                                  [master-value (second (assoc sue-key master-datums))]
                                  [cmp (case sue-key
                                         [(cats: trees:) >]
                                         [(pomeranians: goldfish:) <]
                                         [else =])])
                             (cmp sue-value master-value))
                           (add1 sue-number)))) )
                                                 
       ]



@section{Testing Day 16}

@chunk[<day16-test>
       (module+ test
         (define input-str (file->string "day16-input.txt"))
         (check-equal? (q1 input-str) 103)
         (check-equal? (q2 input-str) 405))]


