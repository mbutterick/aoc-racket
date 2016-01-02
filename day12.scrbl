#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[12]

@link["http://adventofcode.com/day/12"]{The puzzle}. Our @link-rp["day12-input.txt"]{input} is, unfortunately, a @link["http://json.org/"]{JSON} file.

@chunk[<day12>
       <day12-setup>
       <day12-q1>
       <day12-q2>
       <day12-test>]

@section{What's the sum of all the numbers in the document?}

I've never liked JavaScript, and spending more time with Racket has only deepened my antipathy. So I apologize if this solution is terse.

We need to parse the JSON file, extract the numbers, and add them.

To parse the file we'll use the @racket[read-json] function from Racket's @racketmodname[json] library. This function converts the JSON into a JS-expression (see @racket[jsexpr?]), which is a recursively nested data structure. If we had a simple recursively nested list, we could just @racket[flatten] it and filter for the numbers. We'll do something similar here — recursively flatten the JS-expression and pull out the numbers.

If you're new to Racket, notice the @italic{recursive descent} pattern used in @racket[flatten-jsexpr] — it's a very common way of handling recursively structured data.


@chunk[<day12-setup>
       (require racket rackunit json)
       
       (define (string->jsexpr str)
         (read-json (open-input-string str)))
                                             
       ]

@chunk[<day12-q1>
       
       (define (flatten-jsexpr jsexpr)
         (flatten
          (let loop ([x jsexpr])
            (cond
              [(list? x)
               (map loop x)]
              [(hash? x)
               (loop (flatten (hash->list x)))]
              [else x]))))
       
       (define (q1 input-str)
         (define json-items (flatten-jsexpr (string->jsexpr input-str)))
         (apply + (filter number? json-items)))]



@section{What's the sum of all the numbers, if hash tables with value @racket{red} are ignored?}

We'll just update our flattening function to skip over hash tables that have @racket{red} among the values.


@chunk[<day12-q2>
       
       (define (flatten-jsexpr-2 jsexpr)
         (flatten
          (let loop ([x jsexpr])
            (cond
              [(list? x)
               (map loop x)]
              [(hash? x)
               (if (member "red" (hash-values x))
                   empty
                   (loop (flatten (hash->list x))))]
              [else x]))))
       
       (define (q2 input-str)
         (define json-items (flatten-jsexpr-2 (string->jsexpr input-str)))
         (apply + (filter number? json-items)))       ]


@section{Testing Day 12}

@chunk[<day12-test>
       (module+ test
         (define input-str (file->string "day12-input.txt"))
         (check-equal? (q1 input-str) 191164)
         (check-equal? (q2 input-str) 87842))]


