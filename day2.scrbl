#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[2]

@link["http://adventofcode.com/day/2"]{The puzzle}. Our @link-rp["day2-input.txt"]{input} is a list of strings that represent dimensions of rectangular boxes.

@chunk[<day2>
       <day2-setup>
       <day2-q1>
       <day2-test>]


@section{How much paper is needed to wrap the boxes?}

According to the problem, the paper needed to wrap a present is the surface area of the box (= the sum of the areas of the sides) plus the area of the smallest side. 

First we need to parse our input file into a list of box dimensions. We'll model each box as a list of three dimensions. (The question doesn't need us to keep height / width / depth straight, so we won't worry about it.)

Then we have a traditional setup for the devastating one-two punch of @racket[map] and @racket[apply]. We'll write a function to compute surface area from box dimensions. Then we'll @racket[map] that function across the list of boxes, and finally @racket[apply] the @racket[+] operator to our list of results to get the answer.


@chunk[<day2-setup>
       (require racket rackunit)
       (define (string->boxes str)
         (for/list ([ln (in-list (string-split str "\n"))])
                   (map string->number (string-split ln "x"))))]

@chunk[<day2-q1>
       (define (box->paper box)
         (match-define (list x y z) box)
         (define sides (list (* x y) (* y z) (* x z)))
         (+ (* 2 (apply + sides)) (apply min sides)))
       
       (define (q1 str)
         (define boxes (string->boxes str))
         (apply + (map box->paper boxes)))]

@section{How much ribbon is needed to wrap the boxes?}

According to the problem, the ribbon needed is the perimeter of the smallest side plus the volume of the box.

We take the same approach, with a new @racket[box->ribbon] function.

@chunk[<day2-q1>
       (define (box->ribbon box)
         (match-define (list x y z) box)
         (define (perimeter dim1 dim2) (* 2 (+ dim1 dim2)))
         (define perimeters
           (list (perimeter x y) (perimeter y z) (perimeter x z)))
         (+ (apply min perimeters) (* x y z)))
       
       (define (q2 str)
         (define boxes (string->boxes str))
         (apply + (map box->ribbon boxes)))]


@section{Testing Day 2}


@chunk[<day2-test>
       (module+ test
         (define input-str (file->string "day2-input.txt"))
         (check-equal? (q1 input-str) 1586300)
         (check-equal? (q2 input-str) 3737498))]