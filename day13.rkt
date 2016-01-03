#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[13]

@defmodule[aoc-racket/day13]

@link["http://adventofcode.com/day/13"]{The puzzle}. Our @link-rp["day13-input.txt"]{input} is a list of descriptions of ``happiness units'' that would be gained or lost among eight people sitting next to each other at a dinner table.

@chunk[<day13>
       <day13-setup>
       <day13-q1>
       <day13-q2>
       <day13-test>]

@section{What's the optimal happiness score for a seating arrangement of eight?}

This is a lot like @secref{Day_9}, where we had to compute the optimal path between cities. In that puzzle, the distance between city A and city B was a single number. In this case, the ``happiness score'' between person A and person B is the sum of two numbers — A's happiness being next to B, and B's happiness being next to A. (Unlike distances, happiness scores can be negative.)

Also, whereas a path between cities had a start and end, a seating arrangement is circular. So if we model a seating arrangement as a list of people, we have to compute the happiness between each duo of people, but also between the last and first, to capture the circularity of the arrangement.

Those wrinkles noted, we'll proceed as we did in @secref{Day_9}. We'll parse the input data and put the happiness scores into a hash table — the keys will be of the form @racket[(list name1 name2)] and the values will be the happiness scores for that duo, in that order. Then we'll loop through all possible seating arrangements with @racket[in-permutations] and see what the best score is.



@chunk[<day13-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define happiness-scores (make-hash))
       
       (define (parse-happiness-score ln)
         (define result
           (regexp-match #px"^(.*?) would (gain|lose) (\\d+) happiness units by sitting next to (.*?)\\.$" (string-downcase ln)))
         (when result
           (match-define (list _ name1 op amount name2) result)
           (hash-set! happiness-scores (list name1 name2)
                      ((if (equal? op "gain") + -) (string->number amount)))))
       
       (define (calculate-happiness table-arrangement)
         (define table-arrangement-rotated-one-place
           (append (drop table-arrangement 1) (take table-arrangement 1)))
         (define clockwise-duos
           (map list table-arrangement table-arrangement-rotated-one-place))
         (define counterclockwise-duos (map reverse clockwise-duos))
         (define all-duos (append clockwise-duos counterclockwise-duos))
         (for/sum ([duo (in-list all-duos)])
                  (hash-ref happiness-scores duo)))
                                                   
       ]


@subsection{Optimizing @racket[in-permutations]}

I'm in a math-jock mood, so let's make a performance optimization. It's unnecessary for this problem, but when we use @racket[in-permutations] — which grows at factorial speed — we should ask how we might prune the options.

Notice that because our seating arrangement is circular, our permutations will include a lot of ``rotationally equivalent'' arrangements — e.g., @racket['(A B C ...)] is the same as @racket['(B C ... A)], @racket['(C ... A B)], etc. If we have @racket[_n] elements, each distinct arrangement will have @racket[_n] rotationally equivalent arrangements. We can save time by only checking one of each set.

How? By only looking at arrangements starting with a particular name. Doesn't matter which. This will work because every name has to appear in every arrangement. To do this, we could generate all the permutations and use a @racket[#:when] clause to select the ones we want. But it's even more efficient to only permute @racket[(sub1 _n)] names, and then @racket[cons] our first-position name onto each partial arrangement, which will produce the same set of arrangements. Thus we only have to generate and score @racket[(/ 1 _n)] of the original permutations.

@chunk[<day13-q1>
       
       (define (q1 input-str)
         (for-each parse-happiness-score (string-split input-str "\n"))
         (define names
           (remove-duplicates (flatten (hash-keys happiness-scores))))
         (define table-arrangement-scores
           (for/list ([partial-table-arrangement (in-permutations (cdr names))])
                     (define table-arrangement (cons (car names) partial-table-arrangement))
                     (calculate-happiness table-arrangement)))
         (apply max table-arrangement-scores))]



@section{What's the optimal happiness score, including ourself in the seating?}

We can reuse our hash table of @racket[happiness-scores], but we have to update it with scores for ourself seated next to every other person, which in every case is @racket[0]. Then we find the optimal score the same way.

@chunk[<day13-q2>
       
       (define (q2 input-str)
         (define names
           (remove-duplicates (flatten (hash-keys happiness-scores))))
         
         (for ([name (in-list names)])
              (define me-duo (list "me" name))
              (hash-set*! happiness-scores
                          me-duo 0
                          (reverse me-duo) 0))
         
         (define table-arrangement-scores
           (for/list ([partial-table-arrangement (in-permutations names)])
                     (define table-arrangement (cons "me" partial-table-arrangement))
                     (calculate-happiness table-arrangement)))
         (apply max table-arrangement-scores))
                                              
       ]


@section{Testing Day 13}

@chunk[<day13-test>
       (module+ test
         (define input-str (file->string "day13-input.txt"))
         (check-equal? (q1 input-str) 709)
         (check-equal? (q2 input-str) 668))]


