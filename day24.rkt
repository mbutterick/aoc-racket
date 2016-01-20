#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[24]

@defmodule[aoc-racket/day24]

@link["http://adventofcode.com/day/24"]{The puzzle}. Our @link-rp["day24-input.txt"]{input} is a list of weights of certain packages.


@chunk[<day24>
       <day24-setup>
       <day24-q1>
       <day24-q2>
       <day24-test>]

@isection{What's the score of the optimal group of packages, when divided into three groups?}

We need to arrange our packages into three groups of equal weight. There are multiple ways to do this, of course. According to the puzzle, the optimal arrangement is the one that results in a group with the fewest number of packages. If multiple arrangements yield an equally small group, then the winner is the one with the lowest ``quantum entanglement'' score, which is defined as the product of the weights.

This puzzle brings together elements we've seen before. Each group in a valid arrangement needs to have equal weight, which is the total weight divided by 3. Rather than immediately trying to find valid three-group arrangements, we can start with single groups of the right weight, sorted by our scoring criteria, and find the first one that's part of a valid solution.

In principle, we could reuse the @racket[powerset] function we made for @secref{Day_17} to generate all possible groups, and then @racket[filter] for the groups that are the correct weight. But in truth, we got lucky on @secref{Day_17}. There were only 20 elements, so our power set contained @racket[(expt 2 20)] (about a million) options, which only takes a few seconds to search. This time, we have 28 elements, thus 256 times as many options, which would require 10–15 minutes of searching. 

To be more efficient, we'll use @iracket[for*/first] to alternate between generating lists of possible groups, and then testing them. We'll @iracket[sort] these lists by ``quantum entanglement'' so we know we're testing groups from best to worst (similar to what we did in @secref{Day_21} with equipment cost).

After that, we just need to write a function that will test whether a given group is part of a valid solution. The first group that has a solution is our winner. Our @racket[has-solution?] function works by removing the test group from the set of all packages, and seeing if there is another group within that has the same weight. (We don't have to check the third group — if the first two groups are the same weight, then the third one is too.) 


@chunk[<day24-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (groups packages len goal-weight)
         (cond
           [(= len 0) empty]
           [(= len 1) (map list (filter (curry = goal-weight) packages))]
           [else
            (append*
             (for/list ([x (in-list packages)])
                       (define later-packages (cdr (member x packages)))
                       (append-map (λ(ss) (define new-group (cons x ss))
                                     (if (= goal-weight (weight new-group))
                                         (list new-group)
                                         empty))
                                   (groups later-packages (sub1 len) (- goal-weight x)))))]))
       
       (define (weight group) (apply + group))
       
       (define (quantum-entanglement group) (apply * group))

       (define (remove-group group packages)
         (filter (λ(p) (not (member p group))) packages))
       
       (define (has-solution? group packages)
         (define target-weight (weight group))
         (define remaining-packages (remove-group group packages))
         (for/first ([len (in-range (length remaining-packages))]
                     #:when (not (empty?
                                  (groups remaining-packages len target-weight))))
                    #t))
                        
       ]



@chunk[<day24-q1>
       
       (define (find-three-group-solution all-packages target-weight)
         (for*/first ([len (in-range (length all-packages))]
                      [group (in-list
                               (sort
                                (groups all-packages len target-weight)
                                #:key quantum-entanglement <))]
                      #:when (has-solution? group all-packages))
                     (quantum-entanglement group)))
       
       (define (q1 input-str)
         (define all-packages (map string->number (string-split input-str)))
         (define target-weight (/ (weight all-packages) 3))
         (find-three-group-solution all-packages target-weight))
                                                                
       ]



@section{What's the optimal score when divided into four groups?}

The loop is similar, but instead of using @racket[has-solution?] (which in essence tests if there is a two-group solution), we ask if there is a three-group solution after our target group is removed. So we create a recursive relationship between the second question and the first.

@chunk[<day24-q2>
       
       (define (q2 input-str)
         (define all-packages (map string->number (string-split input-str)))
         (define target-weight (/ (weight all-packages) 4))
         (for*/first ([len (in-range (length all-packages))]
                      [group (in-list (sort
                                        (groups all-packages len target-weight)
                                        #:key quantum-entanglement <))]
                      #:when (find-three-group-solution
                              (remove-group group all-packages) target-weight))
                     (quantum-entanglement group)))
                                                    
       ]



@section{Testing Day 24}

@chunk[<day24-test>
       (module+ test
         (define input-str (file->string "day24-input.txt"))
         (check-equal? (q1 input-str) 10439961859)
         (check-equal? (q2 input-str) 72050269))]


