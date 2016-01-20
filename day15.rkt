#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[15]

@defmodule[aoc-racket/day15]

@link["http://adventofcode.com/day/15"]{The puzzle}. Our @link-rp["day15-input.txt"]{input} is a list of four cookie ingredients. Each ingredient has scores for capacity, durability, flavor, texture, and calories in each teaspoon.

@chunk[<day15>
       <day15-setup>
       <day15-q1>
       <day15-q2>
       <day15-test>]

@isection{What's the best cookie we can make with 100 tsps of ingredients?}

This is similar the @secref{Day_14} puzzle. Rather than maximizing reindeer distance after 2503 seconds, we're maximizing the score of a cookie after 100 teaspoons of ingredients. But while our ``recipe'' for a reindeer race included a full measure of each reindeer, our cookie recipes can have any combination of ingredients, as long as they total 100 teaspoons. Thus, similar to combinatoric problems like @secref{Day_9} and @secref{Day_13}, we have to generate all possible cookie recipes that total 100 teaspoons, and then find the best-scoring recipe.

Let's do the receipe generator first, since that's the new element. A recipe is a list of teaspoon amounts. A recipe can have any number of teaspoons for each ingredient, as long as they total to 100. This suggests a typical Rackety recursive pattern where we consider the possible amounts for the first ingredient (0–100 tsps) and then recursively generate recipes for the rest of the ingredients. For instance, suppose we have ingredients @racket['(A B C D)]. If we use 5 tsp of @racket[A], we can combine this with every @racket['(B C D)] recipe that totals 95 tsps. In turn, if we use 10 tsp of @racket[B], we can combine this with every @racket['(C D)] recipe that totals 85 tsps. And so on, generating the whole tree of possibilities.

As for the scoring. The scoring function is not per-ingredient. Rather, the quantity of each characteristic — other than calories, which appears last in each list of ingredient characteristics — is summed. Negative values are rounded up to zero. Then these characteristic scores are @italic{multiplied} to get the cookie score. (In this puzzle, the reading comprehension is harder than the coding.)

Having surveyed the territory, making ingredient functions from the text descriptions seems a little overblown. It's just as convenient to represent each ingredient as a list of its characteristic values. So let's just parse the input into lists of values and store them in a hash.


@chunk[<day15-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (str->ingredient-hash str)
         (for/hash ([ln (in-list (string-split (string-replace str "," " ") "\n"))])
                   (match-define (list ingredient-name characteristic-string)
                     (string-split ln ":"))
                   (values ingredient-name
                           (filter number?
                                   (map string->number
                                        (string-split characteristic-string))))))
       
       (define (make-recipes how-many-ingredients total-tsps)
         (cond
           [(= 0 how-many-ingredients) empty]
           [(= 1 how-many-ingredients) (list (list total-tsps))]
           [else
            (append*
             (for/list ([first-amount (in-range (add1 total-tsps))])
                       (map (curry cons first-amount)
                            (make-recipes (sub1 how-many-ingredients)
                                          (- total-tsps first-amount)))))]))             
                                                                            
       ]

@chunk[<day15-q1>
       (define (q1 input-str)
         (define ingredient-hash (str->ingredient-hash input-str))
         (define ingredients (hash-keys ingredient-hash))
         (define how-many-characteristics (length (car (hash-values ingredient-hash))))
         (define tsps 100)
         (define scores
           (for/list ([recipe (in-list (make-recipes (length ingredients) tsps))])
                     (for/product ([char-idx (in-range (sub1 how-many-characteristics))])
                                  (max 0 (for/sum ([tsp-quantity (in-list recipe)]
                                                   [ingredient (in-list ingredients)])
                                                  (* tsp-quantity
                                                     (list-ref (hash-ref ingredient-hash ingredient) char-idx)))))))
         (apply max scores))]



@isection{What's the best cookie we can make with 100 tsps that's exactly 500 calories?}

Same as the first question, but we'll add a @racket[#:when] clause to our recipe loop to only consider recipes equal to 500 calories, and a @racket[recipe->calorie] helper function. (Recall that calories appear last in the characteristics, which is why we use @iracket[last] to retrieve them.) Obviously, these two answers could be combined with simple refactoring.


@chunk[<day15-q2>
       
       (define (q2 input-str)
         (define ingredient-hash (str->ingredient-hash input-str))
         (define ingredients (hash-keys ingredient-hash))
         (define how-many-characteristics (length (car (hash-values ingredient-hash))))
         (define tsps 100)
         (define (recipe->calories recipe)
           (for/sum ([tsp-quantity (in-list recipe)]
                     [ingredient (in-list ingredients)])
                    (* tsp-quantity (last (hash-ref ingredient-hash ingredient)))))
         (define scores
           (for/list ([recipe (in-list (make-recipes (length ingredients) tsps))]
                      #:when (= 500 (recipe->calories recipe)))
                     (for/product ([char-idx (in-range (sub1 how-many-characteristics))])
                                  (max 0 (for/sum ([tsp-quantity (in-list recipe)]
                                                   [ingredient (in-list ingredients)])
                                                  (* tsp-quantity
                                                     (list-ref (hash-ref ingredient-hash ingredient) char-idx)))))))
         (apply max scores))
                  
       ]




@section{Testing Day 15}

@chunk[<day15-test>
       (module+ test
         (define input-str (file->string "day15-input.txt"))
         (check-equal? (q1 input-str) 18965440)
         (check-equal? (q2 input-str) 15862900))]


