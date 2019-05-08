#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[19]

@defmodule[aoc-racket/day19]

@link["http://adventofcode.com/day/19"]{The puzzle}. Our @link-rp["day19-input.txt"]{input} is list of ``molecule'' transformations that look like @tt{B => TiRnFAr}, and then a longer test molecule.


@chunk[<day19>
       <day19-setup>
       <day19-q1>
       <day19-q2>
       <day19-test>]

@isection{How many distinct molecules can be created after one transformation?}

Starting with our test molecule, we are asked to try every molecule transformation at every possible position, and count up the distinct molecules that are created.

Each molecule transformation defines a string replacement. We'll parse our input  into a test molecule, and a list of transformations (= each transformation is a list of a before and after string). Because we want to perform each transformation at every possible point in the test molecule, we can't use @racket[regexp-replace] (because it only replaces the first match) or @iracket[regexp-replace*] (because it replaces all matches). Instead we'll use @iracket[regexp-match-positions*] to generate a list of match positions, and then perform the substitutions at each location to generate our list of molecules. After doing this for every transformation, we can @iracket[remove-duplicates] and see how many are left.


@chunk[<day19-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (parse-input-str input-str)
         (match-define (cons molecule transformation-strings)
           (reverse (string-split input-str "\n")))
         (define transformations
           (filter-not empty?
                       (map (curryr string-split " => ")  transformation-strings)))
         (values molecule transformations))
                                           
                                           
       ]

@chunk[<day19-q1>
       (define (transform-molecule* molecule target-atom replacement-atom)
         (for/list ([pos (in-list (regexp-match-positions* (regexp target-atom) molecule))])
                   (match-define (cons start finish) pos)
                   (string-append (substring molecule 0 start)
                                  replacement-atom
                                  (substring molecule finish (string-length molecule)))))
       
       (define (q1 input-str)
         (define-values (molecule transformations) (parse-input-str input-str))
         (length
          (remove-duplicates
           (append-map (λ (target replacement)
                         (transform-molecule* molecule target replacement))
                       (map first transformations) (map second transformations)))))]



@isection{What's the fewest number of transformations that will generate the test module?}

Unlike some of the puzzles, this second part is a lot harder. The idea is that the test molecule was made from a series of transformations, starting with the single atom @racket{e}. So not only do we have to reverse-engineer this process, but we have to find the most efficient path.

There were three questions in Advent of Code that I couldn't figure out. This was the first. The answer to the first question seems to provide a clue: we could reverse the transformations, perform the same replacement process, and we'd have a list of precursor molecules that could've existed one step before the test molecule. Then you repeat this process with all the possible precursors, until you get back to @racket{e}. This will work in theory, but not in practice, because it requires checking too many possibilities.

So I cheated. I adapted a @link["https://github.com/ChrisPenner/Advent-Of-Code-Polyglot/blob/master/python/19/part2.py"]{Python algorithm} for Racket. I can tell you how it works, though I'm still figuring out @italic{why} it does. This function loops through the transformations and applies them everywhere they will fit. When it reaches a dead end — meaning, the molecule hasn't changed during the loop — it randomly reorders the transformations with @iracket[shuffle] and tries again. It is strange to me that this process would converge to an answer at all, let alone the best answer, let alone so quickly.

@chunk[<day19-q2>
       
       (define (q2 input-str)
         (define-values (starting-molecule xforms) (parse-input-str input-str))
         (let loop ([current-mol starting-molecule][transform-count 0]
                                                   [shuffles 0][xforms xforms])
           (cond
             [(equal? current-mol "e") transform-count]
             [else
              (define-values (xformed-mol last-count)
                (for/fold ([mol current-mol][count-so-far transform-count])
                          ([(from to) (in-parallel (map first xforms) (map second xforms))])
                  (values (string-replace mol to from)
                          (+ count-so-far (length (regexp-match* to mol))))))
              (if (not (equal? current-mol xformed-mol))
                  (loop xformed-mol last-count shuffles xforms)
                  (loop starting-molecule 0 (add1 shuffles) (shuffle xforms)))])))
                                                                                  
       ]



@section{Testing Day 19}

@chunk[<day19-test>
       (module+ test
         (define input-str (file->string "day19-input.txt"))
         (check-equal? (q1 input-str) 576)
         (check-equal? (q2 input-str) 207))]


