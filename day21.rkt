#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[21]

@defmodule[aoc-racket/day21]

@link["http://adventofcode.com/day/21"]{The puzzle}. Our @link-rp["day21-input.txt"]{input} tells us the hit points, damage, and armor of an enemy we have to defeat.


@chunk[<day21>
       <day21-items>
       <day21-setup>
       <day21-q1>
       <day21-q2>
       <day21-test>]

@section{What's the least we can spend and win?}

I hate RPGs. So at first, I prejudicially refused to do this puzzle. But after a second look, I could see how to do it. So I relented.

We have 100 hit points. Our enemy has the hit points, damage, and armor given in our input. The Fake Equipment Shop is selling the following items (the columns are cost, damage rating, and armor value):

@chunk[<day21-items>
       (define no-item
         '(None           0     0       0))
       
       (define weapons
         '((Dagger        8     4       0)
           (Shortsword   10     5       0)
           (Warhammer    25     6       0)
           (Longsword    40     7       0)
           (Greataxe     74     8       0)))
       
       (define armors
         '((Leather      13     0       1)
           (Chainmail    31     0       2)
           (Splintmail   53     0       3)
           (Bandedmail   75     0       4)
           (Platemail   102     0       5)))
       
       (define rings
         '((Damage+1    25     1       0)
           (Damage+2    50     2       0)
           (Damage+3   100     3       0)
           (Defense+1   20     0       1)
           (Defense+2   40     0       2)
           (Defense+3   80     0       3)))]

We have to buy a weapon. We can buy armor, or not. And we can buy zero, one, or two rings. On each turn, we inflict damage equal to our attack score minus the boss's armor, though we always do at least 1 point of damage. Likewise, the boss inflicts damage equal to her attack minus our armor, with a minimum of 1. Repeat. If the boss's hit points go to zero before ours, we win. Otherwise, not. Thus the question — what's the cheapest set of equipment that wins?

Similar to puzzles we've already seen, this is another combinatoric question. If we follow that pattern, we need to generate all possible equipment sets, find out which ones win, and then pick the cheapest. But there's an easy optimization available here. The cheapness of a set of equipment is independent of whether it wins. So we'll sort our equipment sets by cost, and try them starting from the cheapest. The first one that wins will be our answer (saving us the trouble of trying them all). The total cost, damage, and armor of the equipment set is just the sum of the individual values.

After that, we'll set up a @racket[we-win?] function that will simulate the battle of a certain equipment set vs. the boss. It's a @racket[for/fold] loop that will alternate attacks until either the player or boss has negative hit points, returning @racket[#t] if we win, or @racket[#f] if the boss does.


@chunk[<day21-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define (cost equipment-set)
         (apply + (map second equipment-set)))
       
       (define equipment-sets-by-cost
         (let ([equipment-sets
                (for*/list ([weapon (in-list weapons)]
                            [armor (in-list (cons no-item armors))]
                            [lh-ring (in-list (cons no-item rings))]
                            [rh-ring (in-list (cons no-item (remove lh-ring rings)))])
                           (list weapon armor lh-ring rh-ring))])
           (sort equipment-sets < #:key cost)))
       
       (define player-hit-points 100)
       (define min-damage 1)
       
       (define (equipment-set->player equipment-set)
         (let ([total-damage (apply + (map third equipment-set))]
               [total-armor (apply + (map fourth equipment-set))])
           (list player-hit-points total-damage total-armor)))
       
       (define player-turn? even?)
       (define hit-points first)
       (define damage second)
       (define armor third)
       (define (attack attacker defender)
         (define net-damage (max (- (damage attacker) (armor defender)) min-damage))
         (list (- (hit-points defender) net-damage) (damage defender) (armor defender)))
       
       (define (we-win? player boss)
         (define-values (last-player-state last-boss-state)
           (for/fold ([player-state player][boss-state boss])
                     ([turn-number (in-naturals)]
                      #:break (<= (min (hit-points player-state)
                                       (hit-points boss-state)) 0))
             (if (player-turn? turn-number)
                 (values player-state (player-state . attack . boss-state))
                 (values (boss-state . attack . player-state) boss-state))))
         (<= (hit-points last-boss-state) 0))                                            
                                      
       ]

@chunk[<day21-q1>
       
       (define (q1 input-str)
         (define boss (filter number? (map string->number (string-split input-str))))
         (for/first ([equip (in-list equipment-sets-by-cost)]
                     #:when (let ([player (equipment-set->player equip)])
                              (we-win? player boss)))
                    (cost equip)))]



@section{What's the most we can spend and lose?}

A simple variation of the first answer. Instead, we iterate through the equipment sets starting at the most expensive, and stop when we find the first loser.


@chunk[<day21-q2>
       
       (define (q2 input-str)
         (define boss (filter number? (map string->number (string-split input-str))))
         (for/first ([equip (in-list (reverse equipment-sets-by-cost))]
                     #:when (let ([player (equipment-set->player equip)])
                              (not (we-win? player boss))))
                    (cost equip)))
             
       ]



@section{Testing Day 21}

@chunk[<day21-test>
       (module+ test
         (define input-str (file->string "day21-input.txt"))
         (check-equal? (q1 input-str) 111)
         (check-equal? (q2 input-str) 188))]


