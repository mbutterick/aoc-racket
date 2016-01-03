#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[14]

@defmodule[aoc-racket/day14]

@link["http://adventofcode.com/day/14"]{The puzzle}. Our @link-rp["day14-input.txt"]{input} is a list of flying-reindeer descriptions — in particular, how fast they can fly, and how long they have to rest between flight sessions, e.g., @italic{Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.}

@chunk[<day14>
       <day14-setup>
       <day14-q1>
       <day14-q2>
       <day14-test>]

@section{After 2503 seconds, what is the maximum distance any reindeer has flown?}

For each reindeer, we have a description that specifies a) flight speed, b) flight time, and c) rest time. Thus, if we have the total flight time as input — and we do — we can use it to calculate flight distance. In other words, just as we made functions out of wire descriptions on @secref{Day_7}, now we'll make functions out of reindeer descriptions.

As in @secref{Day_7}, we'll use @racket[define-syntax] to set up the reindeer functions. Each reindeer function will take a time in seconds and then use @racket[calc-distance] to convert it into meters. After that, all we need to do is @racket[map] the time over the reindeer and @racket[apply max] to get the answer.



@chunk[<day14-setup>
       (require racket rackunit (for-syntax racket/file))
       (provide (all-defined-out))
       
       (define-syntax (convert-input-to-reindeer-functions stx)
         (syntax-case stx ()
           [(_)
            (let* ([input-strings (file->lines "day14-input.txt")]
                   [reindeer-strings
                    (map (λ(str) (format "(reindeer ~a)" (string-downcase str))) input-strings)]
                   [reindeer-datums
                    (map (compose1 read open-input-string) reindeer-strings)])
              (datum->syntax stx `(begin ,@reindeer-datums)))]))
       
       (define-syntax (reindeer stx)
         (syntax-case stx (can fly seconds but then must rest for)
           [(_ deer-name can fly speed km/s for fly-secs seconds, but then must rest for rest-secs seconds.)
            #'(define (deer-name total-secs)
                (calc-distance total-secs speed fly-secs rest-secs))]
           [else #'(void)]))
       
       (convert-input-to-reindeer-functions)
       
       (define (calc-distance total-secs speed fly-secs rest-secs)
         (let loop ([secs-remaining total-secs][distance 0])
           (if (<= secs-remaining 0)
               distance
               (let ([secs-in-flight (min secs-remaining fly-secs)])
                 (loop (- secs-remaining fly-secs rest-secs)
                       (+ (* secs-in-flight speed) distance))))))
                                                                 
       ]

@chunk[<day14-q1>
       (define (q1)
         (define seconds-to-travel 2503)
         (apply max (map (λ(deer-func) (deer-func seconds-to-travel))
                         (list dasher dancer prancer vixen comet
                               cupid donner blitzen rudolph))))]



@section{Under the new rule, how many points does the winning reindeer have?}

The new rule is that after each second of travel, the reindeer in the lead gets one point. Thus, the winner after 2503 seconds is not the reindeer that has traveled farthest, but that has gathered the most points — in other words, has been in the lead for the longest time.

This question is similar to the last. But instead of simulating one race, we have to simulate 2503 races, each one ending a second later than the last. After each second, we calculate the winning reindeer, and add it to our list of winners. After 2503 seconds, we find out how many times the winningest reindeer appears on the list. To do this, we'll use the helper function @racket[frequency-hash] from @racketmodname[sugar/list]. (You could also do it with @racket[for/fold], but controlling nine reindeer in parallel is unwieldy. Just ask Santa.)

@chunk[<day14-q2>
       
       (require sugar/list)
       (define (q2)
         (define deer-funcs (list dasher dancer prancer vixen comet
                                  cupid donner blitzen rudolph))
         (define winners
           (frequency-hash
            (flatten
             (for/list ([sec (in-range 1 (add1 2503))])
                       (define results
                         (map (λ(deer-func) (deer-func sec)) deer-funcs))
                       (define max-result (apply max results))
                       (filter procedure?
                               (map (λ(result deer-func)
                                      (and (= result max-result) deer-func))
                                    results deer-funcs))))))
         
         (apply max (hash-values winners)))
                                           
       ]



@section{Testing Day 14}

@chunk[<day14-test>
       (module+ test
         (define input-str (file->string "day14-input.txt"))
         (check-equal? (q1) 2640)
         (check-equal? (q2) 1102))]


