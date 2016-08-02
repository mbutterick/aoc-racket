#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[22]

@italic{This solution contributed by guest star @link["https://github.com/bennn"]{Benjamin Greenman}, because I had no patience for this puzzle. — MB}

@link["http://adventofcode.com/day/22"]{The puzzle}. Once again, our @link-rp["day22-input.txt"]{input} tells us the boss's stats.

@chunk[<day22>
       <day22-player>
       <day22-spells>
       <day22-dp>
       <day22-q1>
       <day22-q2>
       <day22-test>]

@section{You're a Wizard, Henry}

The rules of the game are different this time.
Instead of items, attack power, and defense power, we have mana and spells.
The boss still has a fixed attack power, so we'll use a 3-element @racket[struct]
 to model the player and boss.

@chunk[<day22-player>
       (require racket rackunit)
       (provide (all-defined-out))

       (define BASE-HP 50)
       (define BASE-MANA 500)

       (struct player (hp attack mana) #:transparent)

       (define (make-player)
         (player BASE-HP 0 BASE-MANA))

       (define (make-boss hp attack)
         (player hp attack 0))

       (define (hp+ p val)
         (match-define (player hp attack mana) p)
         (player (+ hp val) attack mana))

       (define (hp- p val)
         (hp+ p (- val)))

       (define (mana+ p val)
         (match-define (player hp attack mana) p)
         (player hp attack (+ mana val)))

       (define (mana- p val)
         (mana+ p (- val)))

]

Next, a few constants and helper functions to model spells.
The key functions are @racket[apply-spell], which represents the actions a player
 can take during their turn, and @racket[apply-effect], which implements the
 delayed action of the @racket[shield], @racket[poison], and @racket[recharge] spells.
Both functions are parameterized over the player, boss, and current active
 spells.
These three values represent the game state at any moment.

@chunk[<day22-spells>
       (define MAGIC-MISSILE-DAMAGE 4)
       (define DRAIN-DAMAGE 2)
       (define SHIELD-ARMOR 7)
       (define SHIELD-DURATION 6)
       (define POISON-DAMAGE 3)
       (define POISON-DURATION 6)
       (define RECHARGE-MANA 101)
       (define RECHARGE-DURATION 5)

       (define ALL-SPELLS
         '(magic-missile drain shield poison recharge))

       (define (unknown-spell spell)
         (raise-user-error 'day22 "Unknown spell '~a'" spell))

       (define (spell-mana s)
         (case s
          [(magic-missile) 53]
          [(drain)         73]
          [(shield)        113]
          [(poison)        173]
          [(recharge)      229]
          [else            (unknown-spell s)]))

       (define (apply-spell spell player0 boss active-spells)
         (define player (mana- player0 (spell-mana spell)))
         (case spell
          [(magic-missile)
           (values player (hp- boss MAGIC-MISSILE-DAMAGE) active-spells)]
          [(drain)
           (values (hp+ player DRAIN-DAMAGE) (hp- boss DRAIN-DAMAGE) active-spells)]
          [(shield)
           (values player boss (cons (cons SHIELD-DURATION 'shield) active-spells))]
          [(poison)
           (values player boss (cons (cons POISON-DURATION 'poison) active-spells))]
          [(recharge)
           (values player boss (cons (cons RECHARGE-DURATION 'recharge) active-spells))]
          [else
           (unknown-spell spell)]))

       (define (apply-effects player boss active-spells)
         (for/fold ([p+ player]
                    [b+ boss]
                    [a+ '()])
                   ([ctr+spell (in-list active-spells)])
           (match-define (cons ctr spell) ctr+spell)
           (define a++ (if (= 1 ctr) a+ (cons (cons (- ctr 1) spell) a+)))
           (case spell
            [(poison)
             (values p+ (hp- b+ POISON-DAMAGE) a++)]
            [(recharge)
             (values (mana+ p+ RECHARGE-MANA) b+ a++)]
            [(shield)
             (values p+ b+ a++)]
            [else
             (unknown-spell spell)])))
]

@section{What's the least (mana) we can spend and win?}

Starting with 50 health, 500 mana and a boss with @math{N} hit points and @math{A}
 attack points, the least mana we can spend and win is either:
@itemize[
  @item{
    The cost of one @emph{Magic Missile}, plus the least-mana solution for
     a player with @math{50 - M} hit points, @math{500 - 53} mana,
     and a boss with @math{N - 4} hit points.
  }
  @item{
    The cost of one @emph{Drain} spell, plus the least-mana solution for
     a player with @math{50 + 2} hit points, @math{500 - 73} mana, and
     a boss with @math{N - 2} hit points.
  }
  @item{
    The cost of one @emph{Shield} spell, plus the least-mana solution for:
    @itemize[
      @item{
        a player with @math{50 - max(1, (M - 7))} hit points and @math{500 - 113} mana,
      }
      @item{
        a boss with @math{N} hit points,
      }
      @item{
        the @emph{Shield} spell active for 4 more turns.
      }
    ]
  }
  @item{
    The cost of one @emph{Poison} spell, plus the least-mana solution for
     a player with @math{50 - 7} hit points and @math{500 - 173} mana and
     a boss with @math{N - 6} hit points, given that the @emph{Poison} spell
     is active for 4 more turns.
  }
  @item{
    The cost of one @emph{Recharge} spell, plus the least-mana solution for
    a player with @math{50 - 7} hit points and @math{500 - 229 + (101 * 2)} mana
    against a boss with @math{N} hit points, given that the @emph{Recharge} spell
    is active for 3 more turns.
  }
]
The correct alternative is the one that happens to be cheapest.
Since we've outlined the alternatives and know the end conditions for the game,
 we can write an algorithm that tries every alternative and returns the best
 sequence of spells.

Never mind the @racket[hard-mode?] parameter for now.

@chunk[<day22-dp>
       (define (win/least-mana player boss #:hard-mode? [hard-mode? #f])
         (or
           (let maybe-win/least-mana ([player player]
                                      [boss boss]
                                      [active-spells '()]
                                      [current-turn 0])
             (cond
              [(lose? player boss)
               #f]
              [(win? player boss)
               '()]
              [else
               (define-values (p+ b+ a+) (apply-effects player boss active-spells))
               (define next-turn (+ 1 current-turn))
               (if (even? current-turn)
                 (let ([p+ (if hard-mode? (hp- p+ 1) p+)])
                   (minimize-mana
                     (for/list ([spell (in-list ALL-SPELLS)]
                                #:when (and (not (active? spell a+))
                                            (has-enough-mana? p+ spell)))
                       (define-values (p++ b++ a++) (apply-spell spell p+ b+ a+))
                       (define future-spells (maybe-win/least-mana p++ b++ a++ next-turn))
                       (and future-spells (cons spell future-spells)))))
                 (maybe-win/least-mana (boss-attack p+ b+ (active? 'shield a+)) b+ a+ next-turn))]))
             (raise-user-error 'day22 "Impossible for ~a to beat ~a. Sorry.\n" player boss)))

       (define (win? player boss)
         (dead? boss))

       (define (dead? player)
         (<= (player-hp player) 0))

       (define lose?
         (let ([MIN-MANA (apply min (map spell-mana ALL-SPELLS))])
           (λ (player boss)
             (or (dead? player)
                 (< (player-mana player) MIN-MANA)))))

       (define (boss-attack player boss shield?)
         (define boss-damage (max 1 (- (player-attack boss) (if shield? SHIELD-ARMOR 0))))
         (hp- player boss-damage))

       (define (active? spell active-spells)
         (for/or ([ctr+spell (in-list active-spells)])
           (eq? spell (cdr ctr+spell))))

       (define (has-enough-mana? player spell)
         (<= (spell-mana spell) (player-mana player)))

       (define (spell*-mana spell*)
         (for/sum ([spell (in-list spell*)])
           (spell-mana spell)))

       (define (minimize-mana spell**)
         (for/fold ([best #f])
                   ([other (in-list spell**)])
           (if (or (not best)
                   (and best other (< (spell*-mana other) (spell*-mana best))))
             other
             best)))
]

@chunk[<day22-q1>
       (define (q1 input-str)
         (define player (make-player))
         (define boss (apply make-boss (filter integer? (map string->number (string-split input-str)))))
         (spell*-mana (win/least-mana player boss)))
]

@section{Hard Mode}

On hard mode, the player loses one hit point on their turn.
Thanks to the optional keyword argument to @racket[win/least-mana], the answer
 is easy find.

@chunk[<day22-q2>
       (define (q2 input-str)
         (define player (make-player))
         (define boss (apply make-boss (filter integer? (map string->number (string-split input-str)))))
         (spell*-mana (win/least-mana player boss #:hard-mode? #t)))]

@section{Testing Day 22}

The algorithm as written is very slow.
Unfortunately, there's no better way to solve this problem than trying everything,
 but while the tests are running, think about what kind of caching strategy could
 improve performance.

@chunk[<day22-test>
       (module+ test
         (define input-str (file->string "day22-input.txt"))
         (check-equal? (q1 input-str) 1269)
         (check-equal? (q2 input-str) 1309))]
