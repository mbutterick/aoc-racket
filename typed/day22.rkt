#lang typed/racket

(define BASE-HP 50)
(define BASE-MANA 500)

(define MAGIC-MISSILE-DAMAGE 4)
(define DRAIN-DAMAGE 2)
(define SHIELD-ARMOR 7)
(define SHIELD-DURATION 6)
(define POISON-DAMAGE 3)
(define POISON-DURATION 6)
(define RECHARGE-MANA 101)
(define RECHARGE-DURATION 5)

(struct player (
  [hp : Integer]
  [attack : Natural]
  [mana : Integer]
) #:transparent)
(define-type Player player)

(define-type Spell (U 'magic-missile 'drain 'shield 'poison 'recharge))
(define-type Active-Spells (Listof (Pairof Natural Spell)))

;(define-type Game-State (Values Player Player Active-Spells))

(define ALL-SPELLS : (Listof Spell)
  '(magic-missile
    drain
    shield
    poison
    recharge))

(: unknown-spell (All (A) (-> Spell A)))
(define (unknown-spell spell)
  (raise-user-error 'day22 "Unknown spell '~a'" spell))

(: spell-mana (-> Spell Natural))
(define (spell-mana s)
  (case s
   [(magic-missile)
    53]
   [(drain)
    73]
   [(shield)
    113]
   [(poison)
    173]
   [(recharge)
    229]
   [else
    (unknown-spell s)]))

(: spell*-mana (-> (Listof Spell) Natural))
(define (spell*-mana spell*)
  (for/sum : Natural ([spell (in-list spell*)])
    (spell-mana spell)))

(: make-player (-> Player))
(define (make-player)
  (player BASE-HP 0 BASE-MANA))

(: make-boss (-> Index * Player))
(define (make-boss . val*)
  (player (car val*) (cadr val*) 0))

(: hp+ (-> Player Integer Player))
(define (hp+ p val)
  (match-define (player hp attack mana) p)
  (player (+ hp val) attack mana))

(: hp- (-> Player Integer Player))
(define (hp- p val)
  (hp+ p (- val)))

(: mana+ (-> Player Integer Player))
(define (mana+ p val)
  (match-define (player hp attack mana) p)
  (player hp attack (+ mana val)))

(: mana- (-> Player Integer Player))
(define (mana- p val)
  (mana+ p (- val)))

(: active? (-> Spell Active-Spells Boolean))
(define (active? spell active-spells)
  (for/or : Boolean
          ([ctr+spell (in-list active-spells)])
    (eq? spell (cdr ctr+spell))))

(: has-enough-mana? (-> Player Spell Boolean))
(define (has-enough-mana? player spell)
  (<= (spell-mana spell) (player-mana player)))

(: boss-attack (-> Player Player Boolean Player))
(define (boss-attack player boss shield?)
  (define boss-damage (max 1 (- (player-attack boss) (if shield? SHIELD-ARMOR 0))))
  (hp- player boss-damage))

(: apply-effects (-> Player Player (Listof (Pairof Natural Spell)) (Values Player Player Active-Spells)))
(define (apply-effects player boss spells)
  (for/fold ([p+ : Player player]
             [b+ : Player boss]
             [a+ : Active-Spells '()])
            ([ctr+spell (in-list spells)])
    (match-define (cons ctr spell) ctr+spell)
    (define a++ (if (= 1 ctr) a+ (cons (cons (assert (- ctr 1) index?) spell) a+)))
    (case spell
     [(poison)
      (values p+ (hp- b+ POISON-DAMAGE) a++)]
     [(recharge)
      (values (mana+ p+ RECHARGE-MANA) b+ a++)]
     [(shield)
      (values p+ b+ a++)]
     [else
      (unknown-spell spell)])))

(: apply-spell (-> Spell Player Player Active-Spells (Values Player Player Active-Spells)))
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

(: win? (-> Player Player Boolean))
(define (win? player boss)
  (dead? boss))

(: dead? (-> Player Boolean))
(define (dead? player)
  (<= (player-hp player) 0))

(: lose? (-> Player Player Boolean))
(define lose?
  (let ([MIN-MANA (apply min (map spell-mana ALL-SPELLS))])
    (λ (player boss)
      (or (dead? player)
          (< (player-mana player) MIN-MANA)))))

(: win/least-mana : Player Player [#:hard-mode? Boolean] -> (Listof Spell))
(define (win/least-mana player boss #:hard-mode? [hard-mode? #f])
  (or
    (let maybe-win/least-mana : (U #f (Listof Spell))
                              ([player : Player player]
                               [boss : Player boss]
                               [active-spells : Active-Spells '()]
                               [current-turn : Natural 0])
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
              (for/list : (Listof (U #f (Listof Spell)))
                       ([spell (in-list ALL-SPELLS)]
                       #:when (and (not (active? spell a+))
                                   (has-enough-mana? p+ spell)))
                (define-values (p++ b++ a++) (apply-spell spell p+ b+ a+))
                (define future-spells (maybe-win/least-mana p++ b++ a++ next-turn))
                (and future-spells (cons spell future-spells)))))
          (maybe-win/least-mana (boss-attack p+ b+ (active? 'shield a+)) b+ a+ next-turn))]))
      (raise-user-error 'day22 "Impossible for ~a to beat ~a. Sorry.\n" player boss)))

(: minimize-mana (-> (Listof (U #f (Listof Spell))) (U #f (Listof Spell))))
(define (minimize-mana spell**)
  (for/fold : (U #f (Listof Spell))
            ([best : (U #f (Listof Spell)) #f])
            ([other : (U #f (Listof Spell)) (in-list spell**)])
    (if (or (not best)
            (and best other (< (spell*-mana other) (spell*-mana best))))
      other
      best)))

(: q1 (-> String Integer))
(define (q1 input-str)
  (define player (make-player))
  (define boss (apply make-boss (filter index? (map string->number (string-split input-str)))))
  (spell*-mana (win/least-mana player boss)))

(: q2 (-> String Integer))
(define (q2 input-str)
  (define player (make-player))
  (define boss (apply make-boss (filter index? (map string->number (string-split input-str)))))
  (spell*-mana (win/least-mana player boss #:hard-mode? #t)))

(module+ test
  (require typed/rackunit)
  (define input-str (file->string "../day22-input.txt"))
  (check-equal? (q1 input-str) 1269)
  (check-equal? (q2 input-str) 1309))

