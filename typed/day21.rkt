#lang typed/racket

(define-type Item (List Symbol Natural Natural Natural))

(define-type HP Integer)
(define-type Attack-Power Natural)
(define-type Defense-Power Natural)
(define-type Player (List HP Attack-Power Defense-Power))

(define no-item : Item
  '(None           0     0       0))

(define weapons : (Listof Item)
  '((Dagger        8     4       0)
    (Shortsword   10     5       0)
    (Warhammer    25     6       0)
    (Longsword    40     7       0)
    (Greataxe     74     8       0)))

(define armors : (Listof Item)
  '((Leather      13     0       1)
    (Chainmail    31     0       2)
    (Splintmail   53     0       3)
    (Bandedmail   75     0       4)
    (Platemail   102     0       5)))

(define rings : (Listof Item)
  '((Damage+1    25     1       0)
    (Damage+2    50     2       0)
    (Damage+3   100     3       0)
    (Defense+1   20     0       1)
    (Defense+2   40     0       2)
    (Defense+3   80     0       3)))

(require typed/rackunit)
(provide (all-defined-out))

(: cost (-> (Listof Item) Natural))
(define (cost equipment-set)
  (apply + (map (inst second Any Natural Any) equipment-set)))

(: equipment-sets-by-cost (Listof (Listof Item)))
(define equipment-sets-by-cost
  (let ([equipment-sets
         (for*/list ([weapon (in-list weapons)]
                     [armor (in-list (cons no-item armors))]
                     [lh-ring (in-list (cons no-item rings))]
                     [rh-ring (in-list (cons no-item (remove lh-ring rings)))])
                    : (Listof (List Item Item Item Item))
                    (list weapon armor lh-ring rh-ring))])
    ((inst sort (Listof Item) Natural) equipment-sets < #:key cost)))

(define player-hit-points 100)
(define min-damage 1)

(: equipment-set->player (-> (Listof Item) Player))
(define (equipment-set->player equipment-set)
  (let ([total-damage (apply + (map (inst third Any Any Natural Any) equipment-set))]
        [total-armor (apply + (map (inst fourth Any Any Any Natural Any) equipment-set))])
    (list player-hit-points total-damage total-armor)))
(: player-turn? (-> Integer Boolean))
(define player-turn? even?)
(: hit-points (-> Player HP))
(define hit-points first)
(: damage (-> Player Attack-Power))
(define damage second)
(: armor (-> Player Defense-Power))
(define armor third)

(: attack (-> Player Player Player))
(define (attack attacker defender)
  (define net-damage (max (- (damage attacker) (armor defender)) min-damage))
  (list (- (hit-points defender) net-damage) (damage defender) (armor defender)))

(: we-win? (-> Player Player Boolean))
(define (we-win? player boss)
  (define-values (last-player-state last-boss-state)
    (let loop : (Values Player Player)
    ;;bg;(for/fold : (Values Player Player)
              ([player-state : Player player][boss-state : Player boss]
               [turn-number : Natural 0]
               ;;bg; #:break 
                       )
      (cond
       [(<= (min (hit-points player-state) (hit-points boss-state)) 0)
        (values player-state boss-state)]
       [(player-turn? turn-number)
        (loop player-state (player-state . attack . boss-state) (+ 1 turn-number))]
       [else
        (loop (boss-state . attack . player-state) boss-state (+ 1 turn-number))])))
  (<= (hit-points last-boss-state) 0))

(: q1 (-> String (U #f Integer)))
(define (q1 input-str)
  (define boss (cast (filter number? (map string->number (string-split input-str))) Player))
  (for/or : (U #f Integer) ([equip (in-list equipment-sets-by-cost)]
              #:when (let ([player (equipment-set->player equip)])
                       (we-win? player boss)))
             (cost equip)))

(: q2 (-> String (U #f Integer)))
(define (q2 input-str)
  (define boss (cast (filter number? (map string->number (string-split input-str))) Player))
  (for/or : (U #f Integer) ([equip (in-list (reverse equipment-sets-by-cost))]
              #:when (let ([player (equipment-set->player equip)])
                       (not (we-win? player boss))))
             (cost equip)))

(module+ test
  (define input-str (file->string "../day21-input.txt"))
  (check-equal? (q1 input-str) 111)
  (check-equal? (q2 input-str) 188))


