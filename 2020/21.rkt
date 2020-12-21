#lang br
(require racket/file rackunit racket/set)

(struct rec (ingredients allergens) #:transparent)

(define (parse-recs str)
  (for/list ([r (in-list (string-split str "\n"))])
    (match-define (list head tail) (string-split r "(contains "))
    (rec
        (apply set (string-split head))
      (apply set (string-split (string-trim tail ")") ", ")))))

(define recs (parse-recs (file->string "21.rktd")))

(define all-allergens (apply set-union (map rec-allergens recs)))

(define filtered-recs
  (for/list ([allergen (in-set all-allergens)])
    (define recs-with-allergen
      (filter (λ (r) (set-member? (rec-allergens r) allergen)) recs))
    (rec (apply set-intersect (map rec-ingredients recs-with-allergen))
      (set allergen))))

(define allergenic-records
  (let loop ([acc null][filtered-recs filtered-recs])
    (cond
      [(null? filtered-recs) acc]
      [else
       (define-values (singletons others)
         (partition (λ (r) (= 1 (set-count (rec-ingredients r)))) filtered-recs))
       (loop (append singletons acc)
             (for/list ([other (in-list others)])
               (rec (apply set-subtract (rec-ingredients other)
                           (map rec-ingredients singletons))
                 (rec-allergens other))))])))

(define allergenic-ingredients
  (apply set-union (map rec-ingredients allergenic-records)))

(define (hypoallergenic-ingredient-count recs)
  (for/sum ([r (in-list recs)])
    (set-count (set-subtract (rec-ingredients r) allergenic-ingredients))))

(check-equal? (hypoallergenic-ingredient-count recs) 1958)

(define ingredient-list
  (let* ([pairs (for/list ([r (in-list allergenic-records)])
                  (car (map cons (set->list (rec-ingredients r))
                            (set->list (rec-allergens r)))))]
         [pairs (sort pairs #:key cdr string<?)]
         [strs (map car pairs)])
    (string-join strs ",")))

(check-equal? ingredient-list "xxscc,mjmqst,gzxnc,vvqj,trnnvn,gbcjqbm,dllbjr,nckqzsg")