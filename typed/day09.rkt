#lang typed/racket
(require typed/rackunit trivial/regexp/no-colon trivial/list/no-colon)
(provide (all-defined-out))

(: distances (HashTable (List String String) Integer))
(define distances (make-hash))

(: str->hash (-> String Void))
(define (str->hash ln)
  (match-define (list _ here there dist)
    (regexp-match #px"^(\\w+) to (\\w+) = (\\d+)" ln))
  (define key (places->key here there))
  (hash-set! distances key (cast (string->number dist) Natural)))

(: places->key (-> String String (List String String)))
(define (places->key here there)
  (define x (sort (list (string-downcase here) (string-downcase there)) string<?))
  (list (car x) (cadr x)))

(: inf Integer)
(define inf 999999999999999999999)

(: calculate-route-distances (-> (Listof Integer)))
(define (calculate-route-distances)
  (: pairify (-> (Listof String) (Listof (List String String))))
  (define (pairify xs)
    ;;bg: replaced map with for/list
    (for/list ([left (in-list (drop-right xs 1))]
               [right (in-list (drop xs 1))])
              : (Listof (List String String))
      (list left right)))
  (: distance (-> String String Integer))
  (define (distance here there)
    (hash-ref distances (places->key here there) (λ () inf)))
  (define cities (remove-duplicates (append* (hash-keys distances))))
  (for/list ([route (in-permutations cities)])
            (for/sum ([pair (in-list (pairify route))])
                     : Integer
                     (apply distance pair))))

(: q1 (-> (Listof String) Integer))
(define (q1 strs)
  (for-each str->hash strs)
  (apply min (calculate-route-distances)))


(: q2 (-> (Listof String) Integer))
(define (q2 strs)
  (apply max (calculate-route-distances)))


(module+ test
  (define input-strs (file->lines "../day09-input.txt"))
  (check-equal? (q1 input-strs) 251)
  (check-equal? (q2 input-strs) 898))


