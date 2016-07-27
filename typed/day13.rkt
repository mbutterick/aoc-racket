#lang typed/racket
(require typed/rackunit trivial/regexp/no-colon)
(provide (all-defined-out))

(: happiness-scores (HashTable (List String String) Integer))
(define happiness-scores (make-hash))

(: parse-happiness-score (-> String Void))
(define (parse-happiness-score ln)
  (define result
    (regexp-match #px"^(.*?) would (gain|lose) (\\d+) happiness units by sitting next to (.*?)\\.$" (string-downcase ln)))
  (when result
    (match-define (list _ name1 op amount name2) result)
    (hash-set! happiness-scores (list name1 name2)
               ((if (equal? op "gain") + -) (cast (string->number amount) Natural)))))

(: calculate-happiness (-> (Listof String) Integer))
(define (calculate-happiness table-arrangement)
  (define table-arrangement-rotated-one-place
    (append (drop table-arrangement 1) (take table-arrangement 1)))
  (define clockwise-duos
    (for/list : (Listof (List String String))
              ([left table-arrangement]
               [right table-arrangement-rotated-one-place])
      (list left right)))
    ;bg;(map list table-arrangement table-arrangement-rotated-one-place)
  (define counterclockwise-duos
    (for/list : (Listof (List String String))
              ([lr (in-list clockwise-duos)])
      (list (cadr lr) (car lr))))
    ;bg;(map (ann reverse (-> (List String String) (List String String))) clockwise-duos)
  (define all-duos (append clockwise-duos counterclockwise-duos))
  (for/sum : Integer ([duo : (List String String) (in-list all-duos)])
           (hash-ref happiness-scores duo)))

;;bg
(: flatten2 (-> (Listof (List String String)) (Listof String)))
(define (flatten2 xss)
  (for*/list : (Listof String)
             ([xs (in-list xss)]
              [x (in-list xs)])
    x))

(: q1 (-> String Integer))
(define (q1 input-str)
  (for-each parse-happiness-score (string-split input-str "\n"))
  (define names : (Listof String)
    (remove-duplicates (flatten2 (hash-keys happiness-scores))))
  (define table-arrangement-scores
    (for/list : (Listof Integer) ([partial-table-arrangement (in-permutations (cdr names))])
              (define table-arrangement (cons (car names) partial-table-arrangement))
              (calculate-happiness table-arrangement)))
  (apply max table-arrangement-scores))


(: q2 (-> String Integer))
(define (q2 input-str)
  (define names
    (remove-duplicates (flatten2 (hash-keys happiness-scores))))
  (for* ([name (in-list names)]
         [duo-proc : (-> String String (List String String)) (in-list (list (λ ([x : String] [y : String]) (list x y)) (λ ([x : String] [y : String]) (list y x))))]) ;;bg
        (hash-set! happiness-scores (duo-proc "me" name) 0))
  (define table-arrangement-scores
    (for/list : (Listof Integer)
              ([partial-table-arrangement (in-permutations names)])
              (define table-arrangement (cons "me" partial-table-arrangement))
              (calculate-happiness table-arrangement)))
  (apply max table-arrangement-scores))

(module+ test
  (define input-str (file->string "../day13-input.txt"))
  (check-equal? (q1 input-str) 709)
  (check-equal? (q2 input-str) 668))


