#lang typed/racket
       (require typed/rackunit)
       (provide (all-defined-out))

       (: str->ingredient-hash (-> String (HashTable String (Listof Integer))))
       (define (str->ingredient-hash str)
         (for/hash : (HashTable String (Listof Integer))
                   ([ln (in-list (string-split (string-replace str "," " ") "\n"))])
                   (match-define (list ingredient-name characteristic-string)
                     (string-split ln ":"))
                   (values ingredient-name
                           (filter exact-integer? ;;bg;number?
                                   (map string->number
                                        (string-split characteristic-string))))))

       (: make-recipes (-> Integer Integer (Listof (Listof Integer))))
       (define (make-recipes how-many-ingredients total-tsps)
         (cond
           [(= 0 how-many-ingredients) empty]
           [(= 1 how-many-ingredients) (list (list total-tsps))]
           [else
            (append*
             (for/list ([first-amount (in-range (add1 total-tsps))])
                       : (Listof (Listof (Listof Integer)))
                       (map (λ ([x : (Listof Integer)]) (cons first-amount x)) ;(curry cons first-amount)
                            (make-recipes (sub1 how-many-ingredients)
                                          (- total-tsps first-amount)))))]))

       (: q1 (-> String Integer))
       (define (q1 input-str)
         (define ingredient-hash (str->ingredient-hash input-str))
         (define ingredients (hash-keys ingredient-hash))
         (define how-many-characteristics (length (car (hash-values ingredient-hash))))
         (define tsps 100)
         (define scores
           (for/list ([recipe (in-list (make-recipes (length ingredients) tsps))])
                     : (Listof Integer)
                     (for/product : Integer ([char-idx (in-range (sub1 how-many-characteristics))])
                                  (max 0 (for/sum : Integer
                                                  ([tsp-quantity (in-list recipe)]
                                                   [ingredient (in-list ingredients)])
                                                  (* tsp-quantity
                                                     (list-ref (hash-ref ingredient-hash ingredient) char-idx)))))))
         (apply max scores))

       (: q2 (-> String Integer))
       (define (q2 input-str)
         (define ingredient-hash (str->ingredient-hash input-str))
         (define ingredients (hash-keys ingredient-hash))
         (define how-many-characteristics (length (car (hash-values ingredient-hash))))
         (define tsps 100)
         (: recipe->calories (-> (Listof Integer) Integer))
         (define (recipe->calories recipe)
           (for/sum : Integer ([tsp-quantity (in-list recipe)]
                     [ingredient (in-list ingredients)])
                    (* tsp-quantity (last (hash-ref ingredient-hash ingredient (λ () (error 'bg)))))))
         (define scores
           (for/list : (Listof Integer) ([recipe (in-list (make-recipes (length ingredients) tsps))]
                      #:when (= 500 (recipe->calories recipe)))
                     (for/product : Integer ([char-idx (in-range (sub1 how-many-characteristics))])
                                  (max 0 (for/sum : Integer ([tsp-quantity (in-list recipe)]
                                                   [ingredient (in-list ingredients)])
                                                  (* tsp-quantity
                                                     (list-ref (hash-ref ingredient-hash ingredient) char-idx)))))))
         (apply max scores))

       (module+ test
         (define input-str (file->string "../day15-input.txt"))
         (check-equal? (q1 input-str) 18965440)
         (check-equal? (q2 input-str) 15862900))


