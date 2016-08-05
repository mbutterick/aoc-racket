#lang typed/racket
       (require typed/rackunit)
       (provide (all-defined-out))

       (: powerset (All (A) (-> (Listof A) (Listof (Listof A)))))
       (define (powerset xs)
         (if (empty? xs)
             (list empty)
             (append-map
              (λ((s : (Listof A))) (list (cons (car xs) s) s))
              (powerset (cdr xs)))))

       (: string->integer (-> String Integer))
       (define (string->integer s)
         (cast (string->number s) Integer))

       (: q1 (-> String Integer))
       (define (q1 input-str)
         (define containers
           (map string->integer (string-split input-str)))
         (length (filter (λ((s : (Listof Integer))) (= 150 (apply + s)))
                         (powerset containers))))

       (: q2 (-> String Integer))
       (define (q2 input-str)
         (define containers
           (map string->integer (string-split input-str)))
         (let* ([winners (filter (λ((s : (Listof Integer))) (= 150 (apply + s)))
                                 (powerset containers))]
                [shortest (apply min (map (inst length Any) winners))]) ;;bg; why
           (length (filter (λ((w : (Listof Any))) (= shortest (length w))) winners))))

       (module+ test
         (define input-str (file->string "../day17-input.txt"))
         (check-equal? (q1 input-str) 1638)
         (check-equal? (q2 input-str) 17))
