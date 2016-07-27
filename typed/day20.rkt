       #lang typed/racket
       (require typed/rackunit (only-in math divisors))
       (provide (all-defined-out))

       (: q1 (-> String (U #f Integer)))
       (define (q1 input-str)
         (define target-gifts (cast (read (open-input-string input-str)) Natural))
         (define gifts-per-elf 10)
         (for/or : (U #f Integer) ([house-number (in-naturals)]
                     #:when (let* ([elves (divisors house-number)]
                                   [elf-gifts
                                    (apply + (map (ann (curry * gifts-per-elf) (-> Natural Natural)) elves))]) 
                              (>= elf-gifts target-gifts)))
                    house-number))


       (: q2 (-> String (U #f Integer)))
       (define (q2 input-str)
         (define target-gifts (cast (read (open-input-string input-str)) Natural))
         (define gifts-per-elf 11)
         (for/or : (U #f Integer) ([house-number (in-naturals)]
                     #:when (let* ([elves (divisors house-number)]
                                   [elves (filter
                                           (λ([e : Integer]) (<= house-number (* 50 e))) elves)]
                                   [elf-gifts
                                    (apply + (map (ann (curry * gifts-per-elf) (-> Natural Natural)) elves))]) 
                              (>= elf-gifts target-gifts)))
                    house-number))

       (module+ test
         (define input-str (file->string "../day20-input.txt"))
         (check-equal? (q1 input-str) 831600)
         (check-equal? (q2 input-str) 884520))


