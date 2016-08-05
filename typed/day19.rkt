#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))
(require/typed racket/function ;;bg
  (curryr (-> (-> String String (Listof String)) String (-> String (Listof String)))))

(: parse-input-str (-> String (Values String (Listof (Listof String)))))
(define (parse-input-str input-str)
  (match-define (cons molecule transformation-strings)
    (reverse (string-split input-str "\n")))
  (define transformations
    (filter-not empty?
                (map (curryr string-split " => ")
                     transformation-strings)))
  (values molecule transformations))

 (: transform-molecule* (-> String String String (Listof String)))
(define (transform-molecule* molecule target-atom replacement-atom)
  (for/list ([pos (in-list (ann (or (regexp-match-positions* (regexp target-atom) molecule) (error 'bg)) (Listof (U #f (Pairof Integer Integer)))))])
            : (Listof String)
            (match-define (cons start finish) (if (pair? pos) pos (error 'bg)))
            (string-append (substring molecule 0 start)
                           replacement-atom
                           (substring molecule finish (string-length molecule)))))

(: q1 (-> String Natural))
(define (q1 input-str)
  (define-values (molecule transformations) (parse-input-str input-str))
  (length
   (remove-duplicates
    ((inst append-map String String String) (λ([target : String] [replacement : String])
                  (transform-molecule* molecule target replacement))
                (map (inst first String String) transformations)
                (map (inst second String String String) transformations)))))


(: q2 (-> String Integer))
(define (q2 input-str)
  (define-values (starting-molecule xforms) (parse-input-str input-str))
  (let loop : Integer ([current-mol starting-molecule][transform-count 0]
                                            [shuffles 0][xforms xforms])
    (cond
      [(equal? current-mol "e") transform-count]
      [else
       (define-values (xformed-mol last-count)
         (for/fold : (Values String Integer)
                   ([mol current-mol][count-so-far transform-count])
                   ([(from to) (in-parallel (map (inst first String String) xforms) (map (inst second String String String) xforms))])
           (values (string-replace mol to from)
                   (+ count-so-far (length (regexp-match* to mol))))))
       (if (not (equal? current-mol xformed-mol))
           (loop xformed-mol last-count shuffles xforms)
           (loop starting-molecule 0 (add1 shuffles) (shuffle xforms)))])))

(module+ test
  (define input-str (file->string "../day19-input.txt"))
  (check-equal? (q1 input-str) 576)
  (check-equal? (q2 input-str) 207))

