#lang typed/racket
;;bg; wow this one came out ugly
(require typed/rackunit trivial/regexp/no-colon)
(provide (all-defined-out))

(: parse-sues (-> String (Listof (Listof String))))
(define (parse-sues str)
  (for/list ([ln (in-list (string-split str "\n"))])
            : (Listof (Listof String))
            (define attr-str (second (or (regexp-match #px"^.*?: (.*)$" ln) (error 'bg))))
            (string-split attr-str ", ")))

(define master-attrs (file->lines "../day16-input-master-attrs.txt"))

(: q1 (-> String (U #f Integer)))
(define (q1 input-str)
  (define sues (parse-sues input-str))
  (for/or ([(sue-attrs sue-number) (in-indexed sues)])
          : (U #f Integer)
          (let loop : (U #f Integer) ([sue-attrs sue-attrs] [acc : (U #f Integer) #f])
            (cond
             [(null? sue-attrs)
              acc]
             [(member (car sue-attrs) master-attrs)
              (loop (cdr sue-attrs) (add1 sue-number))]
             [else
              #f]))))
          ;(for/and ([sue-attr (in-list sue-attrs)])
          ;         : (U #f Integer)
          ;         (and (member sue-attr master-attrs) (add1 sue-number)))

(: q2 (-> String (U #f Integer)))
(define (q2 input-str)
  (: attrs->datums (-> (Listof Any) (Listof (Listof Any))))
  (define (attrs->datums attrs)
    (map (λ (attr) (cast (read (open-input-string (format "(~a)" attr))) (Listof Any))) ;;bg;
    #;(compose1 read open-input-string
                   (λ(attr) (format "(~a)" attr))) attrs))
  (define sues (for/list ([sue-attrs (parse-sues input-str)])
                         : (Listof (Listof (Listof Any)))
                         (attrs->datums sue-attrs)))
  (define master-datums : (Listof (List Symbol Index))
    (cast (attrs->datums master-attrs) (Listof (List Symbol Index))))
  (for/or ([(sue-datums sue-number) (in-indexed sues)])
          : (U #f Integer)
          (let loop : (U #f Integer) ([sue-datums : (Listof (Listof Any)) sue-datums] [acc : (U #f Integer) #f])
            (if (null? sue-datums)
              acc
              (let* ([sue-key (caar sue-datums)]
                     [sue-value (cast (cadar sue-datums) Natural)]
                     [master-value (second (or (assoc sue-key master-datums) (error 'bg)))]
                     [cmp (case sue-key
                           [(cats: trees:) >]
                           [(pomeranians: goldfish:) <]
                           [else =])])
                (if (cmp sue-value master-value)
                  (loop (cdr sue-datums) (add1 sue-number))
                  #f))))))
          ;(for/and ([sue-datum (in-list sue-datums)])
          ;         : (U #f Integer)
          ;         (and
          ;          (let* ([sue-key (first sue-datum)]
          ;                 [sue-value (second sue-datum)]
          ;                 [master-value (second (assoc sue-key master-datums))]
          ;                 [cmp (case sue-key
          ;                        [(cats: trees:) >]
          ;                        [(pomeranians: goldfish:) <]
          ;                        [else =])])
          ;            (cmp sue-value master-value))
          ;          (add1 sue-number)))) )

(module+ test
  (define input-str (file->string "../day16-input.txt"))
  (check-equal? (q1 input-str) 103)
  (check-equal? (q2 input-str) 405))


