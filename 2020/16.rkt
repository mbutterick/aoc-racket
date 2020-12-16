#lang br
(require racket/file rackunit)

(match-define (list fields my-ticket other-tickets)
  (string-split (file->string "16.rktd") "\n\n"))

(struct predicate (name func) #:transparent)
(define predicates
  (for/list ([field (string-split fields "\n")])
    (match (regexp-match #px"^(.*?): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" field)
      [(list* _ name numstrs)
       (match-define (list val1 val2 val3 val4) (map string->number numstrs))
       (predicate name (λ (x) (or (<= val1 x val2) (<= val3 x val4))))])))

(define (t2intvec t) (list->vector (map string->number (string-split t ","))))

(check-equal? (for*/sum ([ticket (cdr (string-split other-tickets "\n"))]
                         [intvec (t2intvec ticket)]
                         #:unless (for/or ([pred predicates])
                                    ((predicate-func pred) intvec)))
                intvec) 26988)

(define all-tickets (cdr (string-split other-tickets "\n")))

(define (ticket-valid? ticket)
  (for/and ([intvec (t2intvec ticket)])
    (for/or ([pred predicates])
      ((predicate-func pred) intvec))))

(define valid-tickets (filter ticket-valid? all-tickets))

(define-values (departure-predicates other-predicates)
  (partition (λ (p) (string-prefix? (predicate-name p) "departure")) predicates))

(define winning-idxss
  (let loop ([extra-count 0])
    (define predicates-to-test
      (append departure-predicates (take other-predicates extra-count)))
    (define possible-winning-idxss
      (let ([valid-ticket-vecs (map t2intvec valid-tickets)])
        (for/list ([idxs (in-combinations (range (length predicates))
                                          (length predicates-to-test))]
                   #:when (for/and ([intvec valid-ticket-vecs])
                            (for/and ([idx idxs]
                                      [pred departure-predicates])
                              ((predicate-func pred) (vector-ref intvec idx)))))
          idxs)))
    (match possible-winning-idxss
      [(list winner) (drop-right winner extra-count)]
      [_ (loop (add1 extra-count))])))

;; too many possibilities
winning-idxss

#;(define my-ticket-intvec (t2intvec (cadr (string-split my-ticket "\n"))))
#;(for/product ([idx winning-idxs]
                [pred departure-predicates])
    (vector-ref my-ticket-intvec idx))

