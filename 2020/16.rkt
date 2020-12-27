#lang br
(require racket/file rackunit csp racket/set racket/dict)

(match-define (list fields my-ticket other-tickets)
  (string-split (file->string "16.rktd") "\n\n"))

(struct predicate (name func) #:transparent)
(define predicates
  (for/list ([field (string-split fields "\n")])
    (match (regexp-match #px"^(.*?): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" field)
      [(list* _ name numstrs)
       (match-define (list val1 val2 val3 val4) (map string->number numstrs))
       (predicate name (λ (x)
                         (let ([h (make-hasheq)])
                           (hash-ref! h x
                                      (λ ()
                                        (or (<= val1 x val2) (<= val3 x val4)))))))])))

(define (ticket->ints t) (map string->number (string-split t ",")))

(check-equal? (for*/sum ([ticket (cdr (string-split other-tickets "\n"))]
                         [ticket-int (ticket->ints ticket)]
                         #:unless (for/or ([pred predicates])
                                    ((predicate-func pred) ticket-int)))
                ticket-int) 26988)

(define all-tickets (cdr (string-split other-tickets "\n")))

(define (ticket-valid? ticket)
  (for/and ([intvec (ticket->ints ticket)])
    (for/or ([pred predicates])
      ((predicate-func pred) intvec))))

(define valid-tickets (filter ticket-valid? all-tickets))

(define cols (apply map list (map ticket->ints valid-tickets)))

(define col-preds (make-hasheq))
(for ([(col colidx) (in-indexed cols)])
  (hash-set! col-preds colidx
             (apply mutable-set (for/list ([(pred predidx) (in-indexed predicates)]
                                           #:when (andmap (predicate-func pred) col))
                                  predidx))))

(define assignment (make-hasheq))

(let loop ()
  (define unique-pairs (for/list ([(colidx predidx) (in-dict (hash->list col-preds))]
                                  #:when (eq? (set-count predidx) 1))
                         (cons colidx (car (set->list predidx)))))
  (when (pair? unique-pairs)
    (for ([(colidx predidx) (in-dict unique-pairs)])
      (hash-set! assignment colidx predidx)
      (hash-remove! col-preds colidx)
      (for ([k (in-hash-keys col-preds)])
        (hash-update! col-preds k (λ (vs) (set-remove! vs predidx) vs))))
    (loop)))

(define (test-assignment assignment)
  (for*/product ([(colidx predidx) (in-dict assignment)]
                 [pred (in-value (list-ref predicates predidx))]
                 #:when (regexp-match "departure" (predicate-name pred)))
    (list-ref (ticket->ints my-ticket) colidx)))

(check-equal? (test-assignment assignment) 426362917709)

;; CSP solution
(require csp)
(define col-pred-prob (make-csp))
(define var-names (make-var-names "col" (range (length cols))))
(add-vars! col-pred-prob var-names (range (length cols)))
(add-pairwise-constraint! col-pred-prob alldiff var-names)
(for ([(colvar colidx) (in-indexed var-names)])
  (add-constraint! col-pred-prob
                   (λ (predidx) (andmap (predicate-func (list-ref predicates predidx))
                                        (list-ref cols colidx))) (list colvar)))

(define csp-assignment
  (parameterize ([current-select-variable mrv-degree-hybrid]
                 [current-order-values shuffle]
                 [current-node-consistency #t])
    (for/list ([(col predidx) (in-dict (solve col-pred-prob))])
      (define colidx (string->number (car (regexp-match #px"\\d+" (~a col)))))
      (cons colidx predidx))))

(check-equal? (test-assignment csp-assignment) 426362917709)