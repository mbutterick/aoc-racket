#lang br
(require racket/file sugar rackunit racket/dict)

(define lines (file->lines "14.rktd"))

(define (template->duos template-str)
  (define cs (string->list template-str))
  (for/list ([c1 cs]
             [c2 (cdr cs)])
            (list c1 c2)))

(define starting-template (car lines))
(define template (frequency-hash (template->duos starting-template)))

(define rule-strs (cddr lines))
(define procs (for/list ([rule-str rule-strs])
                        (match-define (list pat repl) (string-split rule-str " -> "))
                        (define duo (string->list pat))
                        (define replc (string-ref repl 0))
                        (λ (h)
                          (match (hash-ref h duo #f)
                            [#false null]
                            [duo-count
                             (define new-left-duo (list (first duo) replc))
                             (define new-right-duo (list replc (second duo)))
                             (cons
                              (cons duo (- duo-count))
                              (map (λ (new-duo) (cons new-duo duo-count))
                                   (list new-left-duo new-right-duo)))]))))

(define (do-insertion template steps)
  (for/fold ([template template])
            ([step steps])
    (define delta-dict (append-map (λ (proc) (proc template)) procs))
    (define this-template (make-hash))
    (for ([(k v) (in-dict (append (hash->list template) delta-dict))])
         (hash-update! this-template k (λ (val) (+ val v)) 0))
    this-template))

(define (element-counts template)
  (define orig-template-cs (string->list starting-template))
  (define h (make-hash))
  (for* ([(k v) (in-dict template)]
         [c k])
        (hash-update! h c (λ (val) (+ val v)) 0))
  (for-each (λ (c) (hash-update! h c add1)) (list (first orig-template-cs) (last orig-template-cs)))
  (for/hash ([(k v) (in-hash h)])
            (values k (/ v 2))))
  
(define (score template)
  (- (apply max (hash-values template)) (apply min (hash-values template))))

(check-equal? (score (element-counts (do-insertion template 10))) 2703)
(check-equal? (score (element-counts (do-insertion template 40))) 2984946368465)