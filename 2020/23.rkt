#lang br
(require racket/file rackunit racket/set)

(define ints (map string->number (regexp-match* #rx"." "389547612")))

(define (solve ints [max-int 9] [turns 100] [string-result? #true])

  (define cups (let* ([ints (if (= max-int (apply max ints))
                                ints
                                (append ints (for/list ([i (in-range 1 (add1 max-int))]
                                                        #:unless (memq i ints))
                                               i)))]
                      [cups (make-vector (length ints))])
                 (for ([int (in-list ints)]
                       [next (in-list (append (cdr ints) (list (car ints))))])
                   (vector-set! cups (sub1  int) next))
                 cups))

  (define (cup-next val)
    (vector-ref cups (sub1 val)))

  (define (link-cups! this next)
    (vector-set! cups (sub1 this) next))
  
  (define (next-cups start count)
    (cdr (reverse (foldl (λ (val acc) (cons (cup-next (car acc)) acc))
                         (list start)
                         (range count)))))

  (define min-cup-val (apply min ints))
  (define max-cup-val max-int)

  (for/fold ([current (first ints)]
             #:result (void))
            ([turn (in-range turns)])
    (match-define (list cup1 cup2 cup3 cup4) (next-cups current 4))
    (link-cups! current cup4)
    (define dest (let ([pickup-vals (list cup1 cup2 cup3)])
                   (let loop ([dest (sub1 current)])
                     (cond
                       [(< dest min-cup-val) (loop max-cup-val)]
                       [(memq dest pickup-vals) (loop (sub1 dest))]
                       [else dest]))))
    (link-cups! cup3 (cup-next dest))
    (link-cups! dest cup1)
    (cup-next current))

  (if string-result?
      (string-append* (map ~a (next-cups 1 (sub1 (vector-length cups)))))
      (apply * (next-cups 1 2))))

(check-equal? (solve ints 9 100) "45286397")
(check-equal? (solve ints 1000000 10000000 #f) 836763710)
