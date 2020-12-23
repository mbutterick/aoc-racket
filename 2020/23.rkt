#lang br
(require racket/file rackunit racket/set)

(define ints (map string->number (regexp-match* #rx"." "389547612")))

(struct cup (val prev next) #:mutable)

(define (link-cups! left right)
  (set-cup-next! left right)
  (set-cup-prev! right left))

(define (find-cup cups dest-val)
  (for/first ([c (in-list cups)]
              #:when (eq? (cup-val c) dest-val))
    c))

(define (next-cups start count)
  (cdr (reverse (foldl (λ (val acc) (cons (cup-next (car acc)) acc))
                       (list start)
                       (range count)))))

(define (solve ints [max-int 9] [turns 100] [string-result? #true])
  (define cups (let* ([ints (if (= max-int (apply max ints))
                                ints
                                (append ints (for/list ([i (in-range 1 (add1 max-int))]
                                                        #:unless (memq i ints))
                                               i)))]
                      [cups (for/list ([int ints])
                              (cup int #f #f))])
                 (for-each link-cups! cups (append (cdr cups) (list (car cups))))
                 cups))

  (define min-cup-val (apply min ints))
  (define max-cup-val max-int)

  (for/fold ([current (car cups)]
             #:result (void))
            ([turn (in-range turns)])
    (match-define (list cup1 cup2 cup3 cup4) (next-cups current 4))
    (link-cups! current cup4)
    (define dest-cup (let ([pickup-vals (map cup-val (list cup1 cup2 cup3))])
                       (let loop ([dest (sub1 (cup-val current))])
                         (cond
                           [(< dest min-cup-val) (loop max-cup-val)]
                           [(memq dest pickup-vals) (loop (sub1 dest))]
                           [else (find-cup cups dest)]))))
    (define dest-cup-next (cup-next dest-cup))
    (link-cups! dest-cup cup1)
    (link-cups! cup3 dest-cup-next)
    (cup-next current))

  (if string-result?
      (string-append*
       (for/list ([cup (next-cups (find-cup cups 1) (sub1 (length cups)))])
         (~a (cup-val cup))))
      (apply * (map cup-val (next-cups (find-cup cups 1) 2)))))

(check-equal? (solve ints) "45286397")

;; brutality won't be fast enough, but it was worth trying
#;(solve ints 1000000 10000000 #f)
