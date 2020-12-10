#lang br
(require racket/file rackunit)

(define adapters (map string->number (file->lines "10.rktd")))
(define sources (sort `(0 ,@adapters ,(+ (apply max adapters) 3)) <))

(define diffs (for/list ([left sources]
                         [right (cdr sources)])
                (- right left)))

(check-equal? (* (count (curry = 1) diffs) (count (curry = 3) diffs)) 2590)

(define (sequential-subsequences sources [seqs null])
  (match sources
    [(? null?) seqs]
    [(list head tail ...)
     (let loop ([head (list head)][tail tail])
       (match tail
         [(list first rest ...) #:when (eq? (car head) (sub1 first))
          (loop (cons first head) rest)]
         [_ (sequential-subsequences tail (cons head seqs))]))]))

(define (subseq->multiplier subseq)
  (match (length subseq)
    [3 2] ; abc | ac
    [4 4] ; abcd | acd | abd | ad
    [5 7] ; abcde | acde | abde | abce | abe | ace | ade
    [_ 1]))

(check-equal? (for/product ([subseq (sequential-subsequences sources)])
                (subseq->multiplier subseq)) 226775649501184)
