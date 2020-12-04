#lang br
(require racket/file rackunit)

(struct rec (password target low high))
(define recs (for/list ([ln (file->lines "02.rktd")])
               (match-define (list range target password) (string-split ln))
               (match-define (list low high) (map string->number (string-split range "-")))
               (rec password (string-ref (string-trim target ":") 0) low high)))
               
(check-equal?
 (for/sum ([rec recs])
   (define howmany (count (λ (c) (char=? c (rec-target rec))) (string->list (rec-password rec))))
   (if (<= (rec-low rec) howmany (rec-high rec)) 1 0))
 422)

(check-equal?
 (for/sum ([rec recs])
   (define low? (char=? (rec-target rec) (string-ref (rec-password rec) (sub1 (rec-low rec)))))
   (define high? (char=? (rec-target rec) (string-ref (rec-password rec) (sub1 (rec-high rec)))))
   (if (or (and low? (not high?)) (and high? (not low?))) 1 0))
 451)