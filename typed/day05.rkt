#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(: nice? (-> String Boolean))
(define (nice? str)
  (define (three-vowels? (str : String))
    (>= (length (regexp-match* #rx"[aeiou]" str)) 3))
  (define (double-letter? (str : String))
    (regexp-match #px"(.)\\1" str))
  (define (no-kapu? (str : String))
    (not (regexp-match #rx"ab|cd|pq|xy" str)))
  (and (three-vowels? str)
       (double-letter? str)
       (no-kapu? str)))

(: q1 (-> (Listof String) Natural))
(define (q1 words)
  (length (filter nice? words)))

(: nicer? (-> String Boolean))
(define (nicer? str)
  (define (nonoverlapping-pair? (str : String))
    (regexp-match #px"(..).*\\1" str))
  (define (separated-repeater? (str : String))
    (regexp-match #px"(.).\\1" str))
  (and (nonoverlapping-pair? str)
       (separated-repeater? str) #t))

(: q2 (-> (Listof String) Natural))
(define (q2 words)
  (length (filter nicer? words)))

(module+ test
  (define input-str (file->lines "../day05-input.txt"))
  (check-equal? (q1 input-str) 238)
  (check-equal? (q2 input-str) 69))
