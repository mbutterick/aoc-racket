#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(: look-and-say (-> Natural String String))
(define (look-and-say iterations input-key)
   (for/fold : String
             ([start input-key])
             ([i (in-range iterations)])
     (define digit-runs (regexp-match* #px"(\\d)\\1*" start))
     (string-append*
      (map (ann ~a (-> Any String))
           (append-map (λ([digit-run : String])
                         (list (string-length digit-run)
                               (substring digit-run 0 1)))
                       digit-runs)))))

(: q1 (-> String Natural))
(define (q1 input-key)
  (string-length (look-and-say 40 input-key)))

(: q2 (-> String Natural))
(define (q2 input-key)
  (string-length (look-and-say 50 input-key)))


(module+ test
  (define input-key (file->string "../day10-input.txt"))
  (check-equal? (q1 input-key) 492982)
  (check-equal? (q2 input-key) 6989950))


