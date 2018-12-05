#lang debug br
(require sugar/list gregor)

(define fp (open-input-file "04.txt"))

(define (parse-rec ln)
  (match (regexp-match #px"^(\\[.*?\\]) (.*)$" ln)
    [(list _ datestr desc) (cons
                            (parse-datetime datestr "[yyyy-MM-dd HH:mm]")
                            desc)]))

(define recs
  (map parse-rec (port->lines fp)))

(define (make-interval-hash recs)
  (define intervals (make-hasheqv))
  (for/fold ([guard #f]
             [recs (sort recs datetime<? #:key car)]
             #:result intervals)
            ([i (in-naturals)]
             #:break (empty? recs))
    (match (car recs)
      [(cons dt "falls asleep")
       (define other-dt (car (second recs)))
       (define mins (range (->minutes dt) (->minutes other-dt)))
       (hash-update! intervals guard (λ (val) (append mins val)) null)
       (values guard (drop recs 2))]
      [(cons _ (regexp #px"\\d+" m)) (values (string->number (car m)) (cdr recs))])))

(define (sleepiest h)
  (argmax (compose1 length cdr) (hash->list h))) 

(define (most-common-minute gms)
  (argmax cdr (hash->list (frequency-hash gms))))

(define h (make-interval-hash recs))
(define (★)
  (match (sleepiest h)
    [(cons guard minutes)
     (match (most-common-minute minutes)
       [(cons min _) (* guard min)])]))

(define (★★)
  (define guard-min-freqs
    (for/list ([(k v) (in-hash h)])
      (match-define (cons min freq) (most-common-minute v))
      (list k min freq)))
  (apply * (take (argmax third guard-min-freqs) 2)))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 99759)
  (check-equal? (time (★★)) 97884))