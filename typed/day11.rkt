#lang typed/racket
(require typed/rackunit trivial/regexp/no-colon)
(provide (all-defined-out))

(: increment-password (-> String String))
(define (increment-password password)
  (: increment-letter (-> String String))
  (define (increment-letter c)
    (~a (integer->char (add1 (char->integer (car (string->list c)))))))
    ;bg;((compose1 ~a integer->char add1 char->integer car string->list) c)
  (match-define (list _ prefix letter-to-increment trailing-zs)
    (regexp-match #rx"^(.*?)(.)(z*)$" password))
  (string-append* (list prefix (increment-letter letter-to-increment)
                        (regexp-replace* #rx"z" trailing-zs "a"))))

(: three-consecutive-letters? (-> String Boolean))
(define (three-consecutive-letters? str)
  (define ints (map char->integer (string->list str)))
  (let loop : Boolean ([differences (map - (cdr ints) (drop-right ints 1))])
    (if (empty? differences)
        #f
        (or (list-prefix? '(1 1) differences) (loop (cdr differences))))))

(: no-iol? (-> String Boolean))
(define (no-iol? str)
  (not (regexp-match #rx"[iol]" str)))
(: two-nonoverlapping-doubles? (-> String Boolean))
(define (two-nonoverlapping-doubles? str)
  (regexp-match? #px"(\\w)\\1.*?(\\w)\\2" str)) ;;bg: add ?

(: valid? (-> String Boolean))
(define (valid? password)
  (and (three-consecutive-letters? password)
       (no-iol? password)
       (two-nonoverlapping-doubles? password)))
(: find-next-valid-password (-> String String))
(define (find-next-valid-password starting-password)
  (define candidate-pw (increment-password starting-password))
  (if (valid? candidate-pw)
      candidate-pw
      (find-next-valid-password candidate-pw)))

(: q1 (-> String String))
(define (q1 input-key)
  (find-next-valid-password input-key))

(: q2 (-> String String))
(define (q2 input-key)
  (find-next-valid-password (q1 input-key)))

(module+ test
  (define input-key (file->string "../day11-input.txt"))
  (check-equal? (q1 input-key) "hxbxxyzz")
  (check-equal? (q2 input-key) "hxcaabcc"))


