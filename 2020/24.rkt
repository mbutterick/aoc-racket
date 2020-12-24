#lang br
(require racket/file rackunit)

(define (lex-1 port)
  (match (regexp-match #rx"^se|nw|sw|ne|w|e" port)
    [#false eof]
    [(list str) str]))

(define tokss (for/list ([ln (in-list (file->lines "24.rktd"))])
                (for/list ([tok (in-port lex-1 (open-input-string ln))])
                  tok)))

(define (tok->delta tok)
  (match tok
    [#"w" -1]
    [#"e" 1]
    [#"nw" -i]
    [#"ne" 1-i]
    [#"sw" -1+i]
    [#"se" +i]))

(define white -1)
(define black 1)
(define black? positive?)

(define (make-initial-floor tokss)
  (define floor (make-hasheqv))
  (for ([toks (in-list tokss)])
    (hash-update! floor (apply + (map tok->delta toks)) - white))
  floor)

(define deltas '(1 -1 -i 1-i -1+i +i))

(define (grow! floor)
  (for* ([k (in-list (hash-keys floor))]
         [delta (in-list deltas)])
    (hash-ref! floor (+ k delta) white)))

(define (adjacent-black-tiles floor k)
  (for/sum ([delta (in-list deltas)]
            #:when (black? (hash-ref floor (+ k delta) white)))
    1))

(define (iterate floor num)
  (for ([i (in-range num)])
    (grow! floor)
    (define fliplist
      (for/list ([(k v) (in-hash floor)]
                 #:when (let ([abt (adjacent-black-tiles floor k)])
                          (cond
                            [(and (black? v) (or (zero? abt) (< 2 abt)))]
                            [(and (not (black? v)) (= 2 abt))]
                            [else #false])))
        k))
    (for ([k (in-list fliplist)])
      (hash-update! floor k -)))
  floor)
    
(define floor (make-initial-floor tokss))
(check-equal? (count black? (hash-values floor)) 436)
(check-equal? (count black? (hash-values (iterate floor 100))) 4133)