#lang br
(require racket/file rackunit racket/dict math)

(define insts
  (for/list ([ln (file->lines "14.rktd")])
    (cond
      [(string-prefix? ln "mask = ") (string-trim ln "mask = ")]
      [else (apply cons (map string->number (cdr (regexp-match #px"^mem\\[(\\d+)\\] = (\\d+)$" ln))))])))

(define (cs2int cs) (string->number (list->string cs) 2))
(define (int2str int) (~r #:base 2 int #:min-width 36 #:pad-string "0"))

(define (solve [decoder-version 1])
  (define memory (make-hasheq))
  (for/fold ([bitmask #f]
             #:result (apply + (hash-values memory)))
            ([inst (in-list insts)])
    (match inst
      [(? string? new-bitmask) new-bitmask]
      [(cons loc val)
       (define value-to-store
         (cond
           [(eq? decoder-version 1)
            (cs2int (for/list ([val-char (in-string (int2str val))]
                               [mask-char (in-string bitmask)])
                      (if (char=? mask-char #\X) val-char mask-char)))]
           [else val]))
       (define expanded-locs
         (cond
           [(eq? decoder-version 2)
            (for/fold ([locs '(())]
                       #:result (map (λ (loc) (cs2int (reverse loc))) locs))
                      ([loc-char (in-string (int2str loc))]
                       [mask-char (in-string bitmask)])
              (append* (for/list ([loc (in-list locs)])
                         (match mask-char
                           [#\0 (list (cons loc-char loc))]
                           [#\1 (list (cons #\1 loc))]
                           [_ (list (cons #\1 loc) (cons #\0 loc))]))))]
           [else (list loc)]))
       (for ([loc expanded-locs])
         (hash-set! memory loc value-to-store))
       bitmask])))

(check-equal? (solve) 13727901897109)
(check-equal? (solve 2) 5579916171823)