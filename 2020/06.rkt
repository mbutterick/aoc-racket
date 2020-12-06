#lang br
(require racket/file racket/set rackunit)

(define (solve set-proc)
  (for/sum ([group (string-split (file->string "06.rktd") #px"\n\n+")])
    (set-count (apply set-proc (for/list ([person (string-split group "\n")])
                                 (for/seteqv ([c person])
                                   c))))))

(check-equal? (solve set-union) 6549)

(check-equal? (solve set-intersect) 3466)

