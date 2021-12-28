#lang br
(require racket/file sugar rackunit racket/set racket/bool)

(define links (for/set ([line (file->lines "12.rktd")])
                (list->set (string-split line "-"))))

(define (is-downcase? loc) (equal? (string-downcase loc) loc))

(define (paths-from last-loc links [small-caves (set)])
  (match last-loc
    ["end" '(("end"))]
    [loc #:when (and small-caves (set-member? small-caves loc))
     (define pruned-links
       (for*/set ([previous-locs (in-value (set-remove small-caves loc))]
                  [link links]
                  #:when (set-empty? (set-intersect link previous-locs)))
         link))
     (paths-from last-loc pruned-links #false)]
    [loc
     (define next-lc-visited (cond
                               [(false? small-caves) #false]
                               [(and (is-downcase? loc) (not (equal? loc "start")))
                                (set-add small-caves loc)]
                               [else small-caves]))
     (define links-with-loc (for/set ([link links]
                                      #:when (set-member? link loc))
                              link))
     (define remaining-links (if (or (equal? loc "start")
                                     (and (is-downcase? loc) (not small-caves)))
                                 (set-subtract links links-with-loc)
                                 links))
     (for*/list ([link links-with-loc]
                 [next-loc (in-value (set-first (set-remove link loc)))]
                 [path (paths-from next-loc remaining-links  next-lc-visited)])
       (cons loc path))]))

(check-equal? (length (paths-from "start" links #f)) 5576)
(check-equal? (length (paths-from "start" links)) 152837)