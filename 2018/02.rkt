#lang debug br

(define fp (open-input-file "02.txt"))

(define ids
  (map string-trim (port->lines fp)))

(define ((cluster len) chars)
  (for/or ([group (in-list (group-by values chars char=?))])
    (= len (length group))))

(define (★)
  (define charss (map string->list ids))
  (define twos (length (filter (cluster 2) charss)))
  (define threes (length (filter (cluster 3) charss)))
  (* twos threes))

(define (differ-by-one? id id2)
  (= (length (common-chars id id2)) (sub1 (string-length id))))

(define (common-chars id id2)
  (for/list ([c (in-string id)]
             [c2 (in-string id2)]
             #:when (char=? c c2))
    c))

(define (★★)
  (list->string
   (for*/first ([id (in-list ids)]
                [id2 (in-list (cdr ids))]
                #:when (differ-by-one? id id2))
     (common-chars id id2))))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 6696)
  (check-equal? (time (★★)) "bvnfawcnyoeyudzrpgslimtkj"))