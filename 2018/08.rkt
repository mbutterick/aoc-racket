#lang debug br

(define ps "08.txt")

(define (★)
  (define (node-value)
    (define child-count (read))
    (define metadata-count (read))
    (+ (for/sum ([c (in-range child-count)])
         (node-value))
       (for/sum ([c (in-range metadata-count)])
         (read))))
  (with-input-from-file ps node-value))

(define (★★)
  (define (node-value)
    (define child-count (read))
    (define metadata-count (read))
    (define child-values (for/list ([c (in-range child-count)])
                           (node-value)))
    (define metadatas (for/list ([c (in-range metadata-count)])
                        (read)))
    (if (zero? child-count)
        (apply + metadatas)
        (for/sum ([md (in-list metadatas)]
                  #:when (<= 1 md child-count))
          (list-ref child-values (sub1 md)))))
  (with-input-from-file ps node-value))

(module+ test
  (require rackunit)
  (check-equal? (time (★)) 44838)
  (check-equal? (time (★★)) 22198))