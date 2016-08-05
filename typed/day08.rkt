#lang typed/racket

(require typed/rackunit)
(provide (all-defined-out))

(: memory-length (-> String Natural))
(define (memory-length str)
  (string-length (cast (read (open-input-string str)) String)))

(: q1 (-> (Listof String) Integer))
(define (q1 strs)
  (- (apply + (map string-length strs)) (apply + (map memory-length strs))))

(: encoded-length (-> String Natural))
(define (encoded-length str)
  (string-length (~v str)))

(: q2 (-> (Listof String) Integer))
(define (q2 strs)
  (- (apply + (map encoded-length strs)) (apply + (map string-length strs))))

(module+ test
  (define input-strs (file->lines "../day08-input.txt"))
  (check-equal? (q1 input-strs) 1333)
  (check-equal? (q2 input-strs) 2046))

