#lang br/quicklang
(require openssl/md5)

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "day05.rkt"
       (solve #,(string-trim (port->string port))))))
(module+ reader (provide read-syntax))

(provide #%module-begin)

(define (solve key)
  (define-values
    (part-a-solution part-b-solution)
    (for*/fold ([part-a empty]
                [part-b (make-vector 8 #f)])
               ([idx (in-naturals 1000000)]
                [this-key (in-value (format "~a~a" key idx))]
                #:break (and (= 8 (length part-a))
                             (andmap string? (vector->list part-b))))
      (define this-hash (md5 (open-input-string this-key)))
      (define next-part-a
        (if (and (not (= 8 (length part-a)))
                 (string-prefix? this-hash "00000"))
            (let ([next-str (substring this-hash 5 6)])
              (displayln (format "part a progress: ~a" next-str))
              (cons next-str part-a))
            part-a))
      (define next-part-b
        (let ()
          (when (and (string-prefix? this-hash "00000")
                     (string->number (substring this-hash 5 6))
                     (<= 0 (string->number (substring this-hash 5 6)) 7)
                     (not (vector-ref part-b (string->number (substring this-hash 5 6)))))
            (displayln (format "part b progress: idx ~a, hash ~a, ~a at ~a" idx this-hash (substring this-hash 6 7) (substring this-hash 5 6)))
            (vector-set! part-b (string->number (substring this-hash 5 6)) (substring this-hash 6 7)))
          part-b))
      (values next-part-a next-part-b)))
  (displayln (format "part a: ~a" (apply string-append (reverse part-a-solution))))
  (displayln (format "part b: ~a" (apply string-append (vector->list part-b-solution)))))
(provide solve)
