#lang racket
(require rackunit)
(provide read-syntax)

(define (str->visits str)
  (define start '(0 0))
  (define moves (map (λ(move) (case move
                                [("^") '(0 1)]
                                [("v") '(0 -1)]
                                [("<") '(-1 0)]
                                [(">") '(1 0)]))
                     (regexp-match* #rx"." str)))
  (reverse (for/fold ([visit-acc (list start)])
                     ([move (in-list moves)])
             (cons (map + move (car visit-acc)) visit-acc))))

(define (str->unique-visits str)
  (define visits (str->visits str))
  (length (remove-duplicates visits)))

(define (str->robosanta str)
  (define-values (reversed-santa-path reversed-robo-path)
    (for/fold ([santa-acc empty][robo-acc empty])
              ([c (in-string str)][pos (in-naturals)])
      (if (even? pos)
          (values (cons c santa-acc) robo-acc)
          (values santa-acc (cons c robo-acc)))))
  (define santa-str (string-append* (map ~a (reverse reversed-santa-path))))
  (define robo-str (string-append* (map ~a (reverse reversed-robo-path))))
  (length (remove-duplicates (append (str->visits santa-str) (str->visits robo-str)))))


(check-equal? (str->unique-visits ">") 2)
(check-equal? (str->unique-visits "^>v<") 4)

(check-equal? (str->robosanta "^v") 3)
(check-equal? (str->robosanta "^>v<") 3)
(check-equal? (str->robosanta "^v^v^v^v^v") 11)


(define (read-syntax source-path-string in-port)
  (with-syntax ([source-str (string-trim (port->string in-port))]
                [str->unique-visits str->unique-visits]
                [str->robosanta str->robosanta])
    #'(module _ racket
        (str->unique-visits source-str)
        (str->robosanta source-str))))