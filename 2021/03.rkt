#lang br
(require racket/file rackunit)

(define lines (map string->list (file->lines "03.rktd")))
(define (digit-columns lines) (apply map list lines))

(define (most-common-digit chars)
  (define zeroes (count (λ (c) (char=? c #\0)) chars))
  (define most-threshold (/ (length chars) 2))
  (cond
    [(= zeroes most-threshold) #f]
    [(> zeroes most-threshold) #\0]
    [else #\1]))

(define (least-common-digit chars)
  (match (most-common-digit chars)
    [#false #false]
    [#\0 #\1]
    [_ #\0]))

(define (chars->binary-number chars)
  (string->number (list->string chars) 2))

(define gamma-rate (chars->binary-number (map most-common-digit (digit-columns lines))))
(define epsilon-rate (chars->binary-number (map least-common-digit (digit-columns lines))))

(check-equal? (* gamma-rate epsilon-rate) 4174964)

(define (find-digit proc default)
    (for/fold ([lines lines]
             #:result (chars->binary-number (car lines)))
            ([i (in-range (length (car lines)))]
             #:break (= (length lines) 1))
    (define target (or (proc (list-ref (digit-columns lines) i)) default))
    (filter (λ (line) (char=? (list-ref line i) target)) lines)))

(define oxygen-rate (find-digit most-common-digit #\1))
(define co2-rate (find-digit least-common-digit #\0))

(check-equal? (* oxygen-rate co2-rate) 4474944)