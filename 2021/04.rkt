#lang br
(require racket/file sugar rackunit)

(define lines (file->lines "04.rktd"))

(define numbers-to-draw (map string->number (string-split (string-replace (car lines) "," " "))))

(define boards
  (map list->vector (slice-at (map string->number (string-split (string-join (cdr lines)))) 25)))

(define winning-lines
  (let ()
    (define rows '((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24)))
    (define cols (apply map list rows))
    (append rows cols)))

(define (board-wins board)
  (for/or ([winning-line winning-lines])
    (for/and ([idx winning-line])
      (eq? (vector-ref board idx) #true))))

(define (run-game boards)
  (for*/fold ([boards boards]
              [winners null]
              #:result (reverse winners))
             ([number numbers-to-draw]
              #:break (empty? boards))
    (for ([board boards])
      (define pos-of-number (vector-member number board))
      (when pos-of-number
        (vector-set! board pos-of-number #true)))
    (define-values (new-winners losers) (partition board-wins boards))
    (values losers (append (for/list ([winner new-winners])
                             (cons number winner)) winners))))

(define (calc-score res)
  (match res
    [(cons last-number board) (* last-number (apply + (filter number? (vector->list board))))]))

(define results (run-game boards))
(check-equal? (calc-score (first results)) 64084)
(check-equal? (calc-score (last results)) 12833)

