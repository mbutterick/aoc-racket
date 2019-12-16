#lang br
(require racket/file rackunit)

(define (make-patintss ints)
  (for/list ([i (in-range (length ints))])
    (apply append (map (curry make-list (add1 i)) '(0 1 0 -1)))))

(define (fft str [phase-count 1])
  (define ints (for/list ([c (in-string str)])
                 (string->number (string c))))
  (define patintss (make-patintss ints))
  (time
   (for/fold ([ints ints]
              #:result (take ints 8))
             ([pidx (in-range phase-count)])
     (for/list ([patints (in-list patintss)])
       (modulo (abs (for/sum ([int (in-list (cons 0 ints))] ; cons 0 to burn off first patint
                              [patint (in-cycle patints)])
                      (* int patint))) 10)))))


(check-equal? (fft "80871224585914546619083218645595" 100) '(2 4 1 7 6 1 7 6))
(check-equal? (fft "19617804207202209144916044189917" 100) '(7 3 7 4 5 4 1 8))
(check-equal? (fft "69317163492948606335995924319873" 100) '(5 2 4 3 2 1 3 3))

;; 1
(check-equal? (fft (file->string "16.rktd") 100) '(8 5 7 2 6 5 0 2))