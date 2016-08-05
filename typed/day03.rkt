#lang typed/racket
(require typed/rackunit)
(provide (all-defined-out))

(define-type Move (List Integer Integer))
(define-type Cell (List Integer Integer))

(: string->cells (-> String (Listof Cell)))
(define (string->cells str)
  (define start '(0 0))
  (match-define (list east north west south) '((1 0) (0 1) (-1 0) (0 -1)))
  (define moves (for/list : (Listof Move)
                          ([c (in-string str)])
                          (case c
                            [(#\>) east]
                            [(#\^) north]
                            [(#\<) west]
                            [(#\v) south]
                            [else (error 'TR)])))
  (for/fold : (Listof Cell)
            ([cells-so-far : (Listof Cell) (list start)])
            ([next-move (in-list moves)])
    (define current-cell (car cells-so-far))
    (define next-cell (map + current-cell next-move))
    (cons (cast next-cell Cell) cells-so-far)))

(: q1 (-> String Integer))
(define (q1 str)
  (length (remove-duplicates (string->cells str))))

(: string->complex-cells (-> String (Listof Complex)))
(define (string->complex-cells str)
  (define start 0)
  (define east 1)
  (define moves (for/list : (Listof Complex)
                          ([c (in-string str)])
                          (* east (expt +i (case c
                                             [(#\>) 0]
                                             [(#\^) 1]
                                             [(#\<) 2]
                                             [(#\v) 3]
                                             [else (error 'TR)])))))
  (for/fold : (Listof Complex)
            ([cells-so-far : (Listof Complex) (list start)])
            ([next-move (in-list moves)])
    (define current-cell (car cells-so-far))
    (define next-cell (+ current-cell next-move))
    (cons next-cell cells-so-far)))

(: q1-complex (-> String Natural))
(define (q1-complex str)
  (length (remove-duplicates (string->complex-cells str))))

(: split-odds-and-evens (-> String (Values String String)))
(define (split-odds-and-evens str)
  (define-values (odd-chars even-chars)
    (for/fold : (Values (Listof Char) (Listof Char))
              ([odds-so-far : (Listof Char) empty][evens-so-far : (Listof Char) empty])
              ([c (in-string str)][i (in-naturals)])
      (if (even? i)
          (values odds-so-far (cons c evens-so-far))
          (values (cons c odds-so-far) evens-so-far))))
  (values (string-append* (map string (reverse odd-chars)))
          (string-append* (map string (reverse even-chars)))))

(: q2 (-> String Integer))
(define (q2 str)
  (define-values (odd-str even-str) (split-odds-and-evens str))
  (length (remove-duplicates
           (append (string->cells odd-str) (string->cells even-str)))))

(module+ test
  (define input-str (file->string "../day03-input.txt"))
  (check-equal? (q1 input-str) 2565)
  (check-equal? (q1-complex input-str) 2565)
  (check-equal? (q2 input-str) 2639))
