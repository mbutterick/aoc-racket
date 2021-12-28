#lang br
(require racket/file rackunit)

(define lines (map string->list (file->lines "10.rktd")))
(define left-toks (list #\[ #\( #\{ #\<))
(define right-toks (list #\] #\) #\} #\>))
(define duos (map list left-toks right-toks))
(define (matched-pair? left right) (member (list left right) duos))

(define (parse tokens)
  (with-handlers ([char? values])
    (let loop ([tokens tokens][lefts null])
      (cond
        [(empty? tokens) lefts]
        [(memv (car tokens) left-toks)
         (loop (cdr tokens) (cons (car tokens) lefts))]
        [(and (pair? lefts) (matched-pair? (car lefts) (car tokens)))
         (loop (cdr tokens) (cdr lefts))]
        [else (raise (car tokens))]))))

(define (corrupt? parsed-line)
  (case parsed-line
    [(#\)) 3]
    [(#\]) 57]
    [(#\}) 1197]
    [(#\>) 25137]
    [else #f]))

(check-equal? (apply + (filter-map corrupt? (map parse lines))) 366027)

(define (autocomplete-score parsed-line)
  (for/fold ([total 0])
            ([c parsed-line])
    (+ (* total 5) (case c
                     [(#\() 1]
                     [(#\[) 2]
                     [(#\{) 3]
                     [(#\<) 4]))))

(define autocomplete-scores
  (map autocomplete-score (filter-not corrupt? (map parse lines))))

(check-equal? (list-ref (sort autocomplete-scores >) (floor (/ (length autocomplete-scores) 2))) 1118645287)