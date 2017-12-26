#lang br/quicklang
(require "../helper.rkt" racket/sequence)
(provide read-syntax (rename-out [#%mb #%module-begin]) ★ ★★)

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,(for/list ([datum (in-port read (open-input-string (car lines)))])
                         datum)
                     #,@(for/list ([line (in-list (cdr lines))])
                          (for/list ([datum (string-split line)])
                            datum)))))

(define-macro (#%mb (STARS ITERATIONS) (LH "=>" RH) ...)
  #`(#%module-begin
     (time (STARS ITERATIONS '(LH . RH) ...))))

(define (charify str) (map string->list (string-split str "/")))

(define (grid-size grid) (length (car grid)))

(define (grid-split grid size)
  (append*
   (for/list ([rowset (in-slice size grid)])
     (apply map list (map (λ (row) (slice-at row size)) rowset)))))

(define (enhance-grid grid rules)
  (let/ec exit
    (for*/fold ([rotations (list grid)])
               ([i (in-range 4)])
      (define rot (apply map list (reverse (car rotations))))
      (cond
        [(hash-ref rules rot (λ () (hash-ref rules (map reverse rot) #f))) => exit]
        [else (cons rot rotations)]))))

(define (join-grids grids)
  (define side (sqrt (length grids)))
  (append*
   (for/list ([gridset (in-slice side grids)])
     (apply map append gridset))))

(define (★ iterations . rulepairs)
  (define rules (for/hash ([rp (in-list rulepairs)])
                  (values (charify (car rp)) (charify (cdr rp)))))
  (for/fold ([grid (charify ".#./..#/###")]
             #:result (count (λ (c) (char=? c #\#)) (flatten grid)))
            ([i (in-range iterations)])
    (define grid-pieces (grid-split grid (if (even? (grid-size grid)) 2 3)))
    (join-grids (map (λ (gp) (enhance-grid gp rules)) grid-pieces))))

(define ★★ ★)