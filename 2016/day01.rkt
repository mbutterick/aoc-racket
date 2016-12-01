#lang br/quicklang
(require racket/file)
(module+ reader
  (provide read-syntax)
  (define (read-syntax path port)
    (define turn-strings (string-split (port->string port) ", "))
    (define turn-pattern #px"^([LR])(\\d+)$")
    (define turn-datums
      (for*/list ([tstr (in-list turn-strings)])
                 (define match-result (regexp-match turn-pattern (string-trim tstr)))
                 `(turn ,@(cdr (or match-result empty)))))
    (strip-bindings
     #`(module day01-mod "day01.rkt"
         #,@turn-datums))))

(define-macro (mb . TURNS)
  #'(#%module-begin
     (solve . TURNS)))
(provide (rename-out [mb #%module-begin]))

(define (loc-dist loc)
  (+ (abs (imag-part loc)) (abs (real-part loc))))

(define (locs-between loca locb)
  (range loca (add1 locb)))

(module+ test
  (require rackunit)
  (check-equal? (locs-between 0 3) '(0 1 2 3))
  (check-equal? (locs-between 3 0) (reverse (locs-between 0 3)))
  (check-equal? (locs-between +0i +3i) '(+0i +1i +2i +3i))
  (check-equal? (locs-between +3i +0i) (reverse (locs-between +0i +3i))))

(define (solve . turns)
  (define first-loc-visited-twice #f)
  (define north 0+1i)
  (let loop ([locs (list 0+0i)] [dir north] [turns turns])
    (if (empty? turns)
        (displayln (format "part 1 (dist of final location): ~a" (loc-dist (car locs))))
        (let* ([turn (car turns)]
               [rotation (car turn)]
               [new-dir (* dir rotation)]
               [dist (cdr turn)]
               [loc (car locs)]
               [new-loc (+ loc (* new-dir dist))])
          
          (when (and
                 (not first-loc-visited-twice)
                 (report* locs loc new-loc (member new-loc locs)))
            (set! first-loc-visited-twice new-loc)
            (displayln (format "part 2 (dist of first twice-visited location): ~a" (loc-dist new-loc))))        
          (loop (report (append (locs-between new-loc loc) (cdr locs))) new-dir (cdr turns))))))

(define-macro-cases turn
  [(_ DIR DIST)
   (with-pattern ([NEW-DIR (syntax-case #'DIR ()
                             ["L" #'+i]
                             ["R" #'-i])])
     #'(cons NEW-DIR (string->number DIST)))]
  [else #'(void)])
(provide turn)