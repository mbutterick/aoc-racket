#lang br/quicklang

(define (read-syntax path port)
  (define turn-strings (string-split (port->string port) ","))
  (define turn-pattern #px"^([LR])(\\d+)$")
  (define turn-datums
    (for*/list ([tstr (in-list turn-strings)])
               (define match-result (regexp-match turn-pattern (string-trim tstr)))
               `(turn ,@(cdr (or match-result empty)))))
  (strip-bindings
   #`(module day01-mod "lang.rkt"
       #,@turn-datums)))
(provide read-syntax)

(define-macro (mb . TURNS)
  #'(#%module-begin
     (solve . TURNS)))
(provide (rename-out [mb #%module-begin]))

(define (loc-dist loc)
  (+ (abs (imag-part loc)) (abs (real-part loc))))

(struct $turn (rot dist) #:transparent)
(define rotate-left +i)
(define rotate-right -i)
(define same-dir 1)

(define (solve . turns)
  (define found-twice-visited-loc #f)
  (define north 0+1i)
  (define starting-loc 0+0i)
  (let loop ([locs (list starting-loc)] [dir north] [turns turns])
    (cond
      [(empty? turns)
       (displayln (format "part 1 (dist of final location): ~a" (loc-dist (car locs))))]
      [(zero? ($turn-dist (car turns))) (loop locs dir (cdr turns))]
      [else 
       (define new-dir (* dir ($turn-rot (car turns))))
       (define one-step 1)
       (define new-loc (+ (car locs) (* new-dir one-step)))
       (when (and (not found-twice-visited-loc) (member new-loc locs))
         (set! found-twice-visited-loc new-loc)
         (displayln (format "part 2 (dist of first twice-visited location): ~a"
                            (loc-dist new-loc))))
       (define decremented-turn ($turn same-dir (sub1 ($turn-dist (car turns)))))
       (loop (cons new-loc locs) new-dir (cons decremented-turn (cdr turns)))])))

(define-macro-cases turn
  [(_ DIR DIST)
   (with-pattern ([ENCODED-DIR (syntax-case #'DIR ()
                                 ["L" #'rotate-left]
                                 ["R" #'rotate-right])])
     #'($turn ENCODED-DIR (string->number DIST)))]
  [else #'(void)])
(provide turn)