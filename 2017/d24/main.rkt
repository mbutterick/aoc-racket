#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]) ★ ★★)

(define (read-syntax path port)
  (define lines (port->lines port))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([datum (in-port read (open-input-string (car lines)))])
                          datum)
                     #,@(map (λ (ln) (apply cons (map string->number (string-split ln "/")))) (cdr lines)))))

(define-macro (#%mb STARS DOMS ...)
  #'(#%module-begin
     (time (STARS '(DOMS ...)))))

(define (find-dominoes-with-val dominoes val)
  (filter (λ (d) (or (= (car d) val) (= (cdr d) val))) dominoes))

(define (other-val-on-domino dom val)
  ((if (= (car dom) val) cdr car) dom))

(define (remove-dom doms dom)
  (filter-not (λ (d) (equal? d dom)) doms))

(define (bridges dominoes [current-val 0] [current-bridge null])
  (define doms (find-dominoes-with-val dominoes current-val))
  (if (null? doms)
      (list (flatten current-bridge))
      (append-map (λ (dom) (bridges (remove-dom dominoes dom)
                                    (other-val-on-domino dom current-val)
                                    (cons dom current-bridge))) doms)))

(define (strongest bridges)
  (apply max (map (λ (br) (apply + br)) bridges)))

(define (★ dominoes)
  (strongest (bridges dominoes)))

(define (★★ dominoes)
  (define brs (bridges dominoes))
  (define maxlen (apply max (map length brs)))
  (strongest (filter (λ (b) (= maxlen (length b))) brs)))
                                   
