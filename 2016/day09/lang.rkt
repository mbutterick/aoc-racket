#lang br/quicklang
(require racket/string)
(provide read-syntax)
(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,@(for/list ([str (in-lines port)]
                     #:when (not (equal? str "")))
            `(solve ,(string-trim str))))))

(provide solve #%module-begin)
(define (solve str)
  (for-each displayln
            (let (#;[decomp-result (decomp str)]
                  [decomp2-result (decomp2 str)])
              (list #;decomp-result #;(string-length decomp-result)
                    decomp2-result))))

(define (decomp str)
  (define p (open-input-string str))
  (define decompressor-pat "\\((\\d+)x(\\d+)\\)")
  (let loop ([bstrs empty][decompressor #f])
    (define cur-pat (if decompressor
                        (pregexp (format ".{~a}" (car decompressor)))
                        (pregexp (format "~a|." decompressor-pat))))
    (define match (let ([result (regexp-try-match cur-pat p)])
                    (and result (car result))))
    (cond
      [(not match) (string-append* (map ~a (reverse (flatten bstrs))))]
      [decompressor (loop (cons (make-list (cadr decompressor) match) bstrs) #f)]
      [else
       (define maybe-decompressor (regexp-match (pregexp decompressor-pat) match))
       (if maybe-decompressor
           (loop bstrs (map (compose1 string->number ~a) (cdr maybe-decompressor)))
           (loop (cons match bstrs) #f))])))

(define (decomp2 str)
  (define p (open-input-string str))
  (define decompressor-pat "\\((\\d+)x(\\d+)\\)")
  (let loop ([sum 0][decompressor #f])
    (define cur-pat (if decompressor
                        (pregexp (format ".{~a}" (car decompressor)))
                        (pregexp (format "~a|." decompressor-pat))))
    (define match (let ([result (regexp-try-match cur-pat p)])
                    (and result (car result))))
    (cond
      [(not match) sum]
      [decompressor (loop (+ sum (* (cadr decompressor) (decomp2 (~a match)))) #f)]
      [else
       (define maybe-decompressor (regexp-match (pregexp decompressor-pat) match))
       (if maybe-decompressor
           (loop sum (map (compose1 string->number ~a) (cdr maybe-decompressor)))
           (loop (+ sum (string-length (~a match))) #f))])))

