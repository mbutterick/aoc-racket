#lang br/quicklang ;; http://adventofcode.com/2016/day/17
(require openssl/md5 sugar/cache)
(provide read-syntax
         (rename-out [mb #%module-begin]))

(define (read-syntax path port)
  (strip-bindings
   #`(module mod "lang.rkt"
       #,(string-trim (port->string port)))))

(define-macro (mb STR)
  #'(#%module-begin
     (displayln (solve-shortest STR))
     (displayln (solve-longest STR))))

(define (path->dirs path)
  (regexp-match* #rx"[UDLR]" path))

(define (on-grid? pos)
  (and (<= 0 (real-part pos) 3)
       (<= 0 (imag-part pos) 3)))

(define/caching (follow-path path)
  (define result (regexp-match #rx"^(.*)([UDLR])$" path))
  (define end
    (cond
      [result
       (match-define (list _ prefix suffix) result)
       (+ (follow-path prefix) (case suffix
                                 [("D") +i]
                                 [("U") -i]
                                 [("L") -1]
                                 [("R") 1]))]
      [else 0]))
  (and (on-grid? end) end))

(define/caching (get-hash str)
  (md5 (open-input-string str)))

(define (take-step path)
  (define hash (get-hash path))
  (define prefix (car (regexp-match #rx"^...." hash)))
  (define possible-dirs
    (for/list ([dir (in-list (list "U" "D" "L" "R"))]
               [c (in-string prefix)]
               #:when (member c '(#\b #\c #\d #\e #\f)))
      dir))
  (for*/list ([dir (in-list possible-dirs)]
              [path+dir (in-value (string-append path dir))]
              #:when (follow-path path+dir))
    path+dir))

(define vault 3+3i)

(define (solve-shortest str)
  (let loop ([paths (list str)])
     (define stepped-paths (append-map take-step paths))
     (if (empty? stepped-paths)
         'no-solution
         (or (for/first ([sp (in-list stepped-paths)]
                         #:when (= vault (follow-path sp)))
               (apply string-append (path->dirs sp)))
             (loop stepped-paths)))))

(define (solve-longest str)
  (length
   (path->dirs
    (argmax string-length
            (let loop ([paths (list str)][vault-paths empty][i 0])
              (cond
                [(empty? paths) vault-paths]
                [else
                 (define stepped-paths (append-map take-step paths))
                 (define-values (new-vault-paths other-paths)
                   (partition (λ (sp) (= vault (follow-path sp))) stepped-paths))
                 (loop other-paths (if (pair? new-vault-paths) new-vault-paths vault-paths) (add1 i))]))))))
