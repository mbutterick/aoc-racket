#lang br
(require racket/file rackunit br/module)

(begin-for-syntax
  (require racket/match racket/string racket/file)
  (define rules
    (let* ([str (file->string "19.rktd")]
           [str (car (string-split str "\n\n"))]
           [strs (string-split str "\n")])
      (sort strs string<?))))

(define-syntax (make-parser-submodule stx)
  (syntax-case stx ()
    [(_ ID)
     #`(module/lang
        ID
        #,(regexp-replace* #px"(\\d+)"
                           (string-join (cons "#lang brag" rules) "\n") "rule\\1"))]))

(make-parser-submodule parser1)
(require (prefix-in parser1: 'parser1))

(define samples
  (string-split (cadr (string-split (file->string "19.rktd") "\n\n")) "\n"))

(define (solve parser)
  (for/sum ([sample samples])
    (with-handlers ([exn:fail? (λ (exn) 0)])
      (parser sample)
      1)))

(check-equal? (solve parser1:parse) 129)

(begin-for-syntax
  (set! rules
        (for/list ([rule rules])
          (match rule
            ["8: 42" "8: 42 | 42 8"]
            ["11: 42 31" "11: 42 31 | 42 11 31"]
            [_ rule]))))

(make-parser-submodule parser2)
(require (prefix-in parser2: 'parser2))
(check-equal? (solve parser2:parse) 243)