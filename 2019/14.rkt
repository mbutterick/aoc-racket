#lang br/quicklang
(require (for-syntax racket/string racket/sequence) racket/file rackunit)

(define ore? number?)

(define (make-reactor reax-output proc-or-ore [t (current-thread)])
  (let* ([ch (make-channel)]
         [r (thread (λ ()
                      (define reax 0)
                      (define reax-formula (match proc-or-ore
                                             [(? ore? ore) ore]
                                             [proc proc]))
                      (let loop ([supply 0])
                        (match (channel-get ch)
                          ['ore
                           (channel-put ch (match reax-formula
                                             [(? ore? ore) (* ore reax)]
                                             [_ 0]))
                           (loop supply)]
                          ['reset
                           (set! reax 0)
                           (channel-put ch always-evt)
                           (loop 0)]
                          [amt (let inner ([supply supply])
                                 (cond
                                   [(< supply amt)
                                    (set! reax (add1 reax))
                                    (unless (ore? reax-formula)
                                      (reax-formula))
                                    (inner (+ supply reax-output))]
                                   [else
                                    (channel-put ch always-evt)
                                    (loop (- supply amt))]))]))))])
    (λ (arg)
      (channel-put ch arg)
      (channel-get ch))))

(define-syntax (handle stx)
  (syntax-case stx (ORE)
    [(_) #'(begin)]
    [(_ X ORE => Q ID) #'(define ID (make-reactor Q X))]
    [(_ ARG ... => Q ID)
     (with-syntax ([(PR ...)
                    (for/list ([duo (in-slice 2 (reverse (syntax->datum #'(ARG ...))))])
                              (list (datum->syntax stx (car duo))
                                    (cadr duo)))]
                   [ID (datum->syntax stx (syntax->datum #'ID))])
       #'(define ID (make-reactor Q (λ () (sync PR ...)))))]))

(define-syntax (total stx)
  (syntax-case stx ()
    [(_ ID ...)
     (with-syntax ([(ID ...) (for/list ([idstx (in-list (syntax->list #'(ID ...)))])
                                       (datum->syntax stx (syntax->datum idstx)))])
       #'(+ (ID 'ore) ...))]))
(provide handle quote total void)

(module+ reader
  (provide read-syntax)
  (define (read-syntax name ip)
    (define lns (for/list ([ln (in-list (port->lines ip))]
                           #:when (positive? (string-length ln)))
                          (string-replace ln "," "")))
    (define src-datums (format-datums '(handle ~a) lns))
    (datum->syntax #f `(module _ "14.rkt"
                         ,@src-datums
                         (void (FUEL 1))
                         (total ,@(map last src-datums))))))

(define-macro (mb ARG ...)
  (with-syntax ([FUEL (datum->syntax caller-stx 'FUEL)])
    #'(#%module-begin
       ARG ...)))
(provide (rename-out [mb #%module-begin]))
