#lang at-exp racket/base
(require scribble/manual scribble/html-properties
         scribble/core scribble/decode)
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(define current-day (make-parameter #f))
(define current-section (make-parameter #f))

(define-syntax-rule (iracket term)
  (list
   (part-index-decl
    (list (symbol->string 'term))
    (list (tt (symbol->string 'term))
          (if (current-section)
              (decode-content (cons (format "in Day ~a / " (current-day)) (current-section)))
              "")))
   (racket term)))

(define-syntax (define-isec stx)
  (syntax-case stx ()
    [(_ secid)
     (with-syntax ([isecid (format-id stx "i~a" #'secid)])
       #'(define isecid
           (make-keyword-procedure
            (λ (kws kwargs . args)
              (begin
                (current-section args)
                (keyword-apply secid kws kwargs args))))))]))

(define-isec section)
(define-isec subsection)


(define (aoc-title which)
  (define which-str (number->string which))
  (current-day which-str)
  (define day-x (format "day-~a" which-str))
  (define day-prefix (format "~a-" day-x))
  @title[#:style manual-doc-style]{Day @which-str})

(define (link-rp path . text-args)
  (element (style #f (list (link-resource path)))
           text-args))