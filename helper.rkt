#lang at-exp racket/base
(require scribble/manual scribble/html-properties
         scribble/core)
(provide (all-defined-out))

(define (aoc-title which)
  (define which-str (number->string which))
  (define day-x (format "day-~a" which-str))
  (define day-prefix (format "~a-" day-x))
  @title[#:style manual-doc-style]{Day @which-str})

(define (link-rp path . text-args)
  (element (style #f (list (link-resource path)))
           text-args))