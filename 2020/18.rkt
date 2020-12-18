#lang at-exp br
(require racket/file rackunit br/module)

@module/lang[parser1]{
 #lang brag
 op : [op ("+" | "*")] term
 @"@"term : /"(" op /")" | digit
 @"@"digit : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
}

(require (prefix-in parser1: 'parser1))

(define (op . xs)
  (match xs
    [(list left opstr right) ((if (equal? opstr "*") * +) left (op right))]
    [(list (? number? num)) num]
    [(list (app string->number num)) num]))

(define-namespace-anchor ns)

(define ((solve parser) str)
  (eval (parser (regexp-match* #px"\\S" str)) (namespace-anchor->namespace ns)))

(check-equal? (apply + (map (solve parser1:parse) (file->lines "18.rktd"))) 9535936849815)

@module/lang[parser2]{
 #lang brag
 prod : [prod "*"] sum
 sum : [sum "+"] term
 @"@"term : /"(" prod /")" | digit
 @"@"digit : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
}

(define prod op)
(define sum op)

(require (prefix-in parser2: 'parser2))
(check-equal?
(apply + (map (solve parser2:parse) (file->lines "18.rktd"))) 472171581333710)