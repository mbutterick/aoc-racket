#lang br
(require racket/file rackunit racket/dict)

(define (parse-ints strs) (map (λ (str) (string-split str)) strs))

(define recs
  (let* ([lines (string-split (file->string "08.rktd") "\n")]
         [sublines (map (λ (str) (string-split str "|")) lines)])
    (map parse-ints sublines)))

(define strlens (append-map (λ (vals) (map string-length vals)) (map second recs)))
(check-equal? (count (λ (len) (memq len '(2 3 4 7))) strlens) 278)

(require csp racket/set)

(define (infer-config #:one one-digit #:four four-digit #:seven seven-digit #:five five-segment-digits)
  (define prob (make-csp))
  (add-vars! prob '(s1 s2 s3 s4 s5 s6 s7) '(a b c d e f g))
  (add-all-diff-constraint! prob #:same eq?)
  (add-constraint! prob  (λ (s3 s6) (equal? (set s3 s6) one-digit)) '(s3 s6))
  (add-constraint! prob 
                   (λ (s1 s2 s3 s4 s5 s6 s7)
                     (define two-pattern (set s1 s3 s4 s5 s7))
                     (define three-pattern (set s1 s3 s4 s6 s7))
                     (define five-pattern (set s1 s2 s4 s6 s7))
                     (for/and ([pat (list two-pattern three-pattern five-pattern)])
                       (member pat five-segment-digits)))
                   '(s1 s2 s3 s4 s5 s6 s7))
  (add-constraint! prob (λ (s2 s3 s4 s6) (equal? (set s2 s3 s4 s6) four-digit)) '(s2 s3 s4 s6))
  (add-constraint! prob (λ (s1 s3 s6) (equal? (set s1 s3 s6) seven-digit)) '(s1 s3 s6))
  (solve prob))

(define segments-table (list
                        (cons (set 's1 's2 's3 's5 's6 's7) 0)
                        (cons (set 's3 's6) 1)
                        (cons (set 's1 's3 's4 's5 's7) 2)
                        (cons (set 's1 's3 's4 's6 's7) 3)
                        (cons (set 's2 's3 's4 's6) 4)
                        (cons (set 's1 's2 's4 's6 's7) 5)
                        (cons (set 's1 's2 's4 's5 's6 's7) 6)
                        (cons (set 's1 's3 's6) 7)
                        (cons (set 's1 's2 's3 's4 's5 's6 's7) 8)
                        (cons (set 's1 's2 's3 's4 's6 's7) 9)))

(define (string->symbols str) (map (λ (c) (string->symbol (string c))) (string->list str)))

(define (wire-config strs)
  (define encdoded-digits (map (compose1 list->set string->symbols) strs))
  (define (filter-length len) (filter (λ (x) (= len (set-count x))) encdoded-digits))
  (define one-digit (car (filter-length 2)))
  (define four-digit (car (filter-length 4)))
  (define seven-digit (car (filter-length 3)))
  (define five-segment-digits (filter-length 5))
  (infer-config #:one one-digit #:four four-digit #:seven seven-digit #:five five-segment-digits))

(define (decode-number config pattern)
  (define config-reverse (for/hasheq ([(k v) (in-dict config)])
                           (values v k)))
  (define segments (for/set ([item pattern])
                     (dict-ref config-reverse item)))
  (dict-ref segments-table segments))

(define (output-value config strs)
  (map (λ (pat) (decode-number config pat)) (map (compose1 list->set string->symbols) strs)))

(define (rec->output rec)
  (for/sum ([digit (reverse (output-value (wire-config (first rec)) (second rec)))]
            [power (in-naturals)])
    (* digit (expt 10 power))))

(check-equal? (apply + (map rec->output recs)) 986179)