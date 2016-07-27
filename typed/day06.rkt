#lang typed/racket
(require typed/rackunit trivial/regexp/no-colon)
(provide (all-defined-out))

(: string->natural (-> String Natural))
(define (string->natural str)
  (cast (string->number str) Natural))

(: str->instruction (-> String (Pairof (-> Natural Natural) (Listof Natural))))
(define (str->instruction str)
  (match-define (list* _ action coordinates)
    (regexp-match #px"^(.*?)(\\d+),(\\d+) through (\\d+),(\\d+)$" str))
  (: action->bulb-func (-> String (-> Natural Natural)))
  (define (action->bulb-func action)
    (case action
      [("turn on") (λ([bulb : Natural]) 1)] ;;bg : 2016-07-25 TR cannot use thunk*
      [("turn off") (λ([bulb : Natural]) 0)]
      [else (λ([bulb : Natural]) (if (= bulb 1) 0 1))]))
  (list* (action->bulb-func (string-trim action))
         (map string->natural coordinates)))

(: q1 (-> (Listof String) Natural))
(define (q1 strs)
  (define lights : (Vectorof Natural) (make-vector (* 1000 1000) 0))
  (for ([instruction (in-list (map str->instruction strs))])
       (set-lights lights instruction))
  (count-lights lights))

(: set-lights (-> (Vectorof Natural) (Pairof (-> Natural Natural) (Listof Natural)) Void))
(define (set-lights lights arglist)
  (match-define (list bulb-func x1 y1 x2 y2) arglist)
  (for* ([x (in-range x1 (add1 x2))][y (in-range y1 (add1 y2))])
        (define vector-loc (+ (* 1000 x) y))
        (define current-light (vector-ref lights vector-loc))
        (vector-set! lights vector-loc (bulb-func current-light))))

(: count-lights (-> (Vectorof Natural) Natural))
(define (count-lights lights)
  (for/sum : Natural ([light (in-vector lights)]
            #:when (positive? light))
           light))

(: str->instruction-2 (-> String (Pairof (-> Natural Natural) (Listof Natural))))
(define (str->instruction-2 str)
  (match-define (list* _ action coordinates)
    (regexp-match #px"^(.*?)(\\d+),(\\d+) through (\\d+),(\\d+)$" str))
  (: action->bulb-func (-> String (-> Natural Natural)))
  (define (action->bulb-func action)
    (case action
      [("turn on") (λ([bulb : Natural]) (add1 bulb))]
      [("turn off") (λ([bulb : Natural]) (max 0 (sub1 bulb)))]
      [else (λ([bulb : Natural]) (+ bulb 2))]))
  (list* (action->bulb-func (string-trim action))
         (map string->natural coordinates)))

(: q2 (-> (Listof String) Natural))
(define (q2 strs)
  (define lights : (Vectorof Natural) (make-vector (* 1000 1000) 0))
  (for ([instruction (in-list (map str->instruction-2 strs))])
       (set-lights lights instruction))
  (count-lights lights))

(: day06-solve (-> (Listof String) (-> String (-> Natural Natural)) Natural))
(define (day06-solve strs bulb-func-converter)
  (define lights : (Vectorof Natural) (make-vector (* 1000 1000) 0))
  (for ([instruction (in-list (map (make-str-converter bulb-func-converter) strs))])
       (set-lights lights instruction))
  (count-lights lights))

(: make-str-converter (-> (-> String (-> Natural Natural)) (-> String (Pairof (-> Natural Natural) (Listof Natural)))))
(define (make-str-converter bulb-func-converter)
  (λ ([str : String])
    (match-define (list* _ action coordinates)
      (regexp-match #px"^(.*?)(\\d+),(\\d+) through (\\d+),(\\d+)$" str))
    (list* (bulb-func-converter (string-trim action))
           (map string->natural coordinates))))

(: q1-bulb-func-converter (-> String (-> Natural Natural)))
(define q1-bulb-func-converter
  (λ([action : String]) (case action
               [("turn on") (λ([bulb : Natural]) 1)]
               [("turn off") (λ([bulb : Natural]) 0)]
               [else (λ([bulb : Natural]) (if (= bulb 1) 0 1))])))

(: q2-bulb-func-converter (-> String (-> Natural Natural)))
(define q2-bulb-func-converter
  (λ([action : String]) (case action
               [("turn on") (λ([bulb : Natural]) (add1 bulb))]
               [("turn off") (λ([bulb : Natural]) (max 0 (sub1 bulb)))]
               [else (λ([bulb : Natural]) (+ bulb 2))])))


(module+ test
  (define input-strs (file->lines "../day06-input.txt"))
  (check-equal? (q1 input-strs) 400410)
  (check-equal? (q2 input-strs) 15343601)
  (check-equal? (day06-solve input-strs q1-bulb-func-converter) 400410)
  (check-equal? (day06-solve input-strs q2-bulb-func-converter) 15343601))

