#lang br
(require racket/file rackunit racket/dict)

(match-define (cons target buses-all)
  (for/list ([tok
              (in-port read
                       (open-input-string
                        (string-replace (file->string "13.rktd") "," " ")))])
    tok))

(define (overshoot bus)
  (for/last ([i (in-naturals)]
             #:final (> (* i bus) target))
    (- (* i bus) target)))

(define (solve-1)
  (define winner (argmin overshoot (filter integer? buses-all)))
  (* winner (overshoot winner)))

(check-equal? (solve-1) 246)

(define (solve-2)
  (define pairs (for/list ([(b i) (in-indexed buses-all)]
                           #:when (integer? b))
                  (cons b i)))
  (for/fold ([t 0]
             [increment 1]
             #:result t)
            ([(bus overshoot) (in-dict pairs)])
    (define mod-target (let loop ([overshoot overshoot])
                         (cond
                           [(zero? overshoot) 0]
                           [(< overshoot bus) (- bus overshoot)]
                           [else (loop (- overshoot bus))])))
    (define multiplier
      (for/first ([multiplier (in-naturals)]
                  #:when (= (modulo (+ t (* increment multiplier)) bus) mod-target))
        multiplier))
    (values (+ t (* increment multiplier)) (* increment bus))))

(check-equal? (solve-2) 939490236001473)