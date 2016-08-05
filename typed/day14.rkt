#lang typed/racket
(require typed/rackunit (for-syntax racket/file))
(provide (all-defined-out))

(define-syntax (convert-input-to-reindeer-functions stx)
  (syntax-case stx ()
    [(_)
     (let* ([input-strings (file->lines "../day14-input.txt")]
            [reindeer-strings
             (map (λ(str) (format "(reindeer ~a)" (string-downcase str))) input-strings)]
            [reindeer-datums
             (map (compose1 read open-input-string) reindeer-strings)])
       (datum->syntax stx `(begin ,@reindeer-datums)))]))

(define-type Deer-Func (-> Integer Natural))

(define-syntax (reindeer stx)
  (syntax-case stx (can fly seconds but then must rest for)
    [(_ deer-name can fly speed km/s for fly-secs seconds, but then must rest for rest-secs seconds.)
     #'(define (deer-name (total-secs : Integer)) : Natural
         (calc-distance total-secs speed fly-secs rest-secs))]
    [else #'(void)]))

(convert-input-to-reindeer-functions)

(: calc-distance (-> Integer Natural Natural Natural Natural))
(define (calc-distance total-secs speed fly-secs rest-secs)
  (let loop : Natural ([secs-remaining : Integer total-secs] [distance : Natural 0])
    (if (<= secs-remaining 0)
        distance
        (let ([secs-in-flight (min secs-remaining fly-secs)])
          (loop (- secs-remaining fly-secs rest-secs)
                (+ (* secs-in-flight speed) distance))))))

(: q1 (-> Natural))
(define (q1)
  (define seconds-to-travel 2503)
  (apply max (map (λ((deer-func : Deer-Func)) (deer-func seconds-to-travel))
                  (list dasher dancer prancer vixen comet
                        cupid donner blitzen rudolph))))


;;bg added submodule, because (HashTable A B) cannot be converted to a flat contract :/
(module sugar racket/base
  (require (only-in sugar/list frequency-hash))
  (define frequency-list
    (compose1 hash-values frequency-hash))
  (provide frequency-list))

(require/typed (submod "." sugar)
  (frequency-list (-> (Listof Any) (Listof Natural))))

(: q2 (-> Natural))
(define (q2)
  (define deer-funcs (list dasher dancer prancer vixen comet
                           cupid donner blitzen rudolph))
  (define winners
    (frequency-list
     (flatten
      (for/list : (Listof Any) ([sec (in-range 1 (add1 2503))])
                (define deer-results
                  (map (λ([deer-func : Deer-Func]) (deer-func sec)) deer-funcs))
                (define max-result (apply max deer-results))
                (map (λ([deer-result : Natural] [deer-func : Deer-Func])
                       (if (= deer-result max-result)
                           deer-func
                           empty))
                     deer-results deer-funcs)))))
  (apply max winners))

(module+ test
  (define input-str (file->string "../day14-input.txt"))
  (check-equal? (q1) 2640)
  (check-equal? (q2) 1102))

