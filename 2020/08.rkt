#lang br
(require racket/file rackunit racket/sequence racket/set)

(define toks (for/list ([tok (in-port read (open-input-file "08.rktd"))])
               tok))

(struct $inst (name val))
(define instructions (for/vector ([slice (in-slice 2 toks)])
                       (apply $inst slice)))

(define (run insts)
  (let loop ([acc 0][ptr 0][visited null])
    (cond
      [(eq? ptr (vector-length insts)) (cons 'terminated acc)]
      [(memq ptr visited) acc]
      [else
       (define next-visited (cons ptr visited))
       (match (vector-ref insts ptr)
         [($inst 'acc val) (loop (+ acc val) (add1 ptr) next-visited)]
         [($inst 'jmp val) (loop acc (+ ptr val) next-visited)]
         [($inst 'nop val) (loop acc (add1 ptr) next-visited)])])))

(check-equal? (run instructions) 1818)

(define (fix-bug insts)
  (for/or ([(inst idx) (in-indexed insts)]
           #:when (memq ($inst-name inst) '(jmp nop)))
    (define new-insts (vector-copy insts))
    (vector-set! insts idx ($inst (if (eq? ($inst-name inst) 'jmp) 'nop 'jmp) ($inst-val inst)))
    (match (run new-insts)
      [(cons 'terminated acc) acc]
      [_ #false])))

(check-equal? (fix-bug instructions) 187)