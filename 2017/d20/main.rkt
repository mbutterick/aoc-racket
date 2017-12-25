#lang br/quicklang
(require "../helper.rkt")
(provide read-syntax (rename-out [#%mb #%module-begin]) ★ ★★)

(define (read-syntax path port)
  (define str (let* ([str  (port->string port)]
                     [str (string-replace str "," " ")]
                     [str (string-replace str "<" "(")]
                     [str (string-replace str ">" ")")])
                str))
  (strip-context #`(module mod "main.rkt"
                     #,@(for/list ([line (in-lines (open-input-string str))])
                          (for/list ([datum (in-port read (open-input-string line))])
                            datum)))))

(define-macro (#%mb (STARS) (p= PNUMS v= VNUMS a= ANUMS) ...)
  #`(#%module-begin
     (time (STARS (particle 'PNUMS 'VNUMS 'ANUMS) ...))))

(struct particle (pos vel acc) #:transparent)

(define (do-tick p)
  (define next-vel (map + (particle-vel p) (particle-acc p)))
  (define next-pos (map + (particle-pos p) next-vel))
  (particle next-pos next-vel (particle-acc p)))

(define (dist p) (apply + (map abs (particle-pos p))))

(define (remove-collisions particles)
  (define (find-duplicate-particle ps) (check-duplicates ps #:key particle-pos))
  (for/fold ([ps particles]
              [dup (find-duplicate-particle particles)]
              #:result ps)
             ([i (in-naturals)]
              #:break (not dup))
    (values (filter-not (λ (p) (andmap = (particle-pos dup) (particle-pos p))) ps)
            (find-duplicate-particle ps))))

(define (closest particles #:collisions [collisions? #f])
  (for/fold ([particles particles]
             #:result (if collisions?
                          (length particles)
                          (argmin cdr (for/list ([(p pidx) (in-indexed particles)])
                                        (cons pidx (dist p))))))
            ([i (in-range 1000)])
    ((if collisions? remove-collisions values) (map do-tick particles))))

(define (★ . particles) (closest particles))

(define (★★ . particles) (closest particles #:collisions #t))
