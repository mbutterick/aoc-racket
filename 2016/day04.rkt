#lang br/quicklang
(module+ reader
  (provide read-syntax)
  (define (read-syntax path port)
    (strip-bindings
     #`(module mod "day04.rkt"
         #,@(for*/list ([room-str (in-lines port)]
                        #:when (not (equal? "" room-str)))
              `(room ,@(cdr (regexp-match #px"^(.*)-(\\d+)\\[(\\w+)\\]$" room-str))))))))

#|
Each room consists of an encrypted name (lowercase letters separated by dashes)
followed by a dash, a sector ID, and a checksum in square brackets.
|#
(struct $room (name sector checksum) #:transparent)
(define-macro (room NAME SECTOR CHECKSUM)
  #'($room NAME (string->number SECTOR) CHECKSUM))
(provide room)

(define-macro (mb . ROOMS)
  #'(#%module-begin
     (define rooms (list . ROOMS))
     (display "part a: ")
     (displayln (for/sum ([room (in-list rooms)]
                          #:when (real-room? room))
                  ($room-sector room)))
     (display "part b: ")
     (displayln
      (for/first ([room (in-list rooms)]
                  #:when (equal? (shift-string ($room-name room) ($room-sector room))
                                 "northpole object storage"))
        ($room-sector room)))))
(provide (rename-out [mb #%module-begin]))

#|
A room is real (not a decoy) if the checksum is
the five most common letters in the encrypted name, in order,
with ties broken by alphabetization.
|#

(require sugar/list)
(define (real-room? room)
  (define room-chars (string->list (string-replace ($room-name room) "-" "")))
  (define freqs (hash->list (frequency-hash room-chars)))
  (define sorted-freqs (sort (sort freqs char<? #:key car) > #:key cdr))
  (equal? ($room-checksum room) (list->string (map car (take sorted-freqs 5)))))

(define (shift-string str shift)
  (list->string
   (for/list ([c (in-string str)])
     (cond
       [(char=? c #\-) #\space]
       [else
        (define a-val (char->integer #\a))
        (integer->char (+ (modulo (+ (char->integer c) (- a-val) shift) 26) a-val))]))))