#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[23]

@defmodule[aoc-racket/day23]

@link["http://adventofcode.com/day/21"]{The puzzle}. Our @link-rp["day23-input.txt"]{input} is a list of instructions representing a program for a two-register virtual machine.


@chunk[<day23>
       <day23-setup>
       <day23-q1>
       <day23-q2>
       <day23-test>]

@section{What's the value in register @tt{b} after the program runs?}

The virtual machine has two registers, @tt{a} and @tt{b}, that both start at 0. It also has six instructions:

@itemlist[
          
 @item{@tt{hlf r} sets register r to half its current value, then continues.}
 @item{@tt{tpl r} sets register r to triple its current value, then continues.}
 @item{@tt{inc r} adds 1 to register r, then continues.}
 @item{@tt{jmp offset} jumps the instruction that is @tt{offset} steps away. An @tt{offset} can be positive or negative.}
 @item{@tt{jie r, offset} is like @tt{jmp}, but only jumps if r is even.}
 @item{@tt{jio r, offset} jumps by @tt{offset} if register r = 1.}
 ]

Although the virtual machine has the equivalent of functions & variables, the jump instructions add a complication. We can't just evaluate the instructions top to bottom. We have to maintain a list of all the instructions, and a pointer to where we are, so if we get a jump instruction, we can move to the right place.

Because we have to repeatedly update the values of the register, it'll be more convenient to use a hash table (which is designed for this purpose).

@chunk[<day23-setup>
       (require racket rackunit)
       (provide (all-defined-out))
       
       (define registers (make-hash '((a . 0)(b . 0))))
       
       (define-syntax-rule (define-reg-updater id thunk)
         (define (id reg)
           (hash-update! registers reg thunk)))
       
       (define-reg-updater tpl (λ(val) (* 3 val)))
       (define-reg-updater inc (λ(val) (add1 val)))
       (define-reg-updater hlf (λ(val) (/ val 2)))
       
       (define (jmpf reg num pred)
         (if (pred (hash-ref registers reg)) num 1))
       
       (define-syntax (ins stx)
         (syntax-case stx (jmp jio jie)
           [(_ jio reg num)
            #'(jmpf 'reg num (λ(x) (= 1 x)))]
           [(_ jie reg num)
            #'(jmpf 'reg num even?)]
           [(_ jmp num)
            #'num]
           [(_ op reg)
            #'(op 'reg)]
           [else #'(void)]))

         (define-syntax (parse-instructions stx)
           (syntax-case stx ()
             [(_ input-str)
              #'(let* ([str (string-replace input-str "," "")]
                       [lines (string-split str "\n")]
                       [datums)
                  42)]))
                            
       ]

@chunk[<day23-q1>
       
       (define (q1 input-str)
         (define instructions (parse-instructions input-str))
         (let eval-instruction ([idx 0])
           (if (>= idx (length instructions))
               (hash-ref registers 'b)
               (let* ([inst (list-ref instructions idx)]
                      [result (inst)]
                      [next-idx (+ (if (number? result)
                                       result
                                       1) idx)])
                 (eval-instruction next-idx))))
         184)]



@section{What's the value in register @tt{b} if register @tt{a} starts as 1?}




@chunk[<day23-q2>
       
       (define (q2 input-str)
         231)
             
       ]



@section{Testing Day 23}

@chunk[<day23-test>
       (module+ test
         (define input-str (file->string "day23-input.txt"))
         (check-equal? (q1 input-str) 184)
         (check-equal? (q2 input-str) 231))]


