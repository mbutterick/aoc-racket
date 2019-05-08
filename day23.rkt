#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[23]

@defmodule[aoc-racket/day23]

@link["http://adventofcode.com/day/23"]{The puzzle}. Our @link-rp["day23-input.txt"]{input} is a list of instructions representing a program for a two-register virtual machine.


@chunk[<day23>
       <day23-setup>
       <day23-q1>
       <day23-q2>
       <day23-test>]

@isection{What's the value in register @tt{b} after the program runs?}

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

Because we have to repeatedly update the values of the register, it'll be more convenient to use a hash table. Overall, the solution follows the general pattern of @secref{Day_7}.

Notice also that we're encasing the lines of the VM program in @iracket[thunk*]. This creates a function wrapper around each instruction so that its evaluation is delayed until we explicitly ask for it. The @racket[inst] transformer turns the lines of the program into equivalent operations on our register hash table. All these functions return a number that indicates the offset of the next instruction (if it's not a jump instruction, then this value is just 1).

@chunk[<day23-setup>
       (require racket rackunit
                (for-syntax racket/file racket/string sugar/debug))
       (provide (all-defined-out))
       
       (define-syntax (convert-input-to-instruction-functions stx)
         (syntax-case stx ()
           [(_)
            (let* ([input-strings (file->lines "day23-input.txt")]
                   [inst-strings (map (λ (str) (format "(thunk* (inst ~a))" (string-replace str "," ""))) input-strings)]
                   [inst-datums (map (compose1 read open-input-string) inst-strings)])
              (datum->syntax stx `(define instructions (list ,@inst-datums))))]))
       
       (define registers (make-hash '((a . 0)(b . 0))))
       (define default-offset 1)
       
       (define-syntax-rule (define-reg-updater id thunk)
         (define (id reg)
           (hash-update! registers reg thunk)
           default-offset))
       
       (define-reg-updater tpl (λ (val) (* 3 val)))
       (define-reg-updater inc (λ (val) (add1 val)))
       (define-reg-updater hlf (λ (val) (/ val 2)))
       
       (define (jmpf reg num pred)
         (if (pred (hash-ref registers reg)) num 1))
       
       (define-syntax (inst stx)
         (syntax-case stx (jmp jio jie)
           [(_ jio reg num)
            #'(jmpf 'reg num (curry = 1))]
           [(_ jie reg num)
            #'(jmpf 'reg num even?)]
           [(_ jmp num)
            #'num]
           [(_ op reg)
            #'(op 'reg)]))
       
       (convert-input-to-instruction-functions)
       
       ]

With the instructions set up as functions, running the program is easy. We start at index 0 and evaluate the first instruction in our list (which will possibly update one of the registers). The result is the offset for the next instruction. We continue until our index exceeds the number of instructions available.


@chunk[<day23-q1>
       
       (define (q1)
         (let eval-instruction ([idx 0])
           (if (>= idx (length instructions))
               (hash-ref registers 'b)
               (let* ([inst (list-ref instructions idx)]
                      [jump-offset (inst)]
                      [next-idx (+ jump-offset idx)])
                 (eval-instruction next-idx)))))]



@section{What's the value in register @tt{b} if register @tt{a} starts as 1?}

Same as the first question, except we'll reset the registers before running the program.


@chunk[<day23-q2>
       
       (define (q2)
         (hash-set*! registers 'a 1 'b 0)
         (q1))
              
       ]



@section{Testing Day 23}

@chunk[<day23-test>
       (module+ test
         (check-equal? (q1) 184)
         (check-equal? (q2) 231))]


