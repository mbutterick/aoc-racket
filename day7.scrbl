#lang scribble/lp2
@(require scribble/manual aoc-racket/helper)

@aoc-title[7]

Our @link-rp["day7-input.txt"]{input} describes an electrical circuit, with each line of the file describing the signal provided to a particular wire.

@chunk[<day7>
       <day7-setup>
       <day7-ops>
       <day7-q1>
       <day7-q2>
       <day7-test>]

@section{What is the signal on wire @tt{a}?}

The first question we should ask is — how do we model a wire? We're told that it's a thing with inputs that can be evaluated to get a value. So it sounds a lot like a function. Thus, what we'll do is convert our wire descriptions into functions, and then run the function called @racket[a].

In other languages, creating functions from text strings would be a difficult trick. But this facility is built into Racket with @racket[define-syntax]. Essentially our program will run in two phases: in the syntax-transformation phase, we'll read in the list of wire descriptions and expand them into code that represents functions. In the second phase, the program — including our new functions, created via syntax transformation — will compile & run as usual.

The @racket[convert-input-to-wire-functions] syntax transformer takes the input strings and converts them into syntax that looks more like Racket code, so that a line like

@racket["bn RSHIFT 2 -> bo"]

becomes

@racket[(wire bn RSHIFT 2 -> bo)]

Then the @racket[wire] transformer moves the arguments around to define functions, by matching the three definition patterns that appear in the input. Thus, syntax like

@racket[(wire bn RSHIFT 2 -> bo)]

becomes

@racket[(define (bo) (RSHIFT (evaluate-arg bn) (evaluate-arg 2)))]

@racket[evaluate-arg] lets us handle the fact that some of the arguments for our wires are other wires, and some arguments are numbers. Rather than detect these differences during the syntax-transformation phase, we'll just wrap every input argument with @racket[evaluate-arg], which will do the right thing in the next phase.

(@racket[wire-value-cache] is just a performance enhancement, so that wire values don't have to be computed multiple times.)

One gotcha when using syntax transformers is that newly defined identifiers can mask others. For instance, one of the wires in our input is named @tt{if}. When our syntax transformer defines the @tt{if} function, it will override the usual meaning of @racket[if]. There are plenty of elegant ways to handle these kind of namespace collisions. But because this is a puzzle, we'll take the cheap way out: we won't use @racket[if] elsewhere in our code, and instead use @racket[cond].

@chunk[<day7-setup>
       (require racket rackunit
                (for-syntax racket/base racket/file racket/string))
       
       (define-syntax (convert-input-to-wire-functions stx)
         (syntax-case stx ()
           [(_)
            (let* ([input-strings (file->lines "day7-input.txt")]
                   [wire-strings (map (λ(str) (format "(wire ~a)" str)) input-strings)]
                   [wire-datums (map (compose1 read open-input-string) wire-strings)])
              (datum->syntax stx `(begin ,@wire-datums)))]))
       
       (define-syntax (wire stx)
         (syntax-case stx (->)
           [(_ arg -> wire-name)
            #'(define (wire-name) (evaluate-arg arg))]
           [(_ 16bit-op arg -> wire-name)
            #'(define (wire-name) (16bit-op (evaluate-arg arg)))]
           [(_ arg1 16bit-op arg2 -> wire-name)
            #'(define (wire-name) (16bit-op (evaluate-arg arg1) (evaluate-arg arg2)))]
           [(_ expr) #'(begin expr)]
           [else #'(void)]))
       
       (convert-input-to-wire-functions)
       
       (define wire-value-cache (make-hash))
       
       (define (evaluate-arg x)
         (cond
           [(procedure? x) (hash-ref! wire-value-cache x (thunk* (x)))]
           [else x]))
                     
       ]

We also need to implement our 16-bit math operations. As we saw above, our syntax transformers are generating code that looks like, for instance, @racket[(RSHIFT (evaluate-arg bn) (evaluate-arg 2))]. This code won't work unless we've defined an @racket[RSHIFT] function too.

These next definitions use @racket[define-syntax-rule] as a shortcut, which is another syntax transformer.


@chunk[<day7-ops>
       (define (16bitize x)
         (define 16bit-max (expt 2 16))
         (define r (modulo x 16bit-max))
         (cond
           [(negative? r) (16bitize (+ 16bit-max r))]
           [else r]))
       
       (define-syntax-rule (define-16bit id proc)
         (define id (compose1 16bitize proc)))
       (define-16bit AND bitwise-and)
       (define-16bit OR bitwise-ior)
       (define-16bit LSHIFT arithmetic-shift)
       (define-16bit RSHIFT (λ(x y) (arithmetic-shift x (- y))))
       (define-16bit NOT bitwise-not)]


After that, we just evaluate wire function @racket[a] to get our answer.

@chunk[<day7-q1>
       (define (q1) (a))]



@section{What is the signal on wire @tt{a} if wire @tt{b} is overridden with @tt{a}'s original value?}

Having done the heavy lifting, this is easy. We'll redefine wire function @racket[b] to produce the new value, and then check the value of @racket[a] again.

Ordinarily, as a safety measure, Racket won't let you redefine functions. But we can circumvent this limitation by setting @racket[compile-enforce-module-constants] to @racket[#f]. We'll also need to reset our cache, since this change will affect the other wires too.



@chunk[<day7-q2>
       (compile-enforce-module-constants #f)
       
       (define (q2)
         (define first-a-val (a))
         (set! b (thunk* first-a-val))
         (set! wire-value-cache (make-hash))
         (a))
       ]


@section{Testing our input}

@chunk[<day7-test>
       (module+ test
         (define input-strs (file->lines "day7-input.txt"))
         (check-equal? (q1) 46065)
         (check-equal? (q2) 14134))]


