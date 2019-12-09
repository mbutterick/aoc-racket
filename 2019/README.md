## MB’s Advent of Code tips

* The problems are often designed around a particular computer-y abstraction. If you notice what the abstraction is, and then find the closest analog in Racket, the solution tends to come together quickly. Otherwise, you can spend a lot of time reinventing the wheel.

* Complex numbers are a nice way of modeling two-dimensional positions.

* Use lists whenever feasible, because there are many useful list functions in the Racket library that don’t have vector equivalents. In particular, [these list functions](https://docs.racket-lang.org/reference/pairs.html?q=racket%2Flist#%28part._.Additional_.List_.Functions_and_.Synonyms%29) are very useful, especially `argmin` and `argmax`.

* Vectors are better than lists in situations where you need random access to members.

* `eq?` is the fastest equality check, but it only works for symbols and fixnums (therefore, use more symbols and fixnums so you can use `eq?`!)

* `match` is fantastic.

* Association lists (= lists of pairs) are underrated. They’re compatible with all the usual list functions, of course, but also dictionary forms (like `dict-ref` and `in-dict`).

* The `graph` library can be helpful for graph-based problems.

* It’s good to know about sets and mutable pairs.

* Also the fancier `for` iterators, like `for/first` and `for/or`.

* `let/ec` is a way of jumping out of a deeply nested computation, akin to how `return` works in other languages. 



## My solutions

* I try to write solutions that are succinct but not cryptic. 

* I don’t optimize for speed.

* I like doing the Advent of Code problems because it forces me to use parts of Racket that I don’t ordinarily use. So I treat it as a chance to expand my awareness of the Racketverse.

* I’m unlikely to finish every problem. Judging by past years, there is a point where the problems get sufficiently complex that I’d rather put that time into improving my other Racket projects :metal:
